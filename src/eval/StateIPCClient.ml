(*
  This file is part of scilla.

  Copyright (c) 2018 - present Zilliqa Research Pvt. Ltd.

  scilla is free software: you can redistribute it and/or modify it under the
  terms of the GNU General Public License as published by the Free Software
  Foundation, either version 3 of the License, or (at your option) any later
  version.

  scilla is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
  A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along with
  scilla.  If not, see <http://www.gnu.org/licenses/>.
*)
open Core
open! Int.Replace_polymorphic_compare
open Result.Let_syntax
open MonadUtil
open Syntax
open JSON
open TypeUtil
open StateIPCIdl
open ErrorUtils
open IPCUtil
module ER = ParserRep
module M = Idl.IdM
module IDL = Idl.Make (M)

module IPCClient = IPCIdl (IDL.GenClient ())

(* Translate JRPC result to our result. *)
let translate_res res =
  match res |> IDL.T.get |> M.run with
  | Error (e : RPCError.err_t) ->
      fail0
        (Printf.sprintf
           "StateIPCClient: Error in IPC access: (code:%d, message:%s)." e.code
           e.message)
  | Ok res' -> pure res'

let ipcclient_exn_wrapper thunk =
  try thunk () with
  | Unix.Unix_error (_, s1, s2) ->
      fail0 ("StateIPCClient: Unix error: " ^ s1 ^ s2)
  | _ -> fail0 "StateIPCClient: Unexpected error making JSON-RPC call"

let binary_rpc ~socket_addr (call : Rpc.call) : Rpc.response M.t =
  let socket =
    Unix.socket ~domain:Unix.PF_UNIX ~kind:Unix.SOCK_STREAM ~protocol:0
  in
  Unix.connect socket ~addr:(Unix.ADDR_UNIX socket_addr);
  let ic, oc =
    (Unix.in_channel_of_descr socket, Unix.out_channel_of_descr socket)
  in
  let msg_buf = Jsonrpc.string_of_call ~version:Jsonrpc.V2 call in
  DebugMessage.plog (Printf.sprintf "Sending: %s\n" msg_buf);
  (* Send data to the socket. *)
  let _ = send_delimited oc msg_buf in
  (* Get response. *)
  let response = Caml.input_line ic in
  Unix.close socket;
  DebugMessage.plog (Printf.sprintf "Response: %s\n" response);
  M.return @@ Jsonrpc.response_of_string response

(* Encode a literal into bytes, opaque to the backend storage. *)
let serialize_literal l = Bytes.of_string (PrettyPrinters.literal_to_jstring l)

let deserialize_literal s tp =
  try pure @@ ContractState.jstring_to_literal s tp
  with Invalid_json s ->
    fail
      ( s
      @ mk_error0
          "StateIPCClient: Error deserializing literal fetched from IPC call" )

(* Map fields are serialized into Ipcmessage_types.MVal
   Other fields are serialized using serialize_literal into bytes/string. *)
let rec serialize_field value =
  match value with
  | Map (_, mlit) ->
      let mpb =
        Caml.Hashtbl.fold
          (fun key value acc ->
            let key' = Bytes.to_string (serialize_literal key) in
            (* values can be Maps or non-map literals. Hence a recursive call. *)
            let val' = serialize_field value in
            (key', val') :: acc)
          mlit []
      in
      Ipcmessage_types.Mval { Ipcmessage_types.m = mpb }
  (* If there are maps _inside_ a non-map field, they are treated same
   * as non-Map field values are not serialized as protobuf maps. *)
  | _ -> Ipcmessage_types.Bval (serialize_literal value)

(* Deserialize proto_scilla_val, given its type. *)
let rec deserialize_value value tp =
  match value with
  | Ipcmessage_types.Bval s -> deserialize_literal (Bytes.to_string s) tp
  | Ipcmessage_types.Mval m -> (
      match tp with
      | MapType (kt, vt) ->
          let mlit = Caml.Hashtbl.create (List.length m.m) in
          let _ =
            let m =
              List.sort m.m ~compare:(fun (k1, _) (k2, _) ->
                  String.compare k1 k2)
            in
            iterM m ~f:(fun (k, v) ->
                let%bind k' = deserialize_literal k kt in
                let%bind v' = deserialize_value v vt in
                Caml.Hashtbl.add mlit k' v';
                pure ())
          in
          pure (Map ((kt, vt), mlit))
      | _ ->
          fail0
            "StateIPCClient: Type mismatch deserializing value. Unexpected \
             protobuf map." )

let encode_serialized_value value =
  try
    let encoder = Pbrt.Encoder.create () in
    Ipcmessage_pb.encode_proto_scilla_val value encoder;
    pure @@ Bytes.to_string @@ Pbrt.Encoder.to_bytes encoder
  with e -> fail0 (Exn.to_string e)

let decode_serialized_value value =
  try
    let decoder = Pbrt.Decoder.of_bytes value in
    pure @@ Ipcmessage_pb.decode_proto_scilla_val decoder
  with e -> fail0 (Exn.to_string e)

let encode_serialized_query query =
  try
    let encoder = Pbrt.Encoder.create () in
    Ipcmessage_pb.encode_proto_scilla_query query encoder;
    pure @@ Bytes.to_string @@ Pbrt.Encoder.to_bytes encoder
  with e -> fail0 (Exn.to_string e)

(* Fetch from a field. "keys" is empty when fetching non-map fields or an entire Map field.
 * If a map key is not found, then None is returned, otherwise (Some value) is returned. *)
let fetch ~socket_addr ~fname ~keys ~tp =
  let open Ipcmessage_types in
  let q =
    {
      name = get_id fname;
      mapdepth = TypeUtilities.map_depth tp;
      indices = List.map keys ~f:serialize_literal;
      ignoreval = false;
    }
  in
  let%bind q' = encode_serialized_query q in
  let%bind res =
    let thunk () =
      translate_res @@ IPCClient.fetch_state_value (binary_rpc ~socket_addr) q'
    in
    ipcclient_exn_wrapper thunk
  in
  match res with
  | true, res' ->
      let%bind tp' = TypeUtilities.map_access_type tp (List.length keys) in
      let%bind decoded_pb = decode_serialized_value (Bytes.of_string res') in
      let%bind res'' = deserialize_value decoded_pb tp' in
      pure @@ Some res''
  | false, _ -> pure None

(* Update a field. "keys" is empty when updating non-map fields or an entire Map field. *)
let update ~socket_addr ~fname ~keys ~value ~tp =
  let open Ipcmessage_types in
  let q =
    {
      name = get_id fname;
      mapdepth = TypeUtilities.map_depth tp;
      indices = List.map keys ~f:serialize_literal;
      ignoreval = false;
    }
  in
  let%bind q' = encode_serialized_query q in
  let%bind value' = encode_serialized_value (serialize_field value) in
  let%bind _ =
    let thunk () =
      translate_res
      @@ IPCClient.update_state_value (binary_rpc ~socket_addr) q' value'
    in
    ipcclient_exn_wrapper thunk
  in
  pure ()

(* Is a key in a map. keys must be non-empty. *)
let is_member ~socket_addr ~fname ~keys ~tp =
  let open Ipcmessage_types in
  let q =
    {
      name = get_id fname;
      mapdepth = TypeUtilities.map_depth tp;
      indices = List.map keys ~f:serialize_literal;
      ignoreval = true;
    }
  in
  let%bind q' = encode_serialized_query q in
  let%bind res =
    let thunk () =
      translate_res @@ IPCClient.fetch_state_value (binary_rpc ~socket_addr) q'
    in
    ipcclient_exn_wrapper thunk
  in
  pure @@ fst res

(* Remove a key from a map. keys must be non-empty. *)
let remove ~socket_addr ~fname ~keys ~tp =
  let open Ipcmessage_types in
  let q =
    {
      name = get_id fname;
      mapdepth = TypeUtilities.map_depth tp;
      indices = List.map keys ~f:serialize_literal;
      ignoreval = true;
    }
  in
  let%bind q' = encode_serialized_query q in
  let dummy_val = "" in
  (* This will be ignored by the blockchain. *)
  let%bind _ =
    let thunk () =
      translate_res
      @@ IPCClient.update_state_value (binary_rpc ~socket_addr) q' dummy_val
    in
    ipcclient_exn_wrapper thunk
  in
  pure ()
