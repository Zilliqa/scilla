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
open Result.Let_syntax
open MonadUtil
open Syntax
open JSON
open ParserUtil
open TypeUtil
open StateIPCIdl

module ER = ParserRep

module IPCClient = IPCIdl(Idl.GenClient ())

(* Translate JRPC result to our result. *)
let translate_res res =
  match res with
  | Error (e : RPCError.err_t) ->
    fail0 (Printf.sprintf "Error in IPC access: (code:%d, message:%s)." e.code e.message)
  | Ok res' -> pure res'

(* Send msg via socket s with a delimiting character "0xA". *)
let send_delimited oc msg =
  let msg' = msg ^ "\n" in
  Caml.output_string oc msg';
  Caml.flush oc

let binary_rpc ~socket_addr (call: Rpc.call) : Rpc.response =
  let sockaddr = Unix.ADDR_UNIX socket_addr in
  let (ic, oc) = Unix.open_connection sockaddr in
  let msg_buf = Jsonrpc.string_of_call ~version: Jsonrpc.V2 call in
  Printf.printf "Sending: %s\n" msg_buf;
  (* Send data to the socket. *)
  let _ = send_delimited oc msg_buf in
  (* Get response. *)
  let response = Caml.input_line ic in
  Printf.printf "Response: %s\n" response;
  Jsonrpc.response_of_string response

(* Encode a literal into bytes, opaque to the backend storage. *)
let serialize_literal l = Bytes.of_string (PrettyPrinters.literal_to_jstring l)

(* Map fields are serialized into ScillaMessageTypes.MVal
   Other fields are serialized using serialize_literal into bytes/string. *)
let rec serialize_field value =
  match value with
  | Map (_, mlit) ->
    let mpb = Caml.Hashtbl.fold (fun key value acc ->
      let key' = Bytes.to_string (serialize_literal key) in
      (* values can be Maps or non-map literals. Hence a recursive call. *)
      let val' = serialize_field value in
      (key', val') :: acc
    ) mlit [] in
    ScillaMessageTypes.Mval({ ScillaMessageTypes.m = mpb })
    (* If there are maps _inside_ a non-map field, they are treated same
     * as non-Map field values are not serialized as protobuf maps. *)
  | _ -> ScillaMessageTypes.Bval (serialize_literal value)

(* Deserialize proto_scilla_val, given its type. *)
let rec deserialize_value value tp =
  match value with
  | ScillaMessageTypes.Bval s ->
    (match tp with
    | MapType _ -> fail0 "Type mismatch deserializing value. Did not expect MapType."
    | _ -> pure (ContractState.jstring_to_literal (Bytes.to_string s)))
  | ScillaMessageTypes.Mval m ->
    (match tp with
    | MapType (kt, vt) ->
      let mlit = Caml.Hashtbl.create (List.length m.m) in
      let _ = iterM m.m ~f:(fun (k, v) ->
        let k' = ContractState.jstring_to_literal k in
        let%bind v' = deserialize_value v vt in
        Caml.Hashtbl.add mlit k' v';
        pure ()
      ) in
      pure (Map ((kt, vt), mlit))
    | _ -> fail0 "Type mismatch deserializing value. Expected MapType.")

let encode_serialized_value value =
  let encoder = Pbrt.Encoder.create () in
  ScillaMessage_pb.encode_proto_scilla_val value encoder;
  Bytes.to_string @@ Pbrt.Encoder.to_bytes encoder

let decode_serialized_value value =
  let decoder = Pbrt.Decoder.of_bytes value in
  ScillaMessage_pb.decode_proto_scilla_val decoder

let encode_serialized_query query =
  let encoder = Pbrt.Encoder.create () in
  ScillaMessage_pb.encode_proto_scilla_query query encoder;
  Bytes.to_string @@ Pbrt.Encoder.to_bytes encoder

(* Fetch a field value. keys is empty iff this value being fetched is not a whole map itself.
 * If a map key is not found, then None is returned, otherwise (Some value) is returned. *)
let fetch ~socket_addr ~fname ~keys ~tp =
  let open ScillaMessageTypes in
  let q = {
    name = (get_id fname);
    mapdepth = TypeUtilities.map_depth tp;
    indices = List.map keys ~f:(serialize_literal);
    ignoreval = false;
  } in
  let q' = encode_serialized_query q in
  let%bind res = translate_res @@ IPCClient.fetch_state_value (binary_rpc ~socket_addr) q' in
  match res with
  | (true, res') ->
    let%bind res'' = deserialize_value (decode_serialized_value (Bytes.of_string res')) tp in
    pure @@ Some (res'')
  | (false, _) -> pure None

(* Update a field. keys is empty iff the value being updated is not a whole map itself. *)
let update ~socket_addr ~fname ~keys ~value ~tp =
  let open ScillaMessageTypes in
  let q = {
    name = (get_id fname);
    mapdepth = TypeUtilities.map_depth tp;
    indices = List.map keys ~f:(serialize_literal);
    ignoreval = false;
  } in
  let q' = encode_serialized_query q in
  let value' =  encode_serialized_value (serialize_field value) in
  let%bind _ = translate_res @@ IPCClient.update_state_value (binary_rpc ~socket_addr) q' value' in
  pure ()

(* Is a key in a map. keys must be non-empty. *)
let is_member ~socket_addr ~fname ~keys ~tp =
  let open ScillaMessageTypes in
  let q = {
    name = (get_id fname);
    mapdepth = TypeUtilities.map_depth tp;
    indices = List.map keys ~f:(serialize_literal);
    ignoreval = true;
  } in
  let q' = encode_serialized_query q in
  let%bind res = translate_res @@ IPCClient.fetch_state_value (binary_rpc ~socket_addr) q' in
  pure @@ (fst res)

(* Remove a key from a map. keys must be non-empty. *)
let remove ~socket_addr ~fname ~keys ~tp =
  let open ScillaMessageTypes in
  let q = {
    name = (get_id fname);
    mapdepth = TypeUtilities.map_depth tp;
    indices = List.map keys ~f:(serialize_literal);
    ignoreval = true;
  } in
  let q' = encode_serialized_query q in
  let dummy_val = "" in (* This will be ignored by the blockchain. *)
  let%bind _ = translate_res @@ IPCClient.update_state_value (binary_rpc ~socket_addr) q' dummy_val in
  pure ()
