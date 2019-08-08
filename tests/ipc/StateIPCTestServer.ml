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

(* This file implements a state server that mimics the behaviour of the
 * actual blockchain state server. It begins with an empty state and hence
 * must be "updated" to have the required state by calling update_state_value
 * before the contract is executed. *)

open Core
open Result.Let_syntax
open MonadUtil
open StateIPCIdl
open OUnit2

module IPCTestServer = IPCIdl(Idl.GenServer ())

type hashtable =  (string, value_type) Hashtbl_intf.Hashtbl.t

and value_type =
  | NonMapVal of string
  | MapVal of hashtable

let num_pending_requests = 5
let fetch_message = "Fetching state value failed"
let update_message = "Updating state value failed"

(* Global state of the server. *)
let table = Hashtbl.create (module String)
let threadref = ref None

let fail a = Error a

(* Send msg with delimiting character "0xA". *)
let send_delimited oc msg =
  let msg' = msg ^ "\n" in
  Caml.output_string oc msg';
  Caml.flush oc

let binary_rpc conn =
  let ic = Unix.in_channel_of_descr conn in
  let oc = Unix.out_channel_of_descr conn in 
  let request = Jsonrpc.call_of_string (Caml.input_line ic) in
  let response = (Idl.server IPCTestServer.implementation) request in
  send_delimited oc (Jsonrpc.string_of_response response)

let prepare_server sock_addr =
  (try Unix.unlink sock_addr with Unix.Unix_error(Unix.ENOENT, _, _) -> ());
  let socket = Unix.socket ~domain: Unix.PF_UNIX ~kind: Unix.SOCK_STREAM ~protocol: 0 in
  Unix.bind socket ~addr:(Unix.ADDR_UNIX sock_addr);
  Unix.listen socket ~backlog:num_pending_requests;
  Printf.fprintf stdout "ScillaIPCTestServer: listening on %s\n" sock_addr;
  let server () =
    while true do
      let conn, _ = Unix.accept socket in
      try
        match !threadref with
        | Some _ ->
          (* Sleep for 25 milliseconds, similar to the jsonrpccpp server. *)
          let _ = Thread.delay(0.025) in
          binary_rpc conn
        | None ->
          Unix.shutdown conn ~mode:Unix.SHUTDOWN_RECEIVE;
          Unix.unlink sock_addr;
          Thread.exit () (* No active server thead. *)
      with
      | _ ->
          Unix.shutdown conn ~mode:Unix.SHUTDOWN_RECEIVE;
          Unix.unlink sock_addr;
          assert_failure "IPC Server raised exception."
    done
  in
  server

let decode_serialized_value value =
  let decoder = Pbrt.Decoder.of_bytes (Bytes.of_string value) in
  Ipcmessage_pb.decode_proto_scilla_val decoder

let encode_serialized_value value =
  let encoder = Pbrt.Encoder.create () in
  Ipcmessage_pb.encode_proto_scilla_val value encoder;
  Bytes.to_string @@ Pbrt.Encoder.to_bytes encoder

let decode_serialized_query query =
  let decoder = Pbrt.Decoder.of_bytes (Bytes.of_string query) in
  Ipcmessage_pb.decode_proto_scilla_query decoder

let rec serialize_value value =
  match value with
  | NonMapVal v -> Ipcmessage_types.Bval (Bytes.of_string v)
  | MapVal m -> 
    let map_list = Hashtbl.to_alist m in
    let serialized_map_list = List.map map_list ~f: (fun (str, v) -> (str, serialize_value v)) in
    Ipcmessage_types.Mval({ m = serialized_map_list})

let rec deserialize_value value =
  match value with 
  | Ipcmessage_types.Bval v -> NonMapVal (Bytes.to_string v)
  | Ipcmessage_types.Mval m_list ->
    let new_table = Hashtbl.create (module String) in
    List.iter m_list.m ~f: (fun (str, v) -> 
      Hashtbl.set new_table ~key:str ~data:(deserialize_value v));
    MapVal new_table

(* Not sure if need to check mapdepth vs length of indices? *)
let fetch_state_value query =
  let rec recurser value indices =
    match indices with
    | [] -> pure (Some value)
    | head :: tail ->
      match value with
      | NonMapVal _ -> fail RPCError.({ code = 0; message = fetch_message})
      | MapVal m ->
        let vopt = Hashtbl.find m head in
        match vopt with
        | Some v -> recurser v tail
        | None -> pure None
  in
  let query = decode_serialized_query query in
  match query with
  | { name; indices; ignoreval; _ } ->
    let string_indices_list = name :: (List.map indices ~f: Bytes.to_string) in
    let%bind vopt = recurser (MapVal table) string_indices_list in
    match vopt with
    | Some v ->
      if ignoreval then pure @@ (true, "") else
      pure @@ (true, encode_serialized_value (serialize_value v))
    | None -> pure @@ (false, "")

(* Not sure if need to check mapdepth vs length of indices? *)
let update_state_value query value =
  let rec recurser_update ?(new_val = None) map indices =
    match indices with
    | [] -> fail RPCError.({ code = 0; message = update_message})
    | [index] ->
      pure @@ (match new_val with
      | None -> Hashtbl.remove map index
      | Some v -> Hashtbl.set map ~key:index ~data:v)
    | head :: tail ->
      let vopt = Hashtbl.find map head in
      match vopt with
      | None -> fail RPCError.({ code = 0; message = update_message})
      | Some v -> 
        match v with
        | NonMapVal _ -> fail RPCError.({ code = 0; message = update_message})
        | MapVal m -> recurser_update ~new_val m tail
  in
  let query = decode_serialized_query query in
  match query with
  | { name; indices; ignoreval; _ } ->
    let string_indices_list = List.map indices ~f: Bytes.to_string in
    match ignoreval with
    | true -> recurser_update table (name::string_indices_list)
    | false -> 
      let new_val = deserialize_value (decode_serialized_value value) in
      recurser_update ~new_val:(Some new_val) table (name::string_indices_list)

let start_server ~sock_addr =
  IPCTestServer.fetch_state_value fetch_state_value;
  IPCTestServer.update_state_value update_state_value;
  match !threadref with
  | Some _ -> assert_failure "Server already running, cannot restart"
  | None ->
    let server = prepare_server sock_addr in
    let t = Thread.create server () in
    threadref := Some t

let stop_server () =
  match !threadref with
  | Some _ -> threadref := None
  | None ->
    assert_failure "No server running, cannot stop."
