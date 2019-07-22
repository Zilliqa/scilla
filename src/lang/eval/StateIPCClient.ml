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

open Idl
open Stdlib
open Core
open Result.Let_syntax
open MonadUtil

module IPCClientIdl(R: RPC) = struct
  open R
  let query = Param.mk ~name: "query" Rpc.Types.string
  let value = Param.mk ~name: "value" Rpc.Types.string
  let return_string = Param.mk Rpc.Types.string
  let void = Param.mk Rpc.Types.unit
  (* TODO Change error to something other than default error *)
  let error = Idl.DefaultError.err
  let fetch_state_value = declare "fetchStateValue" ["Fetch state value from blockchain"] (query @-> returning return_string error)
  let update_state_value = declare "updateStateValue" ["Update state value in blockchain"] (query @-> value @-> returning void error)
  
  let test_server_rpc = declare "testServerRPC" ["Check if client server interaction is working"] (query @-> returning return_string error)
end

module IPCClient = IPCClientIdl(Idl.GenClient ())

(* TODO figure buffer sizes out *)
let binary_rpc ~socket_address (call: Rpc.call) : Rpc.response =
  let sockaddr = Unix.ADDR_UNIX socket_address in
  let s = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.connect s sockaddr;
  let ic = Unix.in_channel_of_descr s in
  let oc = Unix.out_channel_of_descr s in
  let msg_buf = Jsonrpc.string_of_call ~version: Jsonrpc.V2 call in
  output_string oc msg_buf;
  flush oc;
  (* let len_buf = Bytes.make 128 '\000' in *)
  (* really_input ic len_buf 0 128; *)
  (* let _ = Printf.printf "%s" (Bytes.unsafe_to_string len_buf) in *)
  (* let len = Int64.of_string (Bytes.unsafe_to_string len_buf) in *)
  (* let len_int = Int64.to_int len in *)
  let msg_buf = Bytes.make 38 '\000' in
  really_input ic msg_buf 0 38;
  let _ = Printf.printf "%s" (Bytes.unsafe_to_string msg_buf) in
  let (response: Rpc.response) = Jsonrpc.response_of_string (Bytes.unsafe_to_string msg_buf) in
  (* let (response: Rpc.response) = Jsonrpc.response_of_string msg_buf in *)
  response

let update ~socket_address ~fname ~keys ~value ~is_map =
  (* let is_delete = 
    match value with
    | Some _ -> true
    | None -> false
  in
  let%bind query = construct_and_serialize_query ~fname ~keys ~is_delete ~is_map in
  let%bind serialized_value = serialize_value ~keys ~value ~is_delete in
  let _ = IPCClient.update_state_value binary_rpc ~socket_address query serialized_value in
  let gas_and_value = add_gas ~value:serialized_value ~fname ~keys ~is_map in
  pure @@ gas_and_value *)
  fail0 "StateService: update not implemented yet for IPC mode" 


let fetch ~socket_address ~fname ~keys ~is_map =
  (* let%bind query = construct_and_serialize_query ~fname ~keys ~is_delete: false ~is_map in
  let%bind return_string = IPCClient.fetch_state_value binary_rpc ~socket_address query in
  let%bind value = deserialize return_string in
  let gas_and_value = add_gas ~value ~fname ~keys ~is_map in
  pure @@ gas_and_value *)
  fail0 "StateService: fetch not implemented yet for IPC mode"

let test_server_rpc ~socket_address ~query = 
  IPCClient.test_server_rpc (binary_rpc ~socket_address) query

(* let construct_and_serialize_query ~fname ~keys ~is_delete ~is_map =
  let map_depth = 
    match is_map with
    | true -> List.length keys
    | false -> 0
  in
  let query = ScillaMessageTypes.({ 
    name = fname; 
    mapdepth = map_depth; 
    indices = keys; 
    deletemapkey = is_delete 
  }) in
  let encoder = Pbrt.Encoder.create () in 
  encoder.encode_proto_scilla_query query encoder; *)



(* TO IMPLEMENT
1. construct_and_serialize_query
2. deserialize
3. add_gas
4. serialize_value
*)