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
open Core
open Result.Let_syntax
open MonadUtil
open Syntax
open Stdint

module IPCClientIdl(R: RPC) = struct
  open R
  let query = Param.mk ~name: "query" Rpc.Types.string
  let value = Param.mk ~name: "value" Rpc.Types.string
  let return_string = Param.mk Rpc.Types.string
  let boolean = Param.mk Rpc.Types.bool
  let void = Param.mk Rpc.Types.unit
  (* TODO Change error to something other than default error *)
  let error = Idl.DefaultError.err
  let fetch_state_value = declare "fetchStateValue" ["Fetch state value from blockchain"] (query @-> returning return_string error)
  let update_state_value = declare "updateStateValue" ["Update state value in blockchain"] (query @-> value @-> returning boolean error)
  
  let test_server_rpc = declare "testServerRPC" ["Check if client server interaction is working"] (query @-> returning return_string error)
end

module IPCClient = IPCClientIdl(Idl.GenClient ())

(* Send msg via socket s with a delimiting character "0xA". *)
let send_delimited oc msg =
  let msg' = msg ^ "\n" in
  Stdlib.output_string oc msg';
  Core.Out_channel.flush oc

let binary_rpc ~socket_address (call: Rpc.call) : Rpc.response =
  let sockaddr = Unix.ADDR_UNIX socket_address in
  let (ic, oc) = Unix.open_connection sockaddr in
  let msg_buf = Jsonrpc.string_of_call ~version: Jsonrpc.V2 call in
  (* Send data to the socket. *)
  let _ = send_delimited oc msg_buf in
  (* Get response. *)
  let response = Stdlib.input_line ic in
  Printf.printf "Response: %s\n" response;
  Jsonrpc.response_of_string response

let serialize_literal literal =
  PrettyPrinters.literal_to_jstring literal

let rec serialize_value value =
  match value with
  | Map m -> ScillaMessageTypes.Mval 
  | _ -> ScillaMessageTypes.Bval (serialize_literal value)
  
let construct_and_serialize_query ~fname ~keys ~is_delete ~is_map =
  let map_depth = 
    match is_map with
    | true -> List.length keys
    | false -> 0
  in
  let query = ScillaMessageTypes.({ 
    name = fname; 
    mapdepth = map_depth; 
    indices = List.map keys serialize_literal; 
    deletemapkey = is_delete 
  }) in
  let encoder = Pbrt.Encoder.create () in 
  ScillaMessage_pb.encode_proto_scilla_query query encoder;
  Pbrt.Encoder.to_bytes encoder
  (* Bytes.to_string query_bytes *)

let construct_and_serialize_value ~value = function
  | true -> "" (*The value is irrelevant for a delete operation *)
  | false ->
    let scilla_val = serialize_value value in
    let encoder = Pbrt.Encoder.create () in
    ScillaMessage_pb.encode_proto_scilla_val scilla_val encoder;
    Pbrt.Encoder.to_bytes encoder
    (* Bytes.to_string value_bytes *)
    
let update ~socket_address ~fname ~keys ~value ~is_map =
  let is_delete =
    match value with
    | Some _ -> true
    | None -> false
  in
  let query = construct_and_serialize_query ~fname ~keys ~is_delete ~is_map in
  let%bind serialized_value = construct_and_serialize_value ~value is_delete in
  let _ = IPCClient.update_state_value binary_rpc ~socket_address query serialized_value in
  let gas_and_value = add_gas ~value:serialized_value ~fname ~keys ~is_map in
  pure @@ gas_and_value


let fetch ~socket_address ~fname ~keys ~is_map =
  let query = construct_and_serialize_query ~fname ~keys ~is_delete: false ~is_map in
  let%bind return_string = IPCClient.fetch_state_value binary_rpc ~socket_address query in
  let%bind value = deserialize_value return_string in
  let gas_and_value = add_gas ~value ~fname ~keys ~is_map in
  pure @@ gas_and_value

let test_server_rpc ~socket_address ~query = 
  IPCClient.test_server_rpc (binary_rpc ~socket_address) query
