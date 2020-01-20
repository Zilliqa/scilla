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
open Syntax
open TypeUtil
open StateIPCIdl
open OUnit2
module M = Idl.IdM
module IDL = Idl.Make (M)

module IPCClient = IPCIdl (IDL.GenClient ())

(* The purpose of this Test Client is to initialize the test server with data during testing.
 * While ideally we would've liked to reuse StateService, we cannot do so because that deals
 * with Scilla literals, which we cannot always parse from the JSONs. Parsing it requires the 
 * definitions of custom ADTs that are only available in the source file. *)

type state_info = (string * typ) list

(* For persistance b/w multiple queries. *)
let stateref = ref None

(* Sets up state_info. Should be called before any queries. *)
let initialize ~sock_addr ~(fields : state_info) =
  stateref := Some (sock_addr, fields)

let assert_init () =
  match !stateref with
  | None -> assert_failure "StateIPCTestClient: Uninitialized"
  | Some (sock_addr, fields) -> (sock_addr, fields)

let encode_serialized_value value =
  let encoder = Pbrt.Encoder.create () in
  Ipcmessage_pb.encode_proto_scilla_val value encoder;
  Bytes.to_string @@ Pbrt.Encoder.to_bytes encoder

let decode_serialized_value value =
  let decoder = Pbrt.Decoder.of_bytes value in
  Ipcmessage_pb.decode_proto_scilla_val decoder

let encode_serialized_query query =
  let encoder = Pbrt.Encoder.create () in
  Ipcmessage_pb.encode_proto_scilla_query query encoder;
  Bytes.to_string @@ Pbrt.Encoder.to_bytes encoder

(* Translate JRPC result to our result. *)
let translate_res res =
  match res |> IDL.T.get |> M.run with
  | Error (e : RPCError.err_t) ->
      assert_failure
        (Printf.sprintf
           "StateIPCTestClient: Error in IPC access: (code:%d, message:%s)."
           e.code e.message)
  | Ok res' -> res'

let ipcclient_exn_wrapper thunk =
  try thunk () with
  | Unix.Unix_error (_, s1, s2) ->
      assert_failure ("StateIPCTestClient: Unix error: " ^ s1 ^ s2)
  | _ ->
      assert_failure "StateIPCTestClient: Unexpected error making JSON-RPC call"

(* Send msg via socket s with a delimiting character "0xA". *)
let send_delimited oc msg =
  let msg' = msg ^ "\n" in
  Caml.output_string oc msg';
  Caml.flush oc

let binary_rpc ~sock_addr (call : Rpc.call) : Rpc.response M.t =
  let socket =
    Unix.socket ~domain:Unix.PF_UNIX ~kind:Unix.SOCK_STREAM ~protocol:0
  in
  Unix.connect socket ~addr:(Unix.ADDR_UNIX sock_addr);
  let ic, oc =
    (Unix.in_channel_of_descr socket, Unix.out_channel_of_descr socket)
  in
  let msg_buf = Jsonrpc.string_of_call ~version:Jsonrpc.V2 call in
  (* Cannot use DebugMessage here as we're part of testsuite, and don't want to 
   * have races with the main executable that also (may) logs to the same files. *)
  (* (Printf.printf "StateIPCTestClient: Sending: %s\n" msg_buf); *)
  (* Send data to the socket. *)
  let _ = send_delimited oc msg_buf in
  (* Get response. *)
  let response = Caml.input_line ic in
  Unix.close socket;
  (* (Printf.printf "StateIPCTestClient: Response: %s\n" response); *)
  M.return @@ Jsonrpc.response_of_string response

(* Fetch full state variable from server (no indexing). *)
let fetch ~fname =
  let sock_addr, fields = assert_init () in
  let tp =
    match List.Assoc.find fields ~equal:( = ) fname with
    | Some tp -> tp
    | None ->
        assert_failure ("StateIPCTestClient: Unable to find field " ^ fname)
  in
  let open Ipcmessage_types in
  let q =
    {
      name = fname;
      mapdepth = TypeUtilities.map_depth tp;
      indices = [];
      ignoreval = false;
    }
  in
  let q' = encode_serialized_query q in
  let res =
    translate_res @@ IPCClient.fetch_state_value (binary_rpc ~sock_addr) q'
  in
  match res with
  | true, res' -> decode_serialized_value (Bytes.of_string res')
  | false, _ ->
      assert_failure
        ("StateIPCTestClient: Field " ^ fname ^ " not found on server")

(* Update full state variable to server (no indexing). *)
let update ~fname ~value =
  let open Ipcmessage_types in
  let sock_addr, fields = assert_init () in
  let tp =
    match List.Assoc.find fields ~equal:( = ) fname with
    | Some tp -> tp
    | None ->
        assert_failure ("StateIPCTestClient: Unable to find field " ^ fname)
  in
  let q =
    {
      name = fname;
      mapdepth = TypeUtilities.map_depth tp;
      indices = [];
      ignoreval = false;
    }
  in
  let q' = encode_serialized_query q in
  let value' = encode_serialized_value value in
  translate_res
  @@ IPCClient.update_state_value (binary_rpc ~sock_addr) q' value'

let fetch_all () =
  let _, fields = assert_init () in
  List.map fields ~f:(fun (fname, tp) -> (fname, tp, fetch ~fname))
