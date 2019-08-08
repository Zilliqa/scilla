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

(* This file aids Testcontracts.ml in setting up a state server
 * and initializing it with some initial data. *)

(* Use mock server in StateIPCTestServer? This should be
 * true unless we know there's a capable external server
 * running in the same socket address to connect to.
 * TODO: If need be, provide a command line flag for this. *)
let use_test_server = false

open OUnit2
open ErrorUtils
open JSON
open Core.Result.Let_syntax
open PrettyPrinters
open TypeUtil
open Core
open MonadUtil
open Syntax

(* This is used as a client to initialize the server with
 * whatever state we want to run the contract with. *)
module SS = StateService.MakeStateService ()

let literal_type_failure l =
  let t = TypeUtilities.literal_type l in
  match t with
  | Error emsg ->
    assert_failure (scilla_error_to_string emsg)
  | Ok s-> s

let json_file_to_state path =
  try
    ContractState.get_json_data path
  with
  | Invalid_json s ->
    assert_failure (scilla_error_to_string s)

(* Start a mock server (if set) at ~sock_addr and initialize its
 * state with ~state_json_path. *)
let setup_and_initialize ~sock_addr ~state_json_path =
  let state = json_file_to_state state_json_path in

  (* Setup a mock server within the testsuite? *)
  if use_test_server then StateIPCTestServer.start_server ~sock_addr;

  (* Initialize StateService. *)
  let fields = List.map state
    ~f:(fun (s, l) -> { fname = s; StateService.ftyp = literal_type_failure l; fval = None })
  in
  let sm = StateService.IPC (sock_addr) in
  let () = SS.initialize ~sm ~fields in
  (* Update the server (via StateService) with the state values we want. *)
  match
    mapM state ~f:(fun (s, v) ->
      if s <> ContractUtil.balance_label then
        let%bind _ = SS.update ~fname:(asId s) ~keys:[] ~value:v in
        pure ()
      else pure ()
    )
  with
  | Error s -> assert_failure (scilla_error_to_string s)
  | Ok _ -> ()

(* Get full state, and if a server was started in ~setup_and_initialize, shut it down. *)
let get_final_finish () =
  match SS.get_full_state (), SS.finalize() with
  | Ok fs', Ok () ->
    if use_test_server then StateIPCTestServer.stop_server();
    fs'
  | Error s, _ | Ok _, Error s ->
    assert_failure (scilla_error_to_string s)

open Yojson

let json_exn_wrapper thunk =
  try
    thunk()
  with
    | Json_error s
    | Basic.Util.Undefined (s, _)
    | Basic.Util.Type_error (s, _)
      -> raise (mk_invalid_json s)
    | _ -> assert_failure (Printf.sprintf "Unknown error parsing output JSON")

let json_from_string s =
  let thunk () = Basic.from_string s in
  json_exn_wrapper thunk

let json_to_assoc j =
  let thunk () = Basic.Util.to_assoc j in
  json_exn_wrapper thunk

let json_member m j =
  let thunk () = Basic.Util.member m j in
  json_exn_wrapper thunk

let json_to_string j =
  let thunk() = Basic.Util.to_string j in
  json_exn_wrapper thunk
 
(* Given the interpreter's output, parse the JSON, append svars to it and print out new JSOn. *)
let append_full_state interpreter_output svars =
  let j = json_from_string interpreter_output in
  let items = json_to_assoc j in
  let items' = List.map items ~f:(fun (s, j) ->
    if s <> "states" then (s, j) else
    (* Just add our states to "_balance" that's in the output JSON. *)
    let svars_j = ContractState.state_to_json svars in
    (s, Basic.Util.combine j svars_j)
  ) in
  Yojson.Basic.pretty_to_string (`Assoc items')

