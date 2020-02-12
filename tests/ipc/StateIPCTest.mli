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

(* Start a mock server (if set) at ~sock_addr and initialize its
 * state with ~state_json_path. *)
val setup_and_initialize :
  start_mock_server:bool -> sock_addr:string -> state_json_path:string -> string

(* Get full state, and if a server was started in ~setup_and_initialize, shut it down. *)
val get_final_finish :
  sock_addr:string ->
  (string * Syntax.typ * Ipcmessage_types.proto_scilla_val) list

(* Given the interpreter's output, parse the JSON, append svars to it and print out new JSON. *)
val append_full_state :
  goldoutput_file:string ->
  interpreter_output:string ->
  (string * Syntax.typ * Ipcmessage_types.proto_scilla_val) list ->
  string

val json_from_string : string -> Yojson.Basic.t

val json_to_string : Yojson.Basic.t -> string
