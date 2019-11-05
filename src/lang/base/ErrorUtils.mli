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

(* Location info, slightly more usable than Lexing.position *)
type loc = {
  fname : string; (* file name *)
  lnum : int;     (* line number *)
  cnum : int;     (* column number *)
}
[@@deriving sexp]

val toLoc : Lexing.position -> loc

val dummy_loc : loc

val get_loc_str : loc -> string

type scilla_error = {
  emsg : string;
  startl : loc;
  endl : loc;
}

val sprint_scilla_error_list : scilla_error list -> string

val mk_error0 : string -> scilla_error list
val mk_error1 : string -> loc -> scilla_error list
val mk_error2 : string -> loc -> loc -> scilla_error list

type scilla_warning = {
  wmsg : string;
  wstartl : loc;
  wendl : loc;
  wid : int;
}

(* flag a warning, specifying a message and a warning "id". 
   The "id" can be used to enable or disable specific warnings.
 *)
val warn0 : string -> int -> unit
val warn1 : string -> int -> loc -> unit
val warn2 : string -> int -> loc -> loc -> unit

val get_warnings : unit -> scilla_warning list

exception Invalid_json of scilla_error list
val mk_invalid_json : string -> exn
