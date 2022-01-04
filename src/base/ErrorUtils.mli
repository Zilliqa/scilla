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
  fname : string;
  (* file name *)
  lnum : int;
  (* line number *)
  cnum : int; (* column number *)
}
[@@deriving sexp, equal]

val toLoc : Lexing.position -> loc

val dummy_loc : loc

val get_loc_str : loc -> string

(* `ekind` represents the reason for failure;
   `einst` can contain the concrete instance that causes the failure *)
type scilla_error = {
  ekind : string;
  einst : string option;
  startl : loc;
  endl : loc;
}

val mk_error_description : scilla_error -> string

val sprint_scilla_error_list : scilla_error list -> string

val mk_error0 : kind:string -> ?inst:string -> scilla_error list

val mk_error1 : kind:string -> ?inst:string -> loc -> scilla_error list

val mk_error2 : kind:string -> ?inst:string -> loc -> loc -> scilla_error list

type scilla_warning = { wmsg : string; wstartl : loc; wendl : loc; wid : int }

(* flag a warning, specifying a message and a warning "id".
   The "id" can be used to enable or disable specific warnings.
 *)
val warn0 : string -> int -> unit

val warn1 : string -> int -> loc -> unit

val warn2 : string -> int -> loc -> loc -> unit

val get_warnings : unit -> scilla_warning list

val reset_warnings : unit -> unit

exception Invalid_json of scilla_error list

val mk_invalid_json : kind:string -> ?inst:string -> exn

exception InternalError of scilla_error list

val mk_internal_error : kind:string -> ?inst:string -> exn

exception FatalError of string

val exit_with_error : string -> unit
