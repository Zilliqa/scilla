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

open Core_kernel

(* Location info, slightly more usable than Lexing.position *)
type loc = {
  fname : string;
  (* file name *)
  lnum : int;
  (* line number *)
  cnum : int; (* column number *)
}
[@@deriving sexp, equal]

let toLoc (p : Lexing.position) : loc =
  {
    fname = p.pos_fname;
    lnum = p.pos_lnum;
    (* Translate from Lexing.position *)
    cnum = p.pos_cnum - p.pos_bol + 1;
  }

let dummy_loc = toLoc Lexing.dummy_pos

let get_loc_str (l : loc) : string =
  l.fname ^ ":" ^ Int.to_string l.lnum ^ ":" ^ Int.to_string l.cnum

type scilla_error = {
  ekind : string;
  einst : string option;
  startl : loc;
  endl : loc;
}

(* combine the error kind and the concrete error instance *)
let mk_error_description err =
  let optional_instance =
    match err.einst with None | Some "" -> "" | Some einst -> ": " ^ einst
  in
  err.ekind ^ optional_instance

let sprint_scilla_error_list elist =
  List.fold elist ~init:"" ~f:(fun acc e ->
      acc ^ "\n\n" ^ mk_error_description e ^ "[" ^ get_loc_str e.startl ^ "]["
      ^ get_loc_str e.endl ^ "]" ^ "\n")

(* we don't use optional parameters here, because optional parameters
   can only be implicitly applied after a positional parameter,
   and we don't have any positional parameters for mk_error0 *)
let[@warning "-16"] mk_error0 ~kind ?inst =
  [ { ekind = kind; einst = inst; startl = dummy_loc; endl = dummy_loc } ]

let mk_error1 ~kind ?inst sloc =
  [ { ekind = kind; einst = inst; startl = sloc; endl = dummy_loc } ]

let mk_error2 ~kind ?inst sloc eloc =
  [ { ekind = kind; einst = inst; startl = sloc; endl = eloc } ]

type scilla_warning = { wmsg : string; wstartl : loc; wendl : loc; wid : int }

let warnings_list = ref []

(* flag a warning, specifying a message and a warning "id".
   The "id" can be used to enable or disable specific warnings.
 *)
let warn0 msg id =
  let warning =
    { wmsg = msg; wstartl = dummy_loc; wendl = dummy_loc; wid = id }
  in
  warnings_list := warning :: !warnings_list

let warn1 msg id sloc =
  let warning = { wmsg = msg; wstartl = sloc; wendl = dummy_loc; wid = id } in
  warnings_list := warning :: !warnings_list

let warn2 msg id sloc eloc =
  let warning = { wmsg = msg; wstartl = sloc; wendl = eloc; wid = id } in
  warnings_list := warning :: !warnings_list

let get_warnings () = !warnings_list

let reset_warnings () = warnings_list := []

exception Invalid_json of scilla_error list

let[@warning "-16"] mk_invalid_json ~kind ?inst =
  Invalid_json (mk_error0 ~kind ?inst)

exception InternalError of scilla_error list

let[@warning "-16"] mk_internal_error ~kind ?inst =
  InternalError (mk_error0 ~kind ?inst)

exception FatalError of string

let exit_with_error msg =
  DebugMessage.perr msg;
  exit 1
