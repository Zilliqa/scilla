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

(* Location info, slightly more usable than Lexing.position *)
type loc = {
  fname : string; (* file name *)
  lnum : int;     (* line number *)
  cnum : int;     (* column number *)
}
[@@deriving sexp]

let toLoc (p : Lexing.position) : loc = {
  fname = p.pos_fname;
  lnum = p.pos_lnum; 
  (* Translate from Lexing.position *)
  cnum = p.pos_cnum - p.pos_bol + 1;
}

let dummy_loc =
  toLoc Lexing.dummy_pos

let get_loc_str (l : loc) : string =
  l.fname ^ ":" ^ Int.to_string l.lnum ^ 
      ":" ^ Int.to_string (l.cnum)

type scilla_error = {
  emsg : string;
  startl : loc;
  endl : loc;
}

let sprint_scilla_error_list elist =
    List.fold elist ~init:"" ~f:(fun acc e ->
        acc ^ "\n\n" ^ e.emsg ^ "[" ^ (get_loc_str e.startl) ^ "][" 
        ^ (get_loc_str e.endl) ^"]" ^ "\n")

let mk_error0 msg = [{ emsg = msg; startl = dummy_loc; endl = dummy_loc }]
let mk_error1 msg sloc = [{ emsg = msg; startl = sloc; endl = dummy_loc }]
let mk_error2 msg sloc eloc = [{ emsg = msg; startl = sloc; endl = eloc }]

type scilla_warning = {
  wmsg : string;
  wstartl : loc;
  wendl : loc;
  wid : int;
}

let warnings_list = ref []

(* flag a warning, specifying a message and a warning "id". 
   The "id" can be used to enable or disable specific warnings.
 *)
let warn0 msg id =
  let warning = { wmsg = msg; wstartl = dummy_loc; wendl = dummy_loc; wid = id } in
  warnings_list := warning::!warnings_list

let warn1 msg id sloc =
  let warning = { wmsg = msg; wstartl = sloc; wendl = dummy_loc; wid = id } in
  warnings_list := warning::!warnings_list

let warn2 msg id sloc eloc =
  let warning = { wmsg = msg; wstartl = sloc; wendl = eloc; wid = id } in
  warnings_list := warning::!warnings_list

let get_warnings () =
  !warnings_list

exception Invalid_json of scilla_error list
let mk_invalid_json msg = Invalid_json (mk_error0 msg)
