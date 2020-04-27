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
open! Int.Replace_polymorphic_compare
open Scilla_base
open FrontEndParser
open GlobalConfig
open ErrorUtils
open PrettyPrinters

let raise_if_error = function Ok _ -> () | Error e -> fatal_error e

let run () =
  let r_input_file = ref "" in
  let usage =
    "Usage:\n" ^ Sys.argv.(0) ^ " input.scilla (or input.scillib)\n"
  in
  let anon_handler s = r_input_file := s in
  let () = Arg.parse [] anon_handler usage in
  let input_file = !r_input_file in
  if String.is_empty input_file then fatal_error_noformat usage
  else set_use_json_errors true;
  let open FilePath in
  let open StdlibTracker in
  if check_extension input_file file_extn_library then
    (* Check library modules. *)
    raise_if_error @@ parse_lmodule input_file
  else if check_extension input_file file_extn_contract then
    (* Check contract modules. *)
    raise_if_error @@ parse_cmodule input_file
  else if check_extension input_file file_extn_expression then
    (* Check expressions. *)
    raise_if_error @@ parse_expr_from_file input_file
  else fatal_error (mk_error0 (sprintf "Unknown file extension\n"))

let () = try run () with FatalError msg -> exit_with_error msg
