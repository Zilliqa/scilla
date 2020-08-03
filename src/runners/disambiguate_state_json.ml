(*
  This file is part of scilla.

  Copyright (c) 2020 - present Zilliqa Research Pvt. Ltd.

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
open Scilla_base
open Scilla_eval
open ErrorUtils
open RunnerCLI

(* ************************************************************************ * 
 * This executable parses a contract and reads its associated state using   *
 * local names, and outputs its state using global names. The state itself  *
 * does not change, only the type and constructor names that appear in the  *
 * state.                                                                   *
 *                                                                          *
 * This transformation is necessary in order to enable external libraries   *
 * and remote state reads.                                                  *
 * ************************************************************************ *)

type args = {
  input_init : string;
  input_state : string;
  output : string;
  input : string;
  libdirs : string list;
}

let f_input_init = ref ""

let f_input_state = ref ""

let f_output = ref ""

let f_input = ref ""

let d_libs = ref []

let reset () =
  f_input_init := "";
  f_input_state := "";
  f_output := "";
  f_input := "";
  d_libs := []

let validate_main usage =
  let open Core_kernel in
  (* not mandatory file name input, but if provided, should be valid *)
  let invalid_optional_fname fname =
    not (String.is_empty fname || Sys.file_exists fname)
  in
  let msg = "" in
  let msg =
    (* init.json is mandatory *)
    if not @@ Sys.file_exists !f_input_init then "Invalid initialization file\n"
    else msg
  in
  let msg =
    (* input_state.json is not mandatory, but if provided, should be valid *)
    if invalid_optional_fname !f_input_state then
      msg ^ "Invalid input contract state: " ^ !f_input_state ^ "\n"
    else msg
  in
  let msg =
    (* input file is mandatory *)
    if not @@ Sys.file_exists !f_input then
      msg ^ "Invalid input contract file\n"
    else msg
  in
  (* Note: output file is optional, if it's missing we will output to stdout *)
  if not @@ String.is_empty msg then
    PrettyPrinters.fatal_error_noformat (usage ^ Printf.sprintf "%s\n" msg)

let parse ~exe_name =
  reset ();
  let speclist =
    [
      ( "-init",
        Arg.String (fun x -> f_input_init := x),
        "Path to initialization json" );
      ( "-istate",
        Arg.String (fun x -> f_input_state := x),
        "Path to state input json" );
      ("-o", Arg.String (fun x -> f_output := x), "Path to output json");
      ("-i", Arg.String (fun x -> f_input := x), "Path to scilla contract");
      ( "-libdir",
        Arg.String
          (fun x ->
            let xl =
              if String.is_empty x then [] else Str.split (Str.regexp "[;:]") x
            in
            d_libs := !d_libs @ xl),
        "Path(s) to directory containing libraries separated by ':' (';' on \
         windows)" );
    ]
  in
  let mandatory_usage =
    "Usage:\n" ^ exe_name ^ " -init init.json -istate input_state.json"
    ^ " [-o output.json] -i input.scilla -libdir /path/to/stdlib" ^ "\n"
  in
  let optional_usage =
    String.concat ~sep:"\n  "
    @@ List.map speclist ~f:(fun (flag, _, desc) -> flag ^ " " ^ desc)
  in
  let usage = mandatory_usage ^ "\n  " ^ optional_usage ^ "\n" in
  let ignore_anon _ = () in
  let () = Arg.parse speclist ignore_anon mandatory_usage in
  let () = validate_main usage in
  {
    input_init = !f_input_init;
    input_state = !f_input_state;
    output = !f_output;
    input = !f_input;
    libdirs = !d_libs;
  }

let () =
  GlobalConfig.reset ();
  ErrorUtils.reset_warnings ();
  Datatypes.DataTypeDictionary.reinit ();
  try
    let args = parse ~exe_name:(Sys.get_argv ()).(0) in
    let output = run_with_args args in
    let str = Yojson.Basic.to_string output in
    if String.is_empty args.output then DebugMessage.pout str
    else
      Out_channel.with_file args.output ~f:(fun ch ->
          Out_channel.output_string ch str)
  with FatalError msg -> exit_with_error msg


