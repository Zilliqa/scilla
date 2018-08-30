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


open Printf
open Syntax
open RunnerUtil
open GlobalConfig

let () =
  if (Array.length Sys.argv) < 2 || (Array.length Sys.argv) > 3
  then
    (printf "%s\n" ("Usage: " ^ Sys.argv.(0) ^ " /path/to/exp.scilla [/path/to/stdlib]");
    exit 1)
  else
  let filename = Sys.argv.(1) in
  match FrontEndParser.parse_file ScillaParser.exps filename with
  | Some [e] ->
      (* Since this is not a contract, we have no in-contract lib defined. *)
      let clib = { TypeChecker.ScillaTypechecker.UntypedSyntax.lname = asId "dummy";
                   TypeChecker.ScillaTypechecker.UntypedSyntax.lentries = [] } in
      (* This is an auxiliary executable, it's second argument must
       * have a list of stdlib dirs, so note that down. *)
      add_cmd_stdlib ();
      let lib_dirs = StdlibTracker.get_stdlib_dirs() in
      if lib_dirs = [] then stdlib_not_found_err ();
      (* Import all libraries in known stdlib paths. *)
      let elibs = import_all_libs lib_dirs in
      let envres = Eval.init_libraries (Some clib) elibs in
      let env = (match envres with
        | Ok (env', _) -> env'
        | Error (err, _) ->
          printf "Failed to initialize stdlib. Evaluation halted: %s\n" err;
          exit 1;) in
      let lib_fnames = List.map (fun (name, _) -> name) env in
      let res = Eval.exp_eval e env in
      (match res with
      | Ok _ ->
          printf "%s\n" (Eval.pp_result res lib_fnames)
      | Error _ -> printf "Failed execution:\n%s\n" (Eval.pp_result res lib_fnames))
  | Some _ | None ->
      printf "%s\n" "Failed to parse input file."
  


