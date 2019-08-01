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


open Syntax
open ParserUtil
open RunnerUtil
open GlobalConfig
open PrettyPrinters
open Core

module ParsedSyntax = ParserUtil.ParsedSyntax
module PSRep = ParserRep
module PERep = ParserRep
  
module TC = TypeChecker.ScillaTypechecker (PSRep) (PERep)
module TCSRep = TC.OutputSRep
module TCERep = TC.OutputERep


let default_gas_limit = Stdint.Uint64.of_int 2000

let () =
  let cli = parse_cli() in
  let filename = cli.input_file in
  let gas_limit = if cli.gas_limit = Stdint.Uint64.zero then default_gas_limit else cli.gas_limit in
  match FrontEndParser.parse_file ScillaParser.exp_term filename with
  | Ok e ->
      (* Since this is not a contract, we have no in-contract lib defined. *)
      let clib = { TC.UntypedSyntax.lname = asId "dummy";
                   TC.UntypedSyntax.lentries = [] } in
      StdlibTracker.add_stdlib_dirs cli.stdlib_dirs;
      let lib_dirs = StdlibTracker.get_stdlib_dirs() in
      if lib_dirs = [] then stdlib_not_found_err ();
      (* Import all libraries in known stdlib paths. *)
      let elibs = import_all_libs lib_dirs in
      let envres = Eval.init_libraries (Some clib) elibs in
      let env, gas_remaining = 
        (match envres Eval.init_gas_kont gas_limit with
        | Ok (env', gas_remaining) -> env', gas_remaining
        | Error (err, gas_remaining) -> fatal_error_gas err gas_remaining)
      in
      let lib_fnames = List.map ~f:(fun (name, _) -> name) env in
      let res' = Eval.exp_eval_wrapper e env in
      let res = res' Eval.init_gas_kont gas_remaining in
      (match res with
      | Ok _ ->
          printf "%s\n" (Eval.pp_result res lib_fnames)
      | Error (el, gas_remaining) -> fatal_error_gas el gas_remaining)
  | Error e -> fatal_error e
