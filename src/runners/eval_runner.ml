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
open Scilla_eval
open FrontEndParser
open RunnerUtil
open ErrorUtils
open GlobalConfig
open PrettyPrinters

let default_gas_limit = Stdint.Uint64.of_int 2000

let run () =
  GlobalConfig.reset ();
  ErrorUtils.reset_warnings ();
  Datatypes.DataTypeDictionary.reinit ();
  let cli = parse_cli None ~exe_name:Sys.argv.(0) in
  let filename = cli.input_file in
  let gas_limit =
    if Stdint.Uint64.(compare cli.gas_limit zero = 0) then default_gas_limit
    else cli.gas_limit
  in
  match parse_expr_from_file filename with
  | Ok e -> (
      StdlibTracker.add_stdlib_dirs cli.stdlib_dirs;
      let lib_dirs = StdlibTracker.get_stdlib_dirs () in
      if List.is_empty lib_dirs then stdlib_not_found_err ();
      (* Import all libraries in known stdlib paths. *)
      let elibs = import_all_libs lib_dirs in
      (* Since this is not a contract, we have no in-contract lib defined. *)
      let envres = Eval.init_libraries None elibs in
      let env, gas_remaining =
        match envres Eval.init_gas_kont gas_limit with
        | Ok (env', gas_remaining) -> (env', gas_remaining)
        | Error (err, gas_remaining) -> fatal_error_gas err gas_remaining
      in
      let lib_fnames = List.map ~f:(fun (name, _) -> name) env in
      let res = Eval.(exp_eval_wrapper e env init_gas_kont gas_remaining) in
      match res with
      | Ok _ -> printf "%s\n" (Eval.pp_result res lib_fnames)
      | Error (el, gas_remaining) -> fatal_error_gas el gas_remaining )
  | Error e -> fatal_error e

let () = try run () with FatalError msg -> exit_with_error msg
