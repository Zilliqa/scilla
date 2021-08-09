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
open Scilla_base
open Scilla_eval
open Literal
open RunnerUtil
open ErrorUtils
open GlobalConfig
open PrettyPrinters
open Result.Let_syntax
open ParserUtil
open MonadUtil
module RG = Gas.ScillaGas (ParserRep) (ParserRep)

(* Stdlib are implicitly imported, so we need to use local names in the parser *)
module FEParser = FrontEndParser.ScillaFrontEndParser (LocalLiteral)
module Dis = Disambiguate.ScillaDisambiguation (ParserRep) (ParserRep)
module GlobalSyntax = Dis.PostDisSyntax

let default_gas_limit = Stdint.Uint64.of_int 2000

let gas_cost_rewriter_wrapper gas_remaining rewriter anode =
  match rewriter anode with
  | Error e -> fatal_error_gas_scale Gas.scale_factor e gas_remaining
  | Ok anode' -> anode'

let disambiguate e (std_lib : GlobalSyntax.libtree list) =
  let open Dis in
  let open GlobalSyntax in
  let%bind imp_var_dict, imp_typ_dict, imp_ctr_dict =
    foldM std_lib ~init:([], [], []) ~f:(fun acc_dicts lt ->
        let ({ libn; _ } : libtree) = lt in
        let lib_address = SIdentifier.as_string libn.lname in
        amend_imported_ns_dict libn lib_address None acc_dicts
          (SIdentifier.get_rep libn.lname))
  in
  let imp_dicts =
    {
      var_dict = imp_var_dict;
      typ_dict = imp_typ_dict;
      ctr_dict = imp_ctr_dict;
    }
  in
  match disambiguate_exp imp_dicts e with
  | Error _ -> fail0 (sprintf "Failed to disambiguate\n")
  | Ok e -> pure e

let run () =
  GlobalConfig.reset ();
  ErrorUtils.reset_warnings ();
  Datatypes.DataTypeDictionary.reinit ();
  let cli = parse_cli None ~exe_name:Sys.argv.(0) in
  let filename = cli.input_file in
  let gas_limit' =
    if Stdint.Uint64.(compare cli.gas_limit zero = 0) then default_gas_limit
    else cli.gas_limit
  in
  let gas_limit = Stdint.Uint64.mul gas_limit' Gas.scale_factor in
  match FEParser.parse_expr_from_file filename with
  | Ok e_nogas -> (
      StdlibTracker.add_stdlib_dirs cli.stdlib_dirs;
      let lib_dirs = StdlibTracker.get_stdlib_dirs () in
      if List.is_empty lib_dirs then stdlib_not_found_err ();
      (* Import all libraries in known stdlib paths. *)
      let elibs =
        List.map ~f:(gas_cost_rewriter_wrapper gas_limit RG.libtree_cost)
        @@ import_all_libs lib_dirs
      in
      match disambiguate e_nogas elibs with
      | Ok dis_e_nogas -> (
          let dis_e =
            gas_cost_rewriter_wrapper gas_limit RG.expr_static_cost dis_e_nogas
          in
          (* Since this is not a contract, we have no in-contract lib defined. *)
          let envres = Eval.init_libraries None elibs in
          let env, gas_remaining =
            match envres Eval.init_gas_kont gas_limit with
            | Ok (env', gas_remaining) -> (env', gas_remaining)
            | Error (err, gas_remaining) ->
                fatal_error_gas_scale Gas.scale_factor err gas_remaining
          in
          let lib_fnames = List.map ~f:(fun (name, _) -> name) env in
          let res' = Eval.(exp_eval dis_e env init_gas_kont gas_remaining) in
          match res' with
          | Ok (_, gas_remaining) ->
              let gas_remaining' =
                Gas.finalize_remaining_gas cli.gas_limit gas_remaining
              in
              printf "%s\n" (Eval.pp_result res' lib_fnames gas_remaining')
          | Error (el, gas_remaining) ->
              fatal_error_gas_scale Gas.scale_factor el gas_remaining)
      | Error e -> fatal_error e)
  | Error e -> fatal_error e

let () = try run () with FatalError msg -> exit_with_error msg
