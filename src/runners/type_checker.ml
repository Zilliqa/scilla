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
open Printf
open Scilla_base
open ParserUtil
open TypeUtil
open RecursionPrinciples
open RunnerUtil
open DebugMessage
open MonadUtil
open Result.Let_syntax
open PatternChecker
open PrettyPrinters
open GasUseAnalysis
open TypeInfo
open ErrorUtils
module PSRep = ParserRep
module PERep = ParserRep
module Parser = ScillaParser.Make (ParserSyntax)
module TC = TypeChecker.ScillaTypechecker (PSRep) (PERep)
module TCSRep = TC.OutputSRep
module TCERep = TC.OutputERep
module PM_Checker = ScillaPatternchecker (TCSRep) (TCERep)
module TI = ScillaTypeInfo (TCSRep) (TCERep)
module GUA_Checker = ScillaGUA (TCSRep) (TCERep)

(* Check that the expression parses *)
let check_parsing filename =
  match FrontEndParser.parse_file Parser.Incremental.exp_term filename with
  | Error _ -> fail0 (sprintf "Failed to parse input file %s\n." filename)
  | Ok e ->
      plog
      @@ sprintf "\n[Parsing]:\nExpression in [%s] is successfully parsed.\n"
           filename;
      pure e

(* Type check the expression with external libraries *)
let check_typing e elibs gas_limit =
  let open TC in
  let open TC.TypeEnv in
  let rec_lib =
    {
      ParserSyntax.lname = TCIdentifier.mk_loc_id "rec_lib";
      ParserSyntax.lentries = recursion_principles;
    }
  in
  let tenv0 = TEnv.mk () in
  let%bind typed_rlibs, remaining_gas = type_library tenv0 rec_lib gas_limit in
  (* Step 1: Type check external libraries *)
  let%bind typed_elibs, remaining_gas =
    type_libraries elibs tenv0 remaining_gas
  in
  let%bind typed_expr, remaining_gas =
    type_expr e tenv0 init_gas_kont remaining_gas
  in
  pure ((typed_rlibs, typed_elibs, typed_expr), remaining_gas)

let check_patterns rlibs elibs e =
  let%bind pm_checked_rlibs = PM_Checker.pm_check_library rlibs in
  let%bind pm_checked_elibs = mapM elibs ~f:PM_Checker.pm_check_libtree in
  let%bind pm_checked_e = PM_Checker.pm_check_expr e in
  pure (pm_checked_rlibs, pm_checked_elibs, pm_checked_e)

let analyze_gas e = GUA_Checker.gua_expr_wrapper e

let run () =
  GlobalConfig.reset ();
  ErrorUtils.reset_warnings ();
  Datatypes.DataTypeDictionary.reinit ();
  let cli = parse_cli None ~exe_name:Sys.argv.(0) in
  let open GlobalConfig in
  StdlibTracker.add_stdlib_dirs cli.stdlib_dirs;
  set_debug_level Debug_None;
  let filename = cli.input_file in
  let gas_limit = cli.gas_limit in
  match FrontEndParser.parse_file Parser.Incremental.exp_term filename with
  | Ok e -> (
      (* Get list of stdlib dirs. *)
      let lib_dirs = StdlibTracker.get_stdlib_dirs () in
      if List.is_empty lib_dirs then stdlib_not_found_err ();
      (* Import all libs. *)
      let std_lib = import_all_libs lib_dirs in
      match check_typing e std_lib gas_limit with
      | Ok
          ( (typed_rlibs, typed_elibs, ((_, (e_typ, _)) as typed_erep)),
            _remaining_gas ) -> (
          match check_patterns typed_rlibs typed_elibs typed_erep with
          | Ok _ -> (
              let tj =
                [ ("type", `String (FrontEndParser.FEPType.pp_typ e_typ.tp)) ]
              in
              let output_j =
                `Assoc
                  ( if cli.p_type_info then
                    ( "type_info",
                      JSON.TypeInfo.type_info_to_json
                        (TI.type_info_expr typed_erep) )
                    :: tj
                  else tj )
              in
              pout (sprintf "%s\n" (Yojson.Basic.pretty_to_string output_j));
              if cli.gua_flag then
                match analyze_gas typed_erep with
                | Ok _ -> ()
                | Error el -> fatal_error el )
          | Error el -> fatal_error el )
      | Error ((_, el), _remaining_gas) -> fatal_error el )
  | Error e -> fatal_error e

let () = try run () with FatalError msg -> exit_with_error msg
