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
open Printf
open Scilla_base
open Literal
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

(* Stdlib are implicitly imported, so we need to use local names in the parser *)
module FEParser = FrontEndParser.ScillaFrontEndParser (LocalLiteral)
module Parser = FEParser.Parser
module Syn = FEParser.FESyntax
module Dis = Disambiguate.ScillaDisambiguation (PSRep) (PERep)
module GlobalSyntax = Dis.PostDisSyntax
module TC = TypeChecker.ScillaTypechecker (PSRep) (PERep)
module TCSRep = TC.OutputSRep
module TCERep = TC.OutputERep
module PM_Checker = ScillaPatternchecker (TCSRep) (TCERep)
module TI = ScillaTypeInfo (TCSRep) (TCERep)
module GUA_Checker = ScillaGUA (TCSRep) (TCERep)

(* Check that the expression parses *)
let check_parsing filename =
  match FEParser.parse_file Parser.Incremental.exp_term filename with
  | Error _ -> fail0 (sprintf "Failed to parse input file %s\n." filename)
  | Ok e ->
      plog
      @@ sprintf "\n[Parsing]:\nExpression in [%s] is successfully parsed.\n"
           filename;
      pure e

let disambiguate e (std_lib : GlobalSyntax.libtree list) =
  let open Dis in
  let open GlobalSyntax in
  let%bind imp_var_dict, imp_typ_dict, imp_ctr_dict =
    foldM std_lib ~init:([], [], []) ~f:(fun acc_dicts lt ->
        let ({ libn; _ } : libtree) = lt in
        let lib_address = SIdentifier.as_string libn.lname in
        amend_ns_dict libn lib_address None acc_dicts
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
  | Ok e ->
      plog
      @@ sprintf "\n[Disambiguation]:\nExpression successfully disambiguated.\n";
      pure e

(* Type check the expression with external libraries *)
let check_typing e elibs gas_limit =
  let open TC in
  let open TC.TypeEnv in
  let rec_lib =
    {
      GlobalSyntax.lname =
        TCIdentifier.mk_loc_id (TCName.parse_simple_name "rec_lib");
      GlobalSyntax.lentries = recursion_principles;
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
  let filename = cli.input_file in
  let gas_limit = cli.gas_limit in
  match FEParser.parse_file Parser.Incremental.exp_term filename with
  | Ok e -> (
      (* Get list of stdlib dirs. *)
      let lib_dirs = StdlibTracker.get_stdlib_dirs () in
      if List.is_empty lib_dirs then stdlib_not_found_err ();
      (* Import all libs. *)
      let std_lib = import_all_libs lib_dirs in
      match disambiguate e std_lib with
      | Ok dis_e -> (
          match check_typing dis_e std_lib gas_limit with
          | Ok
              ( (typed_rlibs, typed_elibs, ((_, (e_typ, _)) as typed_erep)),
                _remaining_gas ) -> (
              match check_patterns typed_rlibs typed_elibs typed_erep with
              | Ok _ -> (
                  let tj =
                    [ ("type", `String (GlobalSyntax.SType.pp_typ e_typ.tp)) ]
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
      | Error e -> fatal_error e )
  | Error e -> fatal_error e

let () = try run () with FatalError msg -> exit_with_error msg
