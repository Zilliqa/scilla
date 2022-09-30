(*
  This file is part of scilla.

  Copyright (c) 2021 - present Zilliqa Research Pvt. Ltd.

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
open Printf
open Literal
open ParserUtil
open RecursionPrinciples
open DebugMessage
open MonadUtil
open Result.Let_syntax
open PatternChecker
open GasUseAnalysis
open TypeInfo
module PSRep = ParserRep
module PERep = ParserRep

(* Stdlib are implicitly imported, so we need to use local names in the parser *)
module FEParser = FrontEndParser.ScillaFrontEndParser (LocalLiteral)
module Parser = FEParser.Parser
module Syn = FEParser.FESyntax
module Dis = Disambiguate.ScillaDisambiguation (PSRep) (PERep)
module GlobalSyntax = Dis.PostDisSyntax
module RC = Recursion.ScillaRecursion (PSRep) (PERep)
module RCSRep = RC.OutputSRep
module RCERep = RC.OutputERep
module TC = TypeChecker.ScillaTypechecker (RCSRep) (RCERep)
module TCSRep = TC.OutputSRep
module TCERep = TC.OutputERep
module PM_Checker = ScillaPatternchecker (TCSRep) (TCERep)
module TI = ScillaTypeInfo (TCSRep) (TCERep)
module GUA_Checker = ScillaGUA (TCSRep) (TCERep)

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
  | Error _ -> fail0 ~kind:"Failed to disambiguate" ?inst:None
  | Ok e ->
      plog
      @@ sprintf "\n[Disambiguation]:\nExpression successfully disambiguated.\n";
      pure e

let check_recursion e elibs =
  let%bind rrlibs, relibs =
    match RC.recursion_rprins_elibs recursion_principles elibs None with
    | Error s -> fail s
    | Ok (rlibs, elibs, _, emsgs) ->
        if List.is_empty emsgs then pure (rlibs, elibs) else fail emsgs
  in
  let%bind re = RC.recursion_exp e in
  pure (rrlibs, relibs, re)

(* Type check the expression with external libraries *)
let check_typing e elibs rlibs gas_limit =
  let open TC in
  let open TC.TypeEnv in
  let rec_lib =
    {
      RC.lname = TCIdentifier.mk_loc_id (TCName.parse_simple_name "rec_lib");
      RC.lentries = rlibs;
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
