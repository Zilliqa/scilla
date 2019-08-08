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
open Core
open ErrorUtils
open PrettyPrinters
open ParserUtil
open DebugMessage
open MonadUtil
open Result.Let_syntax
open RunnerUtil
open PatternChecker
open SanityChecker
open GasUseAnalysis
open RecursionPrinciples
open EventInfo
open Cashflow
open Accept

module ParsedSyntax = ParserUtil.ParsedSyntax
module PSRep = ParserRep
module PERep = ParserRep

module Rec = Recursion.ScillaRecursion (PSRep) (PERep)
module RecSRep = Rec.OutputSRep
module RecERep = Rec.OutputERep

module TC = TypeChecker.ScillaTypechecker (RecSRep) (RecERep)
module TCSRep = TC.OutputSRep
module TCERep = TC.OutputERep

module PMC = ScillaPatternchecker (TCSRep) (TCERep)
module PMCSRep = PMC.SPR
module PMCERep = PMC.EPR

module SC = ScillaSanityChecker (TCSRep) (TCERep)
module EI = ScillaEventInfo (PMCSRep) (PMCERep)

module GUA = ScillaGUA (TCSRep) (TCERep)
module CF = ScillaCashflowChecker (TCSRep) (TCERep)
module AC = ScillaAcceptChecker (TCSRep) (TCERep)

(* Check that the module parses *)
let check_parsing ctr syn = 
  let cmod = FrontEndParser.parse_file syn ctr in
  if Result.is_ok cmod then
    plog @@ sprintf "\n[Parsing]:\n module [%s] is successfully parsed.\n" ctr;
  cmod

(* Type check the contract with external libraries *)
let check_recursion cmod elibs  =
  let open Rec in
  let res = recursion_module cmod recursion_principles elibs in
  if Result.is_ok res then
    plog @@ sprintf "\n[Recursion Check]:\n module [%s] is successfully checked.\n" (get_id cmod.contr.cname);
  res

let check_recursion_lmod lmod elibs  =
  let open Rec in
  let res = recursion_lmodule lmod recursion_principles elibs in
  if Result.is_ok res then
    plog @@ sprintf "\n[Recursion Check]:\n lmodule [%s] is successfully checked.\n" (get_id lmod.libs.lname);
  res

(* Type check the contract with external libraries *)
let check_typing cmod rprin elibs  =
  let open TC in
  let res = type_module cmod rprin elibs in
  if Result.is_ok res then
    plog @@ sprintf "\n[Type Check]:\n module [%s] is successfully checked.\n" (get_id cmod.contr.cname);
  res

(* Type check the contract with external libraries *)
let check_typing_lmod lmod rprin elibs  =
  let open TC in
  let res = type_lmodule lmod rprin elibs in
  if Result.is_ok res then
    plog @@ sprintf "\n[Type Check]:\n lmodule [%s] is successfully checked.\n" (get_id lmod.libs.lname);
  res

let check_patterns e  =
  let res = PMC.pm_check_module e in
  if Result.is_ok res then
    plog @@ sprintf "\n[Pattern Check]:\n module [%s] is successfully checked.\n" (get_id e.contr.cname);
  res

let check_sanity m rlibs elibs =
  let res = SC.contr_sanity m rlibs elibs in
  if Result.is_ok res then
    plog @@ sprintf "\n[Sanity Check]:\n module [%s] is successfully checked.\n" (get_id m.contr.cname);
  res

let check_accepts m =AC.contr_sanity m

let analyze_print_gas cmod typed_elibs =
  let res = GUA.gua_module cmod typed_elibs in
  match res with
  | Error msg -> pout @@ scilla_error_to_string msg ; res
  | Ok cpol ->
    plog @@ sprintf "\n[Gas Use Analysis]:\n module [%s] is successfully analyzed.\n" (get_id cmod.contr.cname);
    let _ = List.iter ~f:(fun (i, pol) ->
        pout @@ sprintf "Gas use polynomial for transition %s:\n%s\n\n" (get_id i)
          (GUA.sprint_gup pol)
      ) cpol;
    in res

let check_cashflow typed_cmod token_fields =
  let (param_field_tags, ctr_tags) = CF.main typed_cmod token_fields in
  let param_field_tags_to_string = List.map param_field_tags
      ~f:(fun (i, t) ->
          (i, CF.ECFR.money_tag_to_string t)) in
  let ctr_tags_to_string = List.map ctr_tags
      ~f:(fun (adt, ctrs) ->
          (adt, List.map ctrs
             ~f:(fun (i, ts) ->
                 (i, List.map ts ~f:(fun t_opt -> Option.value_map t_opt ~default:"_" ~f:CF.ECFR.money_tag_to_string))))) in
  (param_field_tags_to_string, ctr_tags_to_string)
      
let check_version vernum =
  let (mver, _, _) = scilla_version in
  if vernum <> mver
  then
    let emsg =  sprintf "Scilla version mismatch. Expected %d vs Contract %d\n" mver vernum in
    fatal_error (mk_error0 emsg)

(* Check a library module. *)
let check_lmodule cli =
  let r = (
    let%bind (lmod : ParsedSyntax.lmodule) = check_parsing cli.input_file ScillaParser.lmodule in
    let elibs = import_libs lmod.elibs cli.init_file  in
    let%bind (recursion_lmod, recursion_rec_principles, recursion_elibs) = 
      check_recursion_lmod lmod elibs in
    let%bind (typed_lmod, typed_elibs, typed_rlibs) = 
      check_typing_lmod recursion_lmod recursion_rec_principles recursion_elibs in
    pure (typed_lmod, typed_elibs, typed_rlibs)
  ) in
  (match r with
  | Error s -> fatal_error s
  | Ok _ ->
      let warnings_output =
        [ ("warnings", scilla_warning_to_json (get_warnings())) ]
      in
      let j = `Assoc warnings_output in
      pout (sprintf "%s\n" (Yojson.pretty_to_string j));)

(* Check a contract module. *)
let check_cmodule cli =
  let r = (
    let%bind (cmod : ParsedSyntax.cmodule) = check_parsing cli.input_file ScillaParser.cmodule  in
    (* Import whatever libs we want. *)
    let elibs = import_libs cmod.elibs cli.init_file in
    let%bind (recursion_cmod, recursion_rec_principles, recursion_elibs) = check_recursion cmod elibs in
    let%bind (typed_cmod, tenv, typed_elibs, typed_rlibs) = check_typing recursion_cmod recursion_rec_principles recursion_elibs  in
    let%bind pm_checked_cmod = check_patterns typed_cmod  in
    let _ = if cli.cf_flag then check_accepts typed_cmod else () in
    let%bind _ = check_sanity typed_cmod typed_rlibs typed_elibs in
    let%bind event_info = EI.event_info pm_checked_cmod in
    let%bind _ = if cli.gua_flag then analyze_print_gas typed_cmod typed_elibs else pure [] in
    let cf_info_opt = if cli.cf_flag then Some (check_cashflow typed_cmod cli.cf_token_fields) else None in
    pure @@ (cmod, tenv, event_info, cf_info_opt)
  ) in
  (match r with
  | Error s -> fatal_error s
  | Ok (cmod, _, event_info, cf_info_opt) ->
      let base_output =
        let warnings_output =
          [ ("warnings", scilla_warning_to_json (get_warnings())) ]
        in
        if cli.p_contract_info then
          ("contract_info", (JSON.ContractInfo.get_json cmod.smver cmod.contr event_info)) :: warnings_output
        else warnings_output
      in
      let output_with_cf =
        match cf_info_opt with
        | None -> base_output
        | Some cf_info -> ("cashflow_tags", JSON.CashflowInfo.get_json cf_info) :: base_output in
      let j = `Assoc output_with_cf in
      check_version cmod.smver;
      pout (sprintf "%s\n" (Yojson.Basic.pretty_to_string j));)

let () =
    let cli = parse_cli () in
    let open GlobalConfig in

    StdlibTracker.add_stdlib_dirs cli.stdlib_dirs;
    let file_extn = FilePath.get_extension cli.input_file in
    (* Get list of stdlib dirs. *)
    let lib_dirs = StdlibTracker.get_stdlib_dirs() in
    if lib_dirs = [] then stdlib_not_found_err ();

    (* Testsuite runs this executable with cwd=tests and ends
       up complaining about missing _build directory for logger.
       So disable the logger. *)
    set_debug_level Debug_None;

    (* Check library modules. *)
    if file_extn = StdlibTracker.file_extn_library
    then
      check_lmodule cli
    else if file_extn <> StdlibTracker.file_extn_contract
    then
      fatal_error (mk_error0(sprintf "Unknown file extension %s\n" file_extn))
    else
      (* Check contract modules. *)
      check_cmodule cli
