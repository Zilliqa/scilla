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
open Recursion
open EventInfo
open Cashflow
open Accept

module ParsedSyntax = ParserUtil.ParsedSyntax
module PSRep = ParserRep
module PERep = ParserRep

module Rec = ADTChecker.ScillaRecursion (PSRep) (PERep)
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
module CF = ScillaCashflowChecker (TCSRep) (TCERep)
module AC = ScillaAcceptChecker (TCSRep) (TCERep)

(* Check that the module parses *)
let check_parsing ctr syn = 
    let parse_module =
      FrontEndParser.parse_file syn ctr in
    match parse_module with
    | None -> exit 1 (* Error is printed by the parser. *)
    | Some cmod -> 
        plog @@ sprintf
          "\n[Parsing]:\n module [%s] is successfully parsed.\n" ctr;
        pure cmod

(* Type check the contract with external libraries *)
let check_recursion cmod elibs  =
  let open Rec in
  let res = recursion_module cmod recursion_principles elibs in
  match res with
  | Error msgs -> pout @@ scilla_error_to_string msgs ; res
  | Ok recursion_module -> pure @@ recursion_module

let check_recursion_lmod lmod elibs  =
  let open Rec in
  let res = recursion_lmodule lmod recursion_principles elibs in
  match res with
  | Error msgs -> pout @@ scilla_error_to_string msgs ; res
  | Ok recursion_module -> pure @@ recursion_module

(* Type check the contract with external libraries *)
let check_typing cmod rprin elibs  =
  let open TC in
  let res = type_module cmod rprin elibs in
  match res with
  | Error msgs -> pout @@ scilla_error_to_string msgs ; res
  | Ok typed_module -> pure @@ typed_module

(* Type check the contract with external libraries *)
let check_typing_lmod lmod rprin elibs  =
  let open TC in
  let res = type_lmodule lmod rprin elibs in
  match res with
  | Error msgs -> pout @@ scilla_error_to_string msgs ; res
  | Ok typed_module -> pure @@ typed_module

let check_patterns e  =
  let res = PMC.pm_check_module e in
  match res with
  | Error msg -> pout @@ scilla_error_to_string msg ; res
  | Ok pm_checked_module -> pure @@ pm_checked_module

let check_sanity m rlibs elibs =
  let res = SC.contr_sanity m rlibs elibs in
  match res with
  | Error msg -> pout @@ scilla_error_to_string msg ; res
  | Ok _ -> pure ()

let check_accepts m =AC.contr_sanity m

let check_events_info einfo  =
  match einfo with
  | Error msg -> pout @@ scilla_error_to_string msg ; einfo
  | Ok _ -> einfo

let check_cashflow typed_cmod =
  let (param_field_tags, ctr_tags) = CF.main typed_cmod in
  let param_field_tags_to_string = List.map param_field_tags
      ~f:(fun (i, t) ->
          (i, CF.ECFR.money_tag_to_string t)) in
  let ctr_tags_to_string = List.map ctr_tags
      ~f:(fun (adt, ctrs) ->
          (adt, List.map ctrs
             ~f:(fun (i, ts) ->
                 (i, List.map ts ~f:CF.ECFR.money_tag_to_string)))) in
  (param_field_tags_to_string, ctr_tags_to_string)

let check_version vernum =
  let (mver, _, _) = scilla_version in
  if vernum <> mver
  then
    let emsg =  sprintf "Scilla version mismatch. Expected %d vs Contract %d\n" mver vernum in
    let err = scilla_error_to_string (mk_error0 emsg) in
    (perr err; exit 1)

(* Check a library module. *)
let check_lmodule file =
  let r = (
    let%bind (lmod : ParsedSyntax.lmodule) = check_parsing file ScillaParser.lmodule in
    let elibs = import_libs lmod.elibs in
    let%bind (recursion_lmod, recursion_rec_principles, recursion_elibs) = 
      check_recursion_lmod lmod elibs in
    let%bind (typed_lmod, typed_elibs, typed_rlibs) = 
      check_typing_lmod recursion_lmod recursion_rec_principles recursion_elibs in
    pure (typed_lmod, typed_elibs, typed_rlibs)
  ) in
  (match r with
  | Error _ -> exit 1; (* we've already printed the error(s). *)
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
    let elibs = import_libs cmod.elibs  in
    let%bind (recursion_cmod, recursion_rec_principles, recursion_elibs) = check_recursion cmod elibs in
    let%bind (typed_cmod, tenv, typed_elibs, typed_rlibs) = check_typing recursion_cmod recursion_rec_principles recursion_elibs  in
    let%bind pm_checked_cmod = check_patterns typed_cmod  in
    let _ = if cli.cf_flag then check_accepts typed_cmod else () in
    let%bind _ = check_sanity typed_cmod typed_rlibs typed_elibs in
    let%bind event_info = check_events_info (EI.event_info pm_checked_cmod)  in
    let%bind cf_info_opt = if cli.cf_flag then pure @@ Some (check_cashflow typed_cmod) else pure @@ None in
    pure @@ (cmod, tenv, event_info, cf_info_opt)
  ) in
  (match r with
  | Error _ -> exit 1; (* we've already printed the error(s). *)
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
      pout (sprintf "%s\n" (Yojson.pretty_to_string j));)

let () =
    let cli = parse_cli () in
    let open GlobalConfig in

    StdlibTracker.add_stdlib_dirs cli.stdlib_dirs;
    let file_extn = Caml.Filename.extension cli.input_file in
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
      check_lmodule cli.input_file
    else if file_extn <> StdlibTracker.file_extn_contract
    then
      (perr (sprintf "Unknown file extension %s\n" file_extn); exit 1)
    else
      (* Check contract modules. *)
      check_cmodule cli
 
