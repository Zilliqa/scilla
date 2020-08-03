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
open ParserUtil
open Syntax
open ErrorUtils
open PrettyPrinters
open DebugMessage
open MonadUtil
open Result.Let_syntax
open RunnerUtil
open PatternChecker
open SanityChecker
open GasUseAnalysis
open RecursionPrinciples
open EventInfo
open TypeInfo
open Cashflow
open Accept
module Parser = ScillaParser.Make (ParserSyntax)
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
module TI = ScillaTypeInfo (TCSRep) (TCERep)

(* Check that the module parses *)
let check_parsing ctr syn =
  let cmod = FrontEndParser.parse_file syn ctr in
  if Result.is_ok cmod then
    plog @@ sprintf "\n[Parsing]:\n module [%s] is successfully parsed.\n" ctr;
  cmod

(* Check restrictions on inductive datatypes, and on associated recursion principles *)
let check_recursion cmod elibs =
  let open Rec in
  let res = recursion_module cmod recursion_principles elibs in
  if Result.is_ok res then
    plog
    @@ sprintf "\n[Recursion Check]:\n module [%s] is successfully checked.\n"
         (RecIdentifier.get_id cmod.contr.cname);
  res

let check_recursion_lmod lmod elibs =
  let open Rec in
  let res = recursion_lmodule lmod recursion_principles elibs in
  if Result.is_ok res then
    plog
    @@ sprintf "\n[Recursion Check]:\n lmodule [%s] is successfully checked.\n"
         (RecIdentifier.get_id lmod.libs.lname);
  res

(* Type check the contract with external libraries *)
let check_typing cmod rprin elibs gas =
  let open TC in
  let res = type_module cmod rprin elibs gas in
  let _ =
    match res with
    | Ok (_, remaining_gas) ->
        plog
        @@ sprintf "\n[Type Check]:\n module [%s] is successfully checked.\n"
             (TCIdentifier.get_id cmod.contr.cname);
        let open Stdint.Uint64 in
        plog
        @@ sprintf "Gas remaining after typechecking: %s units.\n"
             (to_string remaining_gas)
    | _ -> ()
  in
  res

(* Type check the contract with external libraries *)
let check_typing_lmod lmod rprin elibs gas =
  let open TC in
  strip_error_type
  @@
  let res = type_lmodule lmod rprin elibs gas in
  let _ =
    match res with
    | Ok (_, remaining_gas) ->
        plog
        @@ sprintf "\n[Type Check]:\n lmodule [%s] is successfully checked.\n"
             (TCIdentifier.get_id lmod.libs.lname);
        let open Stdint.Uint64 in
        plog
        @@ sprintf "Gas remaining after typechecking: %s units.\n"
             (to_string remaining_gas)
    | _ -> ()
  in
  res

let check_patterns e rlibs elibs =
  let res = PMC.pm_check_module e rlibs elibs in
  if Result.is_ok res then
    plog
    @@ sprintf "\n[Pattern Check]:\n module [%s] is successfully checked.\n"
         (PMC.PCIdentifier.get_id e.contr.cname);
  res

let check_patterns_lmodule e rlibs elibs =
  let res = PMC.pm_check_lmodule e rlibs elibs in
  if Result.is_ok res then
    plog
    @@ sprintf "\n[Pattern Check]:\n library module is successfully checked.\n";
  res

let check_sanity m rlibs elibs =
  let res = SC.contr_sanity m rlibs elibs in
  if Result.is_ok res then
    plog
    @@ sprintf "\n[Sanity Check]:\n module [%s] is successfully checked.\n"
         (SC.SCIdentifier.get_id m.contr.cname);
  res

let check_sanity_lmod m rlibs elibs =
  let res = SC.lmod_sanity m rlibs elibs in
  if Result.is_ok res then
    plog
    @@ sprintf "\n[Sanity Check]:\n module [%s] is successfully checked.\n"
         (SC.SCIdentifier.get_id m.libs.lname);
  res

let check_accepts m = AC.contr_sanity m

let analyze_print_gas cmod typed_elibs =
  let res = GUA.gua_module cmod typed_elibs in
  match res with
  | Error msg ->
      pout @@ scilla_error_to_string msg;
      res
  | Ok cpol ->
      plog
      @@ sprintf
           "\n[Gas Use Analysis]:\n module [%s] is successfully analyzed.\n"
           (GUA.GUAIdentifier.get_id cmod.contr.cname);
      let _ =
        List.iter
          ~f:(fun (i, pol) ->
            pout
            @@ sprintf "Gas use polynomial for transition %s:\n%s\n\n"
                 (GUA.GUAIdentifier.get_id i)
                 (GUA.sprint_gup pol))
          cpol
      in
      res

let check_cashflow typed_cmod token_fields =
  let param_field_tags, ctr_tags = CF.main typed_cmod token_fields in
  let param_field_tags_to_string =
    List.map param_field_tags ~f:(fun (i, t) ->
        (i, CF.ECFR.money_tag_to_string t))
  in
  let ctr_tags_to_string =
    List.map ctr_tags ~f:(fun (adt, ctrs) ->
        ( adt,
          List.map ctrs ~f:(fun (i, ts) ->
              ( i,
                List.map ts ~f:(fun t_opt ->
                    Option.value_map t_opt ~default:"_"
                      ~f:CF.ECFR.money_tag_to_string) )) ))
  in
  (param_field_tags_to_string, ctr_tags_to_string)

let check_version vernum =
  let mver, _, _ = scilla_version in
  if vernum <> mver then
    let emsg =
      sprintf "Scilla version mismatch. Expected %d vs Contract %d\n" mver
        vernum
    in
    fatal_error (mk_error0 emsg)

let wrap_error_with_gas gas res =
  match res with Ok r -> Ok r | Error e -> Error (e, gas)

let check_lmodule cli =
  let r =
    let initial_gas = cli.gas_limit in
    let%bind (lmod : ParserSyntax.lmodule) =
      wrap_error_with_gas initial_gas
      @@ check_parsing cli.input_file Parser.Incremental.lmodule
    in
    let elibs = import_libs lmod.elibs cli.init_file in
    let%bind recursion_lmod, recursion_rec_principles, recursion_elibs =
      wrap_error_with_gas initial_gas @@ check_recursion_lmod lmod elibs
    in
    let%bind (typed_lmod, typed_rlibs, typed_elibs), remaining_gas =
      check_typing_lmod recursion_lmod recursion_rec_principles recursion_elibs
        initial_gas
    in
    let%bind () =
      Result.ignore_m
      @@ wrap_error_with_gas remaining_gas
      @@ check_patterns_lmodule typed_lmod typed_rlibs typed_elibs
    in
    let%bind () =
      Result.ignore_m
      @@ wrap_error_with_gas remaining_gas
      @@ check_sanity_lmod typed_lmod typed_rlibs typed_elibs
    in
    let type_info =
      if cli.p_type_info then TI.type_info_lmod typed_lmod else []
    in
    pure ((typed_lmod, typed_rlibs, typed_elibs), type_info, remaining_gas)
  in
  match r with
  | Error (s, g) -> fatal_error_gas s g
  | Ok (_, type_info, g) ->
      let json_output =
        if cli.p_type_info then
          [ ("type_info", JSON.TypeInfo.type_info_to_json type_info) ]
        else []
      in
      if GlobalConfig.use_json_errors () || not (List.is_empty json_output) then
        let warnings_and_gas_output =
          [
            ("warnings", scilla_warning_to_json (get_warnings ()));
            ("gas_remaining", `String (Stdint.Uint64.to_string g));
          ]
          @ json_output
        in
        let j = `Assoc warnings_and_gas_output in
        sprintf "%s\n" (Yojson.Basic.pretty_to_string j)
      else
        scilla_warning_to_sstring (get_warnings ())
        ^ "\ngas_remaining: " ^ Stdint.Uint64.to_string g ^ "\n"

(* Check a contract module. *)
let check_cmodule cli =
  let r =
    let initial_gas = cli.gas_limit in
    let%bind (cmod : ParserSyntax.cmodule) =
      wrap_error_with_gas initial_gas
      @@ check_parsing cli.input_file Parser.Incremental.cmodule
    in
    (* Import whatever libs we want. *)
    let elibs = import_libs cmod.elibs cli.init_file in
    let%bind recursion_cmod, recursion_rec_principles, recursion_elibs =
      wrap_error_with_gas initial_gas @@ check_recursion cmod elibs
    in
    let%bind (typed_cmod, tenv, typed_elibs, typed_rlibs), remaining_gas =
      check_typing recursion_cmod recursion_rec_principles recursion_elibs
        initial_gas
    in
    let%bind pm_checked_cmod, _pm_checked_rlibs, _pm_checked_elibs =
      wrap_error_with_gas remaining_gas
      @@ check_patterns typed_cmod typed_rlibs typed_elibs
    in
    let _ = if cli.cf_flag then check_accepts typed_cmod else () in
    let type_info =
      if cli.p_type_info then TI.type_info_cmod typed_cmod else []
    in
    let%bind () =
      wrap_error_with_gas remaining_gas
      @@ check_sanity typed_cmod typed_rlibs typed_elibs
    in
    let%bind event_info =
      wrap_error_with_gas remaining_gas @@ EI.event_info pm_checked_cmod
    in
    let%bind () =
      Result.ignore_m
      @@
      if cli.gua_flag then
        wrap_error_with_gas remaining_gas
        @@ analyze_print_gas typed_cmod typed_elibs
      else pure []
    in
    let cf_info_opt =
      if cli.cf_flag then Some (check_cashflow typed_cmod cli.cf_token_fields)
      else None
    in
    pure @@ (cmod, tenv, event_info, type_info, cf_info_opt, remaining_gas)
  in
  match r with
  | Error (s, g) -> fatal_error_gas s g
  | Ok (cmod, _, event_info, type_info, cf_info_opt, g) ->
      check_version cmod.smver;
      let output =
        if cli.p_contract_info then
          [
            ( "contract_info",
              JSON.ContractInfo.get_json cmod.smver cmod.contr event_info );
          ]
        else []
      in
      let output =
        if cli.p_type_info then
          ("type_info", JSON.TypeInfo.type_info_to_json type_info) :: output
        else output
      in
      let output =
        match cf_info_opt with
        | None -> output
        | Some cf_info ->
            ("cashflow_tags", JSON.CashflowInfo.get_json cf_info) :: output
      in
      let output =
        (* This part only has warnings and gas_remaining, which we output as JSON
         * if either `-jsonerrors` OR if there is other JSON output. *)
        if GlobalConfig.use_json_errors () || not (List.is_empty output) then
          output
          @ [
              ("warnings", scilla_warning_to_json (get_warnings ()));
              ("gas_remaining", `String (Stdint.Uint64.to_string g));
            ]
        else output
      in
      (* We print as a JSON if `-jsonerrors` OR if there is some JSON data to display. *)
      if GlobalConfig.use_json_errors () || not (List.is_empty output) then
        let j = `Assoc output in
        sprintf "%s\n" (Yojson.Basic.pretty_to_string j)
      else
        scilla_warning_to_sstring (get_warnings ())
        ^ "\ngas_remaining: " ^ Stdint.Uint64.to_string g ^ "\n"

let run args ~exe_name =
  GlobalConfig.reset ();
  ErrorUtils.reset_warnings ();
  Datatypes.DataTypeDictionary.reinit ();
  let cli = parse_cli args ~exe_name in
  let open GlobalConfig in
  StdlibTracker.add_stdlib_dirs cli.stdlib_dirs;
  (* Get list of stdlib dirs. *)
  let lib_dirs = StdlibTracker.get_stdlib_dirs () in
  if List.is_empty lib_dirs then stdlib_not_found_err ~exe_name ();

  (* Testsuite runs this executable with cwd=tests and ends
       up complaining about missing _build directory for logger.
       So disable the logger. *)
  set_debug_level Debug_None;

  let open FilePath in
  let open StdlibTracker in
  if check_extension cli.input_file file_extn_library then
    (* Check library modules. *)
    check_lmodule cli
  else if check_extension cli.input_file file_extn_contract then
    (* Check contract modules. *)
    check_cmodule cli
  else fatal_error (mk_error0 (sprintf "Unknown file extension\n"))
