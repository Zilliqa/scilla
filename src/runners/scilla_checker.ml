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

module ParsedSyntax = ParserUtil.ParsedSyntax
module PSRep = ParserRep
module PERep = ParserRep
  
module TC = TypeChecker.ScillaTypechecker (PSRep) (PERep)
module TCSRep = TC.OutputSRep
module TCERep = TC.OutputERep

module PMC = ScillaPatternchecker (TCSRep) (TCERep)
module PMCSRep = PMC.SPR
module PMCERep = PMC.EPR

module SC = ScillaSanityChecker (PMCSRep) (PMCERep)
module EI = ScillaEventInfo (PMCSRep) (PMCERep)


(* Check that the module parses *)
let check_parsing ctr  = 
    let parse_module =
      FrontEndParser.parse_file ScillaParser.cmodule ctr in
    match parse_module with
    | None -> exit 1 (* Error is printed by the parser. *)
    | Some cmod -> 
        plog @@ sprintf
          "\n[Parsing]:\nContract module [%s] is successfully parsed.\n" ctr;
        pure cmod

(* Type check the contract with external libraries *)
let check_typing cmod elibs  =
  let open TC in
  let res = type_module cmod recursion_principles elibs in
  match res with
  | Error msgs -> pout @@ scilla_error_to_string msgs ; res
  | Ok typed_module -> pure @@ typed_module

let check_patterns e  =
  let res = PMC.pm_check_module e in
  match res with
  | Error msg -> pout @@ scilla_error_to_string msg ; res
  | Ok pm_checked_module -> pure @@ pm_checked_module

let check_sanity c  =
  let res = SC.contr_sanity c in
  match res with
  | Error msg -> pout @@ scilla_error_to_string msg ; res
  | Ok _ -> pure ()

let check_events_info einfo  =
  match einfo with
  | Error msg -> pout @@ scilla_error_to_string msg ; einfo
  | Ok _ -> einfo

let () =
    let cli = parse_cli () in
    let open GlobalConfig in
    StdlibTracker.add_stdlib_dirs cli.stdlib_dirs;
    set_debug_level Debug_None;
    (* Testsuite runs this executable with cwd=tests and ends
       up complaining about missing _build directory for logger.
       So disable the logger. *)
    let r = (
      let%bind cmod = check_parsing cli.input_file  in
      (* Get list of stdlib dirs. *)
      let lib_dirs = StdlibTracker.get_stdlib_dirs() in
      if lib_dirs = [] then stdlib_not_found_err ();
      (* Import whatever libs we want. *)
      let elibs = import_libs cmod.elibs  in
      let%bind (typed_cmod, tenv) = check_typing cmod elibs  in
      let%bind pm_checked_cmod = check_patterns typed_cmod  in
      let%bind _ = check_sanity pm_checked_cmod.contr  in
      let%bind event_info = check_events_info (EI.event_info pm_checked_cmod.contr)  in
      pure @@ (cmod, tenv, event_info)
    ) in
    match r with
    | Error el -> exit 1 (* we've already printed the error(s). *)
    | Ok (cmod, _, event_info) ->
      let j = `Assoc [
        ("contract_info", (JSON.ContractInfo.get_json cmod.contr event_info));
        ("warnings", scilla_warning_to_json (get_warnings()))
      ] in
      pout (sprintf "%s\n" (Yojson.pretty_to_string j));

