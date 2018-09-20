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
let check_parsing ctr = 
    let parse_module =
      FrontEndParser.parse_file ScillaParser.cmodule ctr in
    match parse_module with
    | None -> fail (sprintf "%s\n" "Failed to parse input file.")
    | Some cmod -> 
        plog @@ sprintf
          "\n[Parsing]:\nContract module [%s] is successfully parsed.\n" ctr;
        pure cmod

(* Type check the contract with external libraries *)
let check_typing cmod elibs =
  let open TC in
  let res = type_module cmod recursion_principles elibs in
  match res with
  | Error msg -> pout @@ sprintf "\n%s\n\n" msg; res
  | Ok typed_module -> pure @@ typed_module

let check_patterns e =
  let res = PMC.pm_check_module e in
  match res with
  | Error msg -> pout @@ sprintf "\n%s\n\n" msg; res
  | Ok pm_checked_module -> pure @@ pm_checked_module

let check_sanity c =
  let res = SC.contr_sanity c in
  match res with
  | Error msg -> pout @@ sprintf "\n%s\n\n" msg; res
  | Ok _ -> pure ()

let check_events_info einfo =
  match einfo with
  | Error msg -> pout @@ sprintf "\n%s\n\n" msg; einfo
  | Ok _ -> einfo

let () =
  if (Array.length Sys.argv) < 2
  then
    (perr (sprintf "Usage: %s foo.scilla\n" Sys.argv.(0))
    )
  else (
    let open GlobalConfig in
    set_debug_level Debug_None;
    (* Testsuite runs this executable with cwd=tests and ends
       up complaining about missing _build directory for logger.
       So disable the logger. *)
    let r = (
      let%bind cmod = check_parsing Sys.argv.(1) in
      (* This is an auxiliary executable, it's second argument must
       * have a list of stdlib dirs, so note that down. *)
      add_cmd_stdlib();
      (* Get list of stdlib dirs. *)
      let lib_dirs = StdlibTracker.get_stdlib_dirs() in
      if lib_dirs = [] then stdlib_not_found_err ();
      (* Import whatever libs we want. *)
      let elibs = import_libs cmod.elibs in
      let%bind (typed_cmod, tenv) = check_typing cmod elibs in
      let%bind pm_checked_cmod = check_patterns typed_cmod in
      let%bind _ = check_sanity pm_checked_cmod.contr in
      let%bind event_info = check_events_info @@ EI.event_info pm_checked_cmod.contr in
      pure @@ (cmod, tenv, event_info)
    ) in
    match r with
    | Error _ -> ()
    | Ok (cmod, _, event_info) ->
      pout (sprintf "%s\n" (JSON.ContractInfo.get_string cmod.contr event_info));
  )
