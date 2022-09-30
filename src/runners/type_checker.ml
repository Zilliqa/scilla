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

open Core
open Printf
open Scilla_base
open Literal
open ParserUtil
open TypeUtil
open RunnerUtil
open DebugMessage
open MonadUtil
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
module RC = Recursion.ScillaRecursion (PSRep) (PERep)
module RCSRep = RC.OutputSRep
module RCERep = RC.OutputERep
module TC = TypeChecker.ScillaTypechecker (RCSRep) (RCERep)
module TCSRep = TC.OutputSRep
module TCERep = TC.OutputERep
module PM_Checker = ScillaPatternchecker (TCSRep) (TCERep)
module TI = ScillaTypeInfo (TCSRep) (TCERep)
module GUA_Checker = ScillaGUA (TCSRep) (TCERep)
open TypeCheckerUtil

(* Check that the expression parses *)
let check_parsing filename =
  match FEParser.parse_file Parser.Incremental.exp_term filename with
  | Error _ -> fail0 ~kind:"Failed to parse input file" ~inst:filename
  | Ok e ->
      plog
      @@ sprintf "\n[Parsing]:\nExpression in [%s] is successfully parsed.\n"
           filename;
      pure e

(* Check that the expression parses *)
let run () =
  GlobalConfig.reset ();
  ErrorUtils.reset_warnings ();
  Datatypes.DataTypeDictionary.reinit ();
  let cli = parse_cli None ~exe_name:Sys.(get_argv ()).(0) in
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
          let rlibs, elibs, e =
            match check_recursion dis_e std_lib with
            | Ok (rlibs, elibs, e) -> (rlibs, elibs, e)
            | Error s -> fatal_error s
          in
          match check_typing e elibs rlibs gas_limit with
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
                      (if cli.p_type_info then
                       ( "type_info",
                         JSON.TypeInfo.type_info_to_json
                           (TI.type_info_expr typed_erep) )
                       :: tj
                      else tj)
                  in
                  pout (sprintf "%s\n" (Yojson.Basic.pretty_to_string output_j));
                  if cli.gua_flag then
                    match analyze_gas typed_erep with
                    | Ok _ -> ()
                    | Error el -> fatal_error el)
              | Error el -> fatal_error el)
          | Error ((_, el), _remaining_gas) -> fatal_error el)
      | Error e -> fatal_error e)
  | Error e -> fatal_error e

let () = try run () with FatalError msg -> exit_with_error msg
