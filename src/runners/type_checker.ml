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
open Syntax
open ParserUtil
open TypeUtil
open Recursion
open RunnerUtil
open DebugMessage
open ErrorUtils
open MonadUtil
open Result.Let_syntax
open PatternChecker
open PrettyPrinters


module ParsedSyntax = ParserUtil.ParsedSyntax
module PSRep = ParserRep
module PERep = ParserRep
  
module TC = TypeChecker.ScillaTypechecker (PSRep) (PERep)
module TCSRep = TC.OutputSRep
module TCERep = TC.OutputERep

module PM_Checker = ScillaPatternchecker (TCSRep) (TCERep)

(* Check that the expression parses *)
let check_parsing filename = 
    let parse_module =
      FrontEndParser.parse_file ScillaParser.exps filename in
    match parse_module with
    | None -> fail0 (sprintf "Failed to parse input file %s\n." filename)
    | Some e ->
        plog @@ sprintf
          "\n[Parsing]:\nExpression in [%s] is successfully parsed.\n" filename;
        pure e

(* Type check the expression with external libraries *)
let check_typing e elibs =
  let open TC in
  let open TC.TypeEnv in
  let rec_lib = { ParsedSyntax.lname = asId "rec_lib" ;
                  ParsedSyntax.lentries = recursion_principles } in
  let%bind (typed_rec_libs, tenv0) = type_library TEnv.mk rec_lib in
  (* Step 1: Type check external libraries *)
  (* TODO: Cache this information unless its version changed! *)
  let%bind (_, tenv1) = MonadUtil.foldM elibs ~init:([], tenv0)
      ~f:(fun (lib_acc, env_acc) elib ->
          let%bind (lib, new_env) = type_library env_acc elib in
        pure @@ (lib_acc @ [lib], new_env)) in
  let%bind typed_e = type_expr tenv1 e in
  pure @@ typed_e

let check_patterns e = PM_Checker.pm_check_expr e
    
let () =
    let cli = parse_cli () in
    let open GlobalConfig in
    StdlibTracker.add_stdlib_dirs cli.stdlib_dirs;
    set_debug_level Debug_None;
    let pp_json = cli.json_errors in
    let filename = cli.input_file in
    match FrontEndParser.parse_file ScillaParser.exps filename with
    | Some [e] ->
        (* Get list of stdlib dirs. *)
        let lib_dirs = StdlibTracker.get_stdlib_dirs() in
        if lib_dirs = [] then stdlib_not_found_err ();
        (* Import all libs. *)
        let std_lib = import_all_libs lib_dirs in
        (match check_typing e std_lib with
         | Ok ((_, (e_typ, _)) as typed_erep) ->
             (match check_patterns typed_erep with
              | Ok _ -> printf "%s\n" (pp_typ e_typ.tp)
              | Error el -> (pout @@ scilla_error_to_string el pp_json; exit 1)
             )
         | Error el -> (pout @@ scilla_error_to_string el pp_json); exit 1)
    | Some _ | None ->
        (pout @@ scilla_error_to_string (mk_error0 "Failed to parse input file") pp_json;
        exit 1)
