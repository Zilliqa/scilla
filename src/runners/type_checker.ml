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
open Result.Let_syntax
open TypeUtil
open Recursion
open RunnerUtil
open DebugMessage
open MonadUtil

module SimpleTEnv = MakeTEnv(PlainTypes)
open SimpleTEnv

(* Check that the expression parses *)
let check_parsing filename = 
    let parse_module =
      FrontEndParser.parse_file ScillaParser.exps filename in
    match parse_module with
    | None -> fail (sprintf "%s\n" "Failed to parse input file.")
    | Some e ->
        plog @@ sprintf
          "\n[Parsing]:\nContract module [%s] is successfully parsed.\n" filename;
        pure e

(* Type check the contract with external libraries *)
let check_typing e elibs =
  let%bind _ = TypeChecker.type_recursion_principles in
  let recs = List.map recursion_principles
      ~f:(fun ({lname = a; _}, c) -> (a, c)) in
  let tenv0 = TEnv.addTs TEnv.mk recs in
  (* Step 1: Type check external libraries *)
  (* TODO: Cache this information unless its version changed! *)
  let%bind tenv1 = MonadUtil.foldM elibs ~init:tenv0
      ~f:(fun acc elib -> TypeChecker.type_library acc elib) in
  TypeChecker.type_expr tenv1 e

let () =
  if (Array.length Sys.argv) < 2
  then
    (perr (sprintf "Usage: %s foo.scilla\n" Sys.argv.(0))
    )
  else (
    GlobalConfig.set_debug_level GlobalConfig.Debug_None;
    let filename = Sys.argv.(1) in
    match FrontEndParser.parse_file ScillaParser.exps filename with
    | Some [e] ->
        let std_lib = parse_stdlib (stdlib_dir()) in
        (match check_typing e std_lib with
         | Ok res ->
             printf "%s\n" (pp_typ res.tp)
         | Error s -> printf "Type checking failed:\n%s\n" s)
    | Some _ | None ->
        printf "%s\n" "Failed to parse input file.")
