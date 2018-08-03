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

module SimpleTEnv = MakeTEnv(PlainTypes)
open SimpleTEnv


let () =
  let filename = Sys.argv.(1) in
  match FrontEndParser.parse_file ScillaParser.exps filename with
  | Some [e] ->
      let res = (
        let%bind _ = TypeChecker.type_recursion_principles in      
        let recs = List.map recursion_principles
            ~f:(fun ({lname = a; _}, c) -> (a, c)) in
        let tenv = TEnv.addTs TEnv.mk recs in
        TypeChecker.type_expr tenv e 
      ) in
      (match res with
      | Ok res ->
          printf "%s\n" (pp_typ res.tp)
      | Error s -> printf "Type checking failed:\n%s\n" s)
  | Some _ | None ->
      printf "%s\n" "Failed to parse input file."
  


