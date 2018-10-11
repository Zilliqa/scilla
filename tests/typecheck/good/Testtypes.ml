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

open OUnit2
open Syntax
open ParserUtil

module TestTypeUtils = TypeUtil.TypeUtilities (ParserRep) (ParserRep)

let make_type_equiv_test st1 st2 eq =
  let open FrontEndParser in
  let open TestTypeUtils in
  let t1, t2 = 
    try
      parse_type st1, parse_type st2 
    with 
    | _ -> raise (SyntaxError ("Error parsing types " ^ st1 ^ " and " ^ st2 ^ " in type_equiv tests"))
  in
  let b,bs = if eq then (type_equiv t1 t2),"=" else (not (type_equiv t1 t2)),"<>" in
  let err_msg = ("Assert " ^ (pp_typ t1) ^ " " ^ bs ^ " " ^ (pp_typ t2) ^ " test failed") in
  test_case (fun _ ->
    assert_bool err_msg b)

let make_type_equiv_tests tlist =
  List.map (fun (st1, st2, eq) ->
    make_type_equiv_test st1 st2 eq) tlist

let type_equiv_tests = [
  ("Uint32", "Uint32", true);
  ("Int32", "Uint32", false);
  ("forall 'A. List ('A) -> List ('A)", "forall 'B. List ('B) -> List ('B)", true);
  ("forall 'A. List ('A) -> List ('A)", "forall 'A. List ('A) -> List ('A) -> List ('A)", false);
  ("forall 'A. forall 'B. ('B -> 'A -> 'B) -> 'B -> List ('A) -> 'B",
    "forall 'B. forall 'A. ('B -> 'A -> 'B) -> 'B -> List ('A) -> 'B", false);
  ("forall 'A. forall 'B. ('B -> 'A -> 'B) -> 'B -> List ('A) -> 'B",
    "forall 'B. forall 'A. ('A -> 'B -> 'A) -> 'A -> List ('B) -> 'A", true);
  ("forall 'A. 'A -> forall 'B. List ('B)", "forall 'B. 'B -> forall 'A. List ('A)", true);
  ("forall 'A. 'A -> (forall 'A. List ('A)) -> 'A", "forall 'B. 'B -> (forall 'C. List ('C)) -> 'B", true);
  ("forall 'A. 'A -> (forall 'A. List ('A)) -> 'B", "forall 'B. 'B -> (forall 'C. List ('C)) -> 'B", false);
  ("forall 'A. 'A -> (forall 'A. List ('A)) -> 'B", "forall 'C. 'C -> (forall 'C. List ('C)) -> 'B", true);
]

let type_equiv_tests = "type_equiv_tests" >::: (make_type_equiv_tests type_equiv_tests)

module Tests = TestUtil.DiffBasedTests(
  struct
    let gold_path dir f = [dir; "typecheck"; "good"; "gold"; f ^ ".gold" ]
    let test_path f = ["typecheck"; "good"; f]
    let runner = "type-checker"      
    let tests = [
      "branch-match.scilla";
      "fun.scilla";
      "fun1.scilla";
      "addr.scilla";
      "app.scilla";
      "list1.scilla";
      "pm1.scilla";
      "pm2.scilla";
      "pm3.scilla";
      "pm4.scilla";
      "subst.scilla";
      "nat_to_int.scilla";
      "to_int.scilla";
      "zip.scilla";
    ]
    let exit_code : Unix.process_status = WEXITED 0
  end)

let all_tests env = "type_check_success_tests" >::: [type_equiv_tests;Tests.add_tests env]
