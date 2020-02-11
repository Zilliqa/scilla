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
open ErrorUtils
module TestTypeUtils = TypeUtil.TypeUtilities

let make_type_assignable_equiv_tests st1 st2 eq f_name f =
  let open FrontEndParser in
  let t1, t2 = match (parse_type st1, parse_type st2) with
    | Ok t1, Ok t2 -> (t1, t2)
    | _ ->
        raise
          (SyntaxError
             ( "Error parsing types " ^ st1 ^ " and " ^ st2
               ^ " in " ^ f_name ^ " tests",
               dummy_loc ))
  in
  let b, bs =
    if eq then (f t1 t2, "=") else (not (f t1 t2), "<>")
  in
  let err_msg =
    "Assert " ^ pp_typ t1 ^ " " ^ bs ^ " " ^ pp_typ t2 ^ " test failed"
  in
  test_case (fun _ -> assert_bool err_msg b)
  
let make_type_assignable_test st1 st2 eq =
  make_type_assignable_equiv_tests st1 st2 eq "type_assignable" type_assignable

let make_type_assignable_tests tlist =
  List.map (fun (st1, st2, eq) -> make_type_assignable_test st1 st2 eq) tlist

let make_type_equiv_test st1 st2 eq =
  make_type_assignable_equiv_tests st1 st2 eq "type_equiv" type_equiv

let make_type_equiv_tests tlist =
  List.map (fun (st1, st2, eq) -> make_type_equiv_test st1 st2 eq) tlist

let all_type_equiv_tests =
  [
    ("Uint32", "Uint32", true);
    ("Int32", "Uint32", false);
    ( "forall 'A. List ('A) -> List ('A)",
      "forall 'B. List ('B) -> List ('B)",
      true );
    ( "forall 'A. List ('A) -> List ('A)",
      "forall 'A. List ('A) -> List ('A) -> List ('A)",
      false );
    ( "forall 'A. forall 'B. ('B -> 'A -> 'B) -> 'B -> List ('A) -> 'B",
      "forall 'B. forall 'A. ('B -> 'A -> 'B) -> 'B -> List ('A) -> 'B",
      false );
    ( "forall 'A. forall 'B. ('B -> 'A -> 'B) -> 'B -> List ('A) -> 'B",
      "forall 'B. forall 'A. ('A -> 'B -> 'A) -> 'A -> List ('B) -> 'A",
      true );
    ( "forall 'A. 'A -> forall 'B. List ('B)",
      "forall 'B. 'B -> forall 'A. List ('A)",
      true );
    ( "forall 'A. 'A -> (forall 'A. List ('A)) -> 'A",
      "forall 'B. 'B -> (forall 'C. List ('C)) -> 'B",
      true );
    ( "forall 'A. 'A -> (forall 'A. List ('A)) -> 'B",
      "forall 'B. 'B -> (forall 'C. List ('C)) -> 'B",
      false );
    ( "forall 'A. 'A -> (forall 'A. List ('A)) -> 'B",
      "forall 'C. 'C -> (forall 'C. List ('C)) -> 'B",
      true );
    ( "ByStr20 with x : Uint32 end",
      "ByStr20 with x : Uint32 end",
      true );
    ( "ByStr20 with x : Int32 end",
      "ByStr20 with x : Uint32 end",
      false );
    ( "ByStr20 with x : Uint32, y : Uint32 end",
      "ByStr20 with x : Uint32 end",
      false );
    ( "ByStr20 with x : Uint32 end",
      "ByStr20 with x : Uint32, y : Uint32 end",
      false );
    ( "ByStr20",
      "ByStr20 with x : Uint32 end",
      false );
    ( "ByStr20 with x : Uint32 end",
      "ByStr20",
      false );
  ]

let all_type_assignable_tests =
  [
    (* TODO: These need to be done properly. They are currently a copy of the equiv tests *)
    ("Uint32", "Uint32", true);
    ("Int32", "Uint32", false);
    ( "forall 'A. List ('A) -> List ('A)",
      "forall 'B. List ('B) -> List ('B)",
      true );
    ( "forall 'A. List ('A) -> List ('A)",
      "forall 'A. List ('A) -> List ('A) -> List ('A)",
      false );
    ( "forall 'A. forall 'B. ('B -> 'A -> 'B) -> 'B -> List ('A) -> 'B",
      "forall 'B. forall 'A. ('B -> 'A -> 'B) -> 'B -> List ('A) -> 'B",
      false );
    ( "forall 'A. forall 'B. ('B -> 'A -> 'B) -> 'B -> List ('A) -> 'B",
      "forall 'B. forall 'A. ('A -> 'B -> 'A) -> 'A -> List ('B) -> 'A",
      true );
    ( "forall 'A. 'A -> forall 'B. List ('B)",
      "forall 'B. 'B -> forall 'A. List ('A)",
      true );
    ( "forall 'A. 'A -> (forall 'A. List ('A)) -> 'A",
      "forall 'B. 'B -> (forall 'C. List ('C)) -> 'B",
      true );
    ( "forall 'A. 'A -> (forall 'A. List ('A)) -> 'B",
      "forall 'B. 'B -> (forall 'C. List ('C)) -> 'B",
      false );
    ( "forall 'A. 'A -> (forall 'A. List ('A)) -> 'B",
      "forall 'C. 'C -> (forall 'C. List ('C)) -> 'B",
      true );
  ]
  @ List.filter (fun (_, _, eq) -> eq) all_type_equiv_tests  (* Equivalence should imply assignable *)
(* TODO: Swap all true equiv tests, and run again. Also add swapped version to equiv tests *)

let make_map_access_type_test t at nindices =
  let open FrontEndParser in
  let open TestTypeUtils in
  let t', at' =
    match (parse_type t, parse_type at) with
    | Ok t', Ok at' -> (t', at')
    | _ ->
        raise
          (SyntaxError ("Error parsing type in map_access_type tests", dummy_loc))
  in
  let at_computed = map_access_type t' nindices in
  test_case (fun _ ->
      match at_computed with
      | Error _ ->
          assert_failure
            "Failed map_access_type test. map_access_type returned failure."
      | Ok at_computed' ->
          let b = type_equiv at' at_computed' in
          assert_bool
            (Printf.sprintf
               "Failed map_access_type test for %s[%d]. Expected %s, but got %s.\n"
               t nindices at (pp_typ at_computed'))
            b)

let map_access_type_tests =
  [
    (* (Test type, expected access type, number of indices) *)
    ("Map (Uint32) (Uint32)", "Map (Uint32) (Uint32)", 0);
    ("Map (Uint32) (Uint32)", "Uint32", 1);
    ( "Map (Uint32) (Map (Uint32) (Int32))",
      "Map (Uint32) (Map (Uint32) (Int32))",
      0 );
    ("Map (Uint32) (Map (Uint32) (Int32))", "Map (Uint32) (Int32)", 1);
    ("Map (Uint32) (Map (Uint32) (Int32))", "Int32", 2);
    ("Int32", "Int32", 0);
  ]

let make_map_access_type_tests tlist =
  List.map
    (fun (t, at, nindices) -> make_map_access_type_test t at nindices)
    tlist

let type_equiv_tests =
  "type_equiv_tests" >::: make_type_equiv_tests all_type_equiv_tests

let type_assignable_tests =
  "type_assignable_tests" >::: make_type_assignable_tests all_type_assignable_tests

let map_access_type_tests =
  "map_access_type_tests" >::: make_map_access_type_tests map_access_type_tests

module Tests = TestUtil.DiffBasedTests (struct
  let gold_path dir f = [ dir; "typecheck"; "good"; "gold"; f ^ ".gold" ]

  let test_path f = [ "typecheck"; "good"; f ]

  let runner = "type-checker"

  let ignore_predef_args = false

  let gas_limit = Stdint.Uint64.of_int 4002000

  let custom_args = [ "-typeinfo" ]

  let additional_libdirs = []

  let provide_init_arg = false

  let tests =
    [
      "branch-match.scilexp";
      "builtin-strings.scilexp";
      "builtin-bech32-1.scilexp";
      "builtin-bech32-2.scilexp";
      "builtin-alt-bn128.scilexp";
      "fun.scilexp";
      "fun1.scilexp";
      "addr.scilexp";
      "app.scilexp";
      "hash1.scilexp";
      "list1.scilexp";
      "pm1.scilexp";
      "pm2.scilexp";
      "pm3.scilexp";
      "pm4.scilexp";
      "pair.scilexp";
      "subst.scilexp";
      "nat_to_int.scilexp";
      "to_int.scilexp";
      "type-subst-avoids-capture-1.scilexp";
      "type-subst-avoids-capture-2.scilexp";
      "zip.scilexp";
      "str-nonprint-char-1.scilexp";
    ]

  let exit_code : Unix.process_status = WEXITED 0
end)

let all_tests env =
  "type_check_success_tests"
  >::: [
         type_equiv_tests;
         type_assignable_tests;
         Tests.all_tests env;
         map_access_type_tests;
       ]
