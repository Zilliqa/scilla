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
open OUnit2
open Scilla_base
open Literal
open Syntax
open ErrorUtils

(* Use GlobalLiteral for compatibility with TypeUtil *)
module FEParser = FrontEndParser.ScillaFrontEndParser (GlobalLiteral)
module TestTypeUtils = TypeUtil.TypeUtilities
module TestTypeType = TypeUtil.TUType

let make_type_assignable_equiv_test st1 st2 eq f_name f =
  let open FEParser in
  let t1, t2 =
    match (parse_type st1, parse_type st2) with
    | Ok t1, Ok t2 -> (t1, t2)
    | _ ->
        raise
          (SyntaxError
             ( "Error parsing types " ^ st1 ^ " and " ^ st2 ^ " in " ^ f_name
               ^ " tests",
               dummy_loc ))
  in
  let b, bs = if eq then (f t1 t2, "=") else (not (f t1 t2), "<>") in
  let err_msg =
    "Assert " ^ TestTypeType.pp_typ t1 ^ " " ^ bs ^ " " ^ TestTypeType.pp_typ t2
    ^ " test failed"
  in
  test_case (fun _ -> assert_bool err_msg b)

let make_type_assignable_test st1 st2 eq =
  make_type_assignable_equiv_test st1 st2 eq "type_assignable"
    (fun expected actual -> TestTypeType.type_assignable ~expected ~actual)

let make_all_type_assignable_tests tlist =
  List.map tlist ~f:(fun (st1, st2, eq) -> make_type_assignable_test st1 st2 eq)

let make_type_equiv_test st1 st2 eq =
  make_type_assignable_equiv_test st1 st2 eq "type_equiv"
    TestTypeType.type_equivalent

let make_all_type_equiv_tests tlist =
  List.map tlist ~f:(fun (st1, st2, eq) -> make_type_equiv_test st1 st2 eq)

let equivalent_types =
  [
    ("Uint32", "Uint32");
    ("forall 'A. List ('A) -> List ('A)", "forall 'B. List ('B) -> List ('B)");
    ( "forall 'A. forall 'B. ('B -> 'A -> 'B) -> 'B -> List ('A) -> 'B",
      "forall 'B. forall 'A. ('A -> 'B -> 'A) -> 'A -> List ('B) -> 'A" );
    ( "forall 'A. 'A -> forall 'B. List ('B)",
      "forall 'B. 'B -> forall 'A. List ('A)" );
    ( "forall 'A. 'A -> (forall 'A. List ('A)) -> 'A",
      "forall 'B. 'B -> (forall 'C. List ('C)) -> 'B" );
    ( "forall 'A. 'A -> (forall 'A. List ('A)) -> 'B",
      "forall 'C. 'C -> (forall 'C. List ('C)) -> 'B" );
    (* Addresses *)
    ("ByStr20", "ByStr20");
    ("ByStr20 with end", "ByStr20 with end");
    ("ByStr20 with library end", "ByStr20 with library end");
    ("ByStr20 with _codehash end", "ByStr20 with _codehash end");
    ("ByStr20 with contract end", "ByStr20 with contract end");
    ( "ByStr20 with contract field x : Uint32 end",
      "ByStr20 with contract field x : Uint32 end" );
    ( "ByStr20 with contract field x : Uint32, field y : Bool end",
      "ByStr20 with contract field x : Uint32, field y : Bool end" );
    ( "ByStr20 with contract field y : Bool, field x : Uint32 end",
      "ByStr20 with contract field x : Uint32, field y : Bool end" );
    ( "ByStr20 with contract field x : Uint32, field y : ByStr20 with end end",
      "ByStr20 with contract field x : Uint32, field y : ByStr20 with end end"
    );
    ( "ByStr20 with contract field x : Uint32, field y : ByStr20 with contract \
       end end",
      "ByStr20 with contract field x : Uint32, field y : ByStr20 with contract \
       end end" );
    ( "ByStr20 with contract field x : Uint32, field y : ByStr20 with contract \
       field y2 : ByStr20, field y1 : Option Int256 end end",
      "ByStr20 with contract field x : Uint32, field y : ByStr20 with contract \
       field y1 : Option Int256, field y2 : ByStr20 end end" );
    ( "forall 'A. 'A -> ByStr20 with contract field f : 'A end",
      "forall 'C. 'C -> ByStr20 with contract field f : 'C end" );
    ( "forall 'A. 'A -> ByStr20 with contract field f : ByStr20 with contract \
       field z : 'A end end",
      "forall 'C. 'C -> ByStr20 with contract field f : ByStr20 with contract \
       field z : 'C end end" );
    ( "ByStr20 with contract field x : Uint32, field y : Bool end -> ByStr20 \
       with contract field y : Bool end",
      "ByStr20 with contract field x : Uint32, field y : Bool end -> ByStr20 \
       with contract field y : Bool end" );
    ( "ByStr20 with end -> ByStr20 with contract field y : Bool end",
      "ByStr20 with end -> ByStr20 with contract field y : Bool end" );
    ( "ByStr20 with contract end -> ByStr20 with contract field y : Bool end",
      "ByStr20 with contract end -> ByStr20 with contract field y : Bool end" );
    ( "ByStr20 -> ByStr20 with contract field y : Bool end",
      "ByStr20 -> ByStr20 with contract field y : Bool end" );
    ( "ByStr20 with contract field y : Bool end -> ByStr20 with end",
      "ByStr20 with contract field y : Bool end -> ByStr20 with end" );
    ( "ByStr20 with contract field y : Bool end -> ByStr20 with contract end",
      "ByStr20 with contract field y : Bool end -> ByStr20 with contract end" );
    ( "ByStr20 with contract field y : Bool end -> ByStr20",
      "ByStr20 with contract field y : Bool end -> ByStr20" );
    ( "Map (ByStr20 with contract field x : Uint32, field y : ByStr20 with end \
       end) ByStr20",
      "Map (ByStr20 with contract field x : Uint32, field y : ByStr20 with end \
       end) ByStr20" );
    ( "Map (ByStr20 with contract end) (ByStr20 with contract field x : Uint32 \
       end)",
      "Map (ByStr20 with contract end) (ByStr20 with contract field x : Uint32 \
       end)" );
    ( "Map ByStr20 (ByStr20 with contract field x : Uint32 end)",
      "Map ByStr20 (ByStr20 with contract field x : Uint32 end)" );
  ]

let assignable_but_not_equivalent_types =
  [
    (* Addresses *)
    ("ByStr20", "ByStr20 with end");
    ("ByStr20", "ByStr20 with library end");
    ("ByStr20", "ByStr20 with _codehash end");
    ("ByStr20 with _codehash end", "ByStr20 with library end");
    ("ByStr20 with _codehash end", "ByStr20 with contract end");
    ("ByStr20", "ByStr20 with contract end");
    ("ByStr20 with end", "ByStr20 with contract end");
    ("ByStr20 with end", "ByStr20 with contract field x : Uint32 end");
    ("ByStr20 with contract end", "ByStr20 with contract field x : Uint32 end");
    ( "ByStr20 with contract field x : Uint32 end",
      "ByStr20 with contract field x : Uint32, field y : Uint32 end" );
    ( "ByStr20 with contract field x : Uint32 end",
      "ByStr20 with contract field x : Uint32, field y : Uint32, field z : \
       ByStr20 with end end" );
    ( "ByStr20 with contract field y : Uint32, field x : Uint32 end",
      "ByStr20 with contract field x : Uint32, field y : Uint32, field z : \
       ByStr20 with end end" );
    ( "ByStr20 with contract field x : Uint32, field y : ByStr20 with contract \
       field y1 : Int32 end end",
      "ByStr20 with contract field x : Uint32, field y : ByStr20 with contract \
       field y2 : Bool, field y1 : Int32 end end" );
    ( "ByStr20 with contract field x : Uint32, field y : ByStr20 with contract \
       end end",
      "ByStr20 with contract field x : Uint32, field y : ByStr20 with contract \
       field y2 : Bool, field y1 : Int32 end end" );
    ( "ByStr20 with contract field x : ByStr20 with end end",
      "ByStr20 with contract field x : ByStr20 with contract end end" );
    ( "forall 'A. 'A -> ByStr20",
      "forall 'C. 'C -> ByStr20 with contract field f : 'C end" );
    ( "forall 'A. 'A -> ByStr20 with end",
      "forall 'C. 'C -> ByStr20 with contract field f : 'C end" );
    ( "forall 'A. 'A -> ByStr20 with contract end",
      "forall 'C. 'C -> ByStr20 with contract field f : 'C end" );
    ( "forall 'A. 'A -> ByStr20 with contract field f : ByStr20 end",
      "forall 'C. 'C -> ByStr20 with contract field f : ByStr20 with contract \
       field z : 'C end end" );
    ( "forall 'A. 'A -> ByStr20 with contract field f : ByStr20 with end end",
      "forall 'C. 'C -> ByStr20 with contract field f : ByStr20 with contract \
       field z : 'C end end" );
    ( "forall 'A. 'A -> ByStr20 with contract field f : ByStr20 with contract \
       end end",
      "forall 'C. 'C -> ByStr20 with contract field f : ByStr20 with contract \
       field z : 'C end end" );
    ( "ByStr20 with contract field x : Uint32, field y : Bool end -> ByStr20 \
       with contract field y : Bool end",
      "ByStr20 with contract field x : Uint32 end -> ByStr20 with contract \
       field y : Bool end" );
    ( "ByStr20 with contract field x : Uint32, field y : Bool end -> ByStr20 \
       with contract field y : Bool end",
      "ByStr20 with contract end -> ByStr20 with contract field y : Bool end" );
    ( "ByStr20 with contract field x : Uint32, field y : Bool end -> ByStr20 \
       with contract field y : Bool end",
      "ByStr20 with end -> ByStr20 with contract field y : Bool end" );
    ( "ByStr20 with contract field x : Uint32, field y : Bool end -> ByStr20 \
       with contract field y : Bool end",
      "ByStr20 -> ByStr20 with contract field y : Bool end" );
    ( "ByStr20 with contract field x : Uint32, field y : Bool end -> ByStr20 \
       with contract field y : Bool end",
      "ByStr20 with end -> ByStr20 with contract field y : Bool, field z : \
       Uint32 end" );
    ( "ByStr20 with contract field x : Uint32, field y : Bool end -> ByStr20 \
       with contract field y : Bool end",
      "ByStr20 with contract field x : Uint32, field y : Bool end -> ByStr20 \
       with contract field y : Bool, field z : Uint32 end" );
    ( "ByStr20 with contract field x : Uint32, field y : Bool end -> ByStr20 \
       with contract end",
      "ByStr20 with contract field x : Uint32, field y : Bool end -> ByStr20 \
       with contract field y : Bool, field z : Uint32 end" );
    ( "ByStr20 with contract field x : Uint32, field y : Bool end -> ByStr20 \
       with end",
      "ByStr20 with contract field x : Uint32, field y : Bool end -> ByStr20 \
       with contract field y : Bool, field z : Uint32 end" );
    ( "ByStr20 with contract field x : Uint32, field y : Bool end -> ByStr20",
      "ByStr20 with contract field x : Uint32, field y : Bool end -> ByStr20 \
       with contract field y : Bool, field z : Uint32 end" );
    ( "Map (ByStr20 with end) (ByStr20 with contract field x : Uint32 end)",
      "Map (ByStr20 with contract end) (ByStr20 with contract field x : Uint32 \
       end)" );
    ( "Map (ByStr20 with contract end) (ByStr20 with contract end)",
      "Map (ByStr20 with contract end) (ByStr20 with contract field x : Uint32 \
       end)" );
  ]

let not_assignable_in_either_direction_types =
  [
    ("Int32", "Uint32");
    ( "forall 'A. List ('A) -> List ('A)",
      "forall 'A. List ('A) -> List ('A) -> List ('A)" );
    ( "forall 'A. forall 'B. ('B -> 'A -> 'B) -> 'B -> List ('A) -> 'B",
      "forall 'B. forall 'A. ('B -> 'A -> 'B) -> 'B -> List ('A) -> 'B" );
    ( "forall 'A. 'A -> (forall 'A. List ('A)) -> 'B",
      "forall 'B. 'B -> (forall 'C. List ('C)) -> 'B" );
    (* Addresses *)
    ("ByStr20 with library end", "ByStr20 with contract end");
    ( "ByStr20 with contract field x : Int32 end",
      "ByStr20 with contract field x : Uint32 end" );
    ( "ByStr20 with contract field x : Int32 end",
      "ByStr20 with contract field y : Int32 end" );
    ( "ByStr20 with contract field x : Int32, field z : Uint32 end",
      "ByStr20 with contract field y : Int32, field w : Uint32 end" );
    ( "ByStr20 with contract field x : ByStr20 with contract field y1 : Int32 \
       end end",
      "ByStr20 with contract field x : ByStr20 with contract field y1 : Uint32 \
       end end" );
    ( "ByStr20 with contract field x : ByStr20 with contract field y1 : Int32 \
       end end",
      "ByStr20 with contract field x : ByStr20 with contract field y2 : Int32 \
       end end" );
    ( "forall 'A. 'A -> ByStr20 with contract field f : Uint32 end",
      "forall 'C. 'C -> ByStr20 with contract field f : 'C end" );
    ( "forall 'A. forall 'B. ByStr20 with contract field f : 'A, field g : 'B \
       end",
      "forall 'C. forall 'D. ByStr20 with contract field f : 'D, field g : 'C \
       end" );
    ( "forall 'A. forall 'B. 'A -> 'B -> ByStr20 with contract field f : 'A, \
       field g : 'B end",
      "forall 'C. forall 'D. 'D -> 'C -> ByStr20 with contract field f : 'C, \
       field g : 'D end" );
    ( "ByStr20 with contract field x : Uint32, field y : Bool end -> ByStr20 \
       with contract field y : Bool end",
      "ByStr20 with contract field x : Uint32, field z : Bool end -> ByStr20 \
       with contract field y : Bool end" );
    ( "ByStr20 with contract field x : Uint32 end -> ByStr20 with contract \
       field y : Bool, field z : Bool end",
      "ByStr20 with contract field x : Uint32 end -> ByStr20 with contract \
       field y : Bool, field w : Bool  end" );
    ( "ByStr20 with contract end -> ByStr20 with contract end",
      "ByStr20 with contract field x : Uint32 end -> ByStr20 with contract \
       field y : Bool end" );
    ( "ByStr20 with contract field x : Uint32 end -> ByStr20 with contract \
       field y : Bool end",
      "ByStr20 with contract end -> ByStr20 with contract end" );
    ( "Map (ByStr20 with contract end) (ByStr20 with contract field x : Uint32 \
       end)",
      "Map (ByStr20 with contract field x : Uint32 end) (ByStr20 with contract \
       end)" );
    ( "Map (ByStr20 with contract end) (ByStr20 with contract field y : Uint32 \
       end)",
      "Map (ByStr20 with contract end) (ByStr20 with contract field x : Uint32 \
       end)" );
    ( "Map (ByStr20 with contract field y : Uint32 end) (ByStr20 with contract \
       end)",
      "Map (ByStr20 with contract field x : Uint32 end) (ByStr20 with contract \
       end)" );
  ]

let make_test eq (t1, t2) = (t1, t2, eq)
let reverse_test eq (t1, t2) = (t2, t1, eq)

let all_type_equiv_tests =
  (* Equivalent types *)
  List.map equivalent_types ~f:(make_test true)
  (* Equivalence should be reflexive *)
  @ List.map equivalent_types ~f:(reverse_test true)
  (* Assignable but not equivalent *)
  @ List.map assignable_but_not_equivalent_types ~f:(make_test false)
  (* Non-equivalence is reflexive  *)
  @ List.map assignable_but_not_equivalent_types ~f:(reverse_test false)
  (* Non-assignable implies non-equivalence *)
  @ List.map not_assignable_in_either_direction_types ~f:(make_test false)
  (* Non-equivalence is reflexive *)
  @ List.map not_assignable_in_either_direction_types ~f:(reverse_test false)

let all_type_assignable_tests =
  (* Equivalence implies assignability *)
  List.map equivalent_types ~f:(make_test true)
  (* Equivalence should be reflexive, and implies assignability *)
  @ List.map equivalent_types ~f:(reverse_test true)
  (* Assignable but not equivalent *)
  @ List.map assignable_but_not_equivalent_types ~f:(make_test true)
  (* Intersection of assignable and non-equivalt is non-reflexive  *)
  @ List.map assignable_but_not_equivalent_types ~f:(reverse_test false)
  (* Non-assignable *)
  @ List.map not_assignable_in_either_direction_types ~f:(make_test false)
  (* Non-assignable and non-equivalent is reflexive
     - if it becomes assignable, then it should be place in assignable_but_not_equivalent_types. *)
  @ List.map not_assignable_in_either_direction_types ~f:(reverse_test false)

let make_map_access_type_test t at nindices =
  let open FEParser in
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
          let b = [%equal: TestTypeType.t] at' at_computed' in
          assert_bool
            (Printf.sprintf
               "Failed map_access_type test for %s[%d]. Expected %s, but got %s.\n"
               t nindices at
               (TestTypeType.pp_typ at_computed'))
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
  List.map tlist ~f:(fun (t, at, nindices) ->
      make_map_access_type_test t at nindices)

let make_ground_type_test ts exp_bool =
  let open FEParser in
  let open TestTypeUtils in
  let t =
    match parse_type ts with
    | Ok t -> t
    | _ ->
        raise
          (SyntaxError
             ("Error parsing type " ^ ts ^ " in type_equiv tests", dummy_loc))
  in
  test_case (fun _ ->
      let b = is_ground_type t in
      assert_bool "TypeUtil: is_ground_type test failed on type"
        Bool.(b = exp_bool))

let ground_type_tests =
  [
    ("'A", false);
    ("Uint32", true);
    ("Uint32 -> 'A", false);
    ("forall 'A. List ('A) -> List ('A)", false);
    ("List ('A)", false);
    ("forall 'A. Map Int32 Uint32", false);
    ("forall 'A. Pair Int32 'A", false);
  ]

let all_ground_type_tests =
  List.map ground_type_tests ~f:(fun (t, res) -> make_ground_type_test t res)

let type_equiv_tests =
  "type_equiv_tests" >::: make_all_type_equiv_tests all_type_equiv_tests

let type_assignable_tests =
  "type_assignable_tests"
  >::: make_all_type_assignable_tests all_type_assignable_tests

let map_access_type_tests =
  "map_access_type_tests" >::: make_map_access_type_tests map_access_type_tests

let ground_type_tests = "ground_type_tests" >::: all_ground_type_tests

module Tests = Scilla_test.Util.DiffBasedTests (struct
  let gold_path dir f = [ dir; "typecheck"; "good"; "gold"; f ^ ".gold" ]
  let test_path f = [ "typecheck"; "good"; f ]
  let runner = "type-checker"
  let ignore_predef_args = false
  let json_errors = true
  let gas_limit = Stdint.Uint64.of_int 4002000
  let custom_args = [ "-typeinfo" ]
  let additional_libdirs = []
  let provide_init_arg = false
  let diff_filter s = s

  let tests =
    [
      "ackermann.scilexp";
      "branch-match.scilexp";
      "builtin-strings.scilexp";
      "builtin-bech32-1.scilexp";
      "builtin-bech32-2.scilexp";
      "builtin-alt-bn128.scilexp";
      "builtin-isqrt.scilexp";
      "builtin-eq-bystr.scilexp";
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
      "pm5.scilexp";
      "pair.scilexp";
      "subst.scilexp";
      "nat_to_int.scilexp";
      "to_int.scilexp";
      "type-subst-avoids-capture-1.scilexp";
      "type-subst-avoids-capture-2.scilexp";
      "zip.scilexp";
      "str-nonprint-char-1.scilexp";
      "map_value_type_pair.scilexp";
      "to_bystr.scilexp";
      "crypto-neg.scilexp";
      "crypto-bmul.scilexp";
    ]

  let exit_code : UnixLabels.process_status = WEXITED 0
end)

let tests env =
  "good"
  >::: [
         type_equiv_tests;
         type_assignable_tests;
         Tests.tests env;
         ground_type_tests;
         map_access_type_tests;
       ]
