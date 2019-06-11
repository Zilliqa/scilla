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

(* PART A: Test literal type checks. The only way to have malformed
 * literals is by constructing them ourselves as there are checks
 * in both Scilla source parser and the JSON parser against
 * building bad literals. *)
open Syntax
open PrimTypes
open PrettyPrinters

module TestTypeUtils = TypeUtil.TypeUtilities
open TestTypeUtils
    
(* Given a literal "l", return a test that will assert that
 * the literal is malformed. Do not pass good literals. *)
let make_bad_lit_test l =
  let is_invalid_literal l =
    let v = is_wellformed_lit l in
    match v with
    | Error _ -> true
    | Ok _ -> false
  in
  let err_msg l =
    Printf.sprintf "Malformed literal %s did not fail consistency check" (pp_literal l)
  in
  test_case (fun _ ->
    assert_bool (err_msg l) (is_invalid_literal l))

exception IntBuilderInTestsuite of string
let int_builder w s =
  BatOption.get (build_prim_literal (Int_typ w) s)


(* k/v types should match declared map type. *)
let t1 = 
  (* declared type = (Int32, Int32) *)
  let mt = (int32_typ, int32_typ) in
  (* value type = (Int32, Int64) *)
  let kv = Caml.Hashtbl.create 1 in
  let _ = Caml.Hashtbl.replace kv (int_builder Bits32 "1") (int_builder Bits64 "2") in
  let l = Map (mt, kv) in
    make_bad_lit_test l

(* Map's key type can only be a primitive type. *)
let t2 =
  (* declared type = (Map(Int32, Int32), Int32) *)
  let mt = (map_typ int32_typ int32_typ, int32_typ) in
  let mt' = (int32_typ, int32_typ) in
  let m1 = Caml.Hashtbl.create 1 in
  let _ = Caml.Hashtbl.replace m1 (int_builder Bits32 "1") (int_builder Bits32 "2") in
  let l' =  Map (mt', m1) in
  let m2 = Caml.Hashtbl.create 1 in
  (* The key for m2 is being set to another Map, non-primitive. *)
  let _ = Caml.Hashtbl.replace m2 l' (int_builder Bits32 "3") in
  let l = Map (mt, m2) in
    make_bad_lit_test l

(* Bool ADT with some arg. *)
let t3 =
  let badt = ADTValue ("False", [], [(int_builder Bits32 "1")]) in
  make_bad_lit_test badt

(* Bool ADT with some type. *)
let t4 =
  let badt = ADTValue ("False", [int32_typ], [(int_builder Bits32 "1")]) in
  make_bad_lit_test badt

(* Malformed Option ADT. *)
let t5 =
  let bado = ADTValue ("Some", [int32_typ], [(int_builder Bits64 "1")]) in
  make_bad_lit_test bado

(* Malformed Option ADT. *)
let t6 =
  let bado = ADTValue ("Some", [int32_typ;int32_typ], [(int_builder Bits32 "1")]) in
  make_bad_lit_test bado

(* Malformed List *)
let t7 =
  let badl = ADTValue ("Nil", [], []) in
  make_bad_lit_test badl

(* Malformed List *)
let t8 =
  (* l1 is malformed. *)
  let l1 = ADTValue ("Nil", [], []) in
  let l2 = ADTValue ("Cons", [int32_typ], [(int_builder Bits32 "1");l1]) in
  make_bad_lit_test l2

(* Malformed List *)
let t9 =
  (* l2 should have a second arg. *)
  let l2 = ADTValue ("Cons", [int32_typ], [(int_builder Bits32 "1")]) in
  make_bad_lit_test l2

(* Malformed List *)
let t10 =
  let l1 = ADTValue ("Nil", [int32_typ], []) in
  (* l2 should have Int32 as first arg and l1 as second arg. *)
  let l2 = ADTValue ("Cons", [int32_typ], [l1]) in
  make_bad_lit_test l2

(* Malformed List *)
let t11 =
  (* l1 has different type compared to l2 *)
  let l1 = ADTValue ("Nil", [int64_typ], []) in
  let l2 = ADTValue ("Cons", [int32_typ], [(int_builder Bits32 "1");l1]) in
  make_bad_lit_test l2

let lit_typ_tests = "literal_type_tests" >::: [t1;t2;t3;t4;t5;t6;t7;t8;t9;t10;t11]

(* PART B: Regular tests based on diffing outputs. *)
module Tests = TestUtil.DiffBasedTests(
  struct
    let gold_path dir f = [dir; "typecheck"; "bad"; "gold"; f ^ ".gold" ]
    let test_path f = ["typecheck"; "bad"; f]
    let runner = "type-checker"      
    let custom_args = []
    let additional_libdirs = []
    let tests = [
      "adt-error1.scilexp";
      "branch-mismatch.scilexp";
      "builtin-tvar.scilexp";
      "builtin-bech32-1.scilexp";
      "bad-map-key.scilexp";
      "bad-map-key2.scilexp";
      "fun2.scilexp";
      "fun3.scilexp";
      "fun4.scilexp";
      "foo-expr.scilexp";
      "list-error.scilexp";
      "list-error2.scilexp";
      "list-lit.scilexp";
      "list-lit2.scilexp";
      "pm-error1.scilexp";
      "pm-error2.scilexp";
      "map-error.scilexp";
      "map-lit.scilexp";
      "nth-error.scilexp";
      "folder-error.scilexp";
      "some.scilexp";
      "substr.scilexp";
      "parsing_hex_literals.scilexp";
      "bad_assign.scilexp";
      "bad_assign2.scilexp";
      "bad_match.scilexp";
      "let-error.scilexp";
    ]
    let exit_code : Unix.process_status = WEXITED 1
  end)

let all_tests env = "type_check_fail_tests" >::: [lit_typ_tests;Tests.add_tests env]
