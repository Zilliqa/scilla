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
open Polynomial

let t1_pn = test_case (fun _ ->
  let m1 = (2, [('A', 2);('B', 1);('A', 3)]) in
  let m2 = (2, [('A', 5);('B', 1);]) in
  let b = eq_term m1 m2 in
  assert_bool "polynomial term equality failed" b)

let t2_pn = test_case (fun _ ->
  let m1 = (2, [('A', 2);('B', 1);('A', 3)]) in
  let m2 = (3, [('A', 1);('B', 1);]) in
  let p1 = [m1;m2] in
  let n1 = (3, [('A', 1);('B', 1);]) in
  let n2 = (2, [('A', 2);('B', 1);('A', 3)]) in
  let n3 = (0, []) in
  let p2 = [n1;n2;n3] in
  let b = eq_pn p1 p2 in
  assert_bool "polynomial equality failed" b)

let t3_pn = test_case (fun _ ->
  let m1 = (2, [('A', 2);('B', 1);('A', 3)]) in
  let m2 = (3, [('B', 3);('A', 1);]) in
  let p1 = [m1;m2] in
  let n1 = (3, [('A', 1);('B', 1);]) in
  let n2 = (4, [('A', 2);('B', 1);('A', 3);('C', 0)]) in
  let n3 = (0, []) in
  let p2 = [n1;n2;n3] in
  let sum = add_pn p1 p2 in
  let sum' = [(6, [('A', 5);('B', 1)]);(3, [('A', 1);('B', 1)]);(3, [('A', 1);('B', 3)]);] in
  let b = eq_pn sum sum' in
  assert_bool "polynomial addition test failed" b)

let t4_pn = test_case (fun _ ->
  let m1 = (2, [('A', 2);('B', 1);('A', 3)]) in
  let m2 = (3, [('B', 3);('A', 1);]) in
  let p1 = [m1;m2] in
  let n1 = (3, [('A', 1);('B', 1);]) in
  let n2 = (4, [('A', 2);('B', 1);('A', 3);('C', 0)]) in
  let n3 = (0, []) in
  let p2 = [n1;n2;n3] in
  let prod = mul_pn p1 p2 in
  let prod' = [(6, [('A', 6);('B', 2)]);(8, [('A', 10);('B', 2)]);(9, [('A', 2);('B', 4)]);(12, [('A', 6);('B', 4)]);] in
  let b = eq_pn prod prod' in
  assert_bool "polynomial multiplication test failed" b)

let t5_pn = test_case (fun _ ->
  let m1 = (1, [('A', 1)]) in (* A *)
  let m2 = (1, [('B', 1)]) in (* B *)
  let p1 = [m1;m2] in (* A + B *)
  let sq = mul_pn p1 p1 in (* A^2 + 2AB + B^2 *)
  let sq' = [(1, [('A', 2)]); (1, [('B', 2)]); (2, [('A', 1);('B', 1)])] in
  let b = eq_pn sq sq' in
  assert_bool "polynomial multiplication test failed" b)

let t6_pn = test_case (fun _ ->
  (* A^2 + 2AB + B^2 *)
  let a = [(1, [('A', 2)]); (1, [('B', 2)]); (2, [('A', 1);('B', 1)])] in
  let f c = if c = 'B' then Some [(1, [('A', 2)])] else None in
  let b = expand_parameters_pn a ~f:f in
  let bexpected = [(1, [('A', 2)]); (2, [('A', 3)]); (1, [('A', 4)])] in
  let res = eq_pn b bexpected in
  assert_bool "polynomial parameter substitution test failed" res)

let t7_pn = test_case (fun _ ->
  (* A^2 + 2AB + B^2 *)
  let a = [(1, [('A', 2)]); (1, [('B', 2)]); (2, [('A', 1);('B', 1)])] in
  let f c = 
    if c = 'B' then Some [(2, [('C', 2)]); (2, [('A', 1)])] (* 2(c^2) + 2*a *)
    else if c = 'C' then Some [(3, [('A', 5);('D', 1)])] (* 3(a^5)d *)
    else None 
  in
  let b = expand_parameters_pn a ~f:f in
  (*
    let _ = Printf.fprintf stderr "\nPolynomial: %s\n" 
      (sprint_pn b ~f:(function | 'A' -> "A" | 'B' -> "B" | 'C' -> "C" | 'D' -> "D" | _ -> "X")) in
  *)
  let bexpected = [(9, [('A', 2)]); (324, [('A', 20);('D', 4)]); (108, [('A', 11);('D', 2)])] in
  let res = eq_pn b bexpected in
  assert_bool "polynomial parameter substitution test failed" res)

let t8_pn = test_case (fun _ ->
  let m1 = (2, [('A', 2);('B', 1);]) in
  let m2 = (4, [('B', 3);('A', 1);]) in
  let m3 = (11, [('B', 3);]) in
  let p1 = [m1;m2;m3] in
  let n1 = (3, [('A', 2);('B', 1);]) in
  let n2 = (3, [('A', 1);('B', 3);]) in
  let n3 = (10, [('A', 1)]) in
  let p2 = [n1;n2;n3] in
  let maxc = max_combine_pn p1 p2 in
  let maxc' = [(3, [('A', 2);('B', 1)]);(4, [('A', 1);('B', 3)]);(10, [('A', 1)]);(11, [('B', 3)])] in
  let b = eq_pn maxc maxc' in
  assert_bool "polynomial max-combine test failed" b)


(* The test to be called from Testsuite. *)
let polynomial_tests = "polynomial_tests" >::: ([t1_pn;t2_pn;t3_pn;t4_pn;t5_pn;t6_pn;t7_pn;t8_pn])
