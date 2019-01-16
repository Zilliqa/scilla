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





let uint256_max_str = "115792089237316195423570985008687907853269984665640564039457584007913129639935"
let int256_max_str =  "57896044618658097711785492504343953926634992332820282019728792003956564819967"
let int256_min_str = "-57896044618658097711785492504343953926634992332820282019728792003956564819968"

module Uint256_Emu = struct
   open Big_int
   type t = big_int

  let zero = zero_big_int
  let one = unit_big_int
  let max_int = big_int_of_string uint256_max_str
  let min_int = zero

  let of_string s = big_int_of_string s
  let to_string i = string_of_big_int i

  let mod_unsigned a =
    (* modulo arithmetic for unsigned integers *)
    mod_big_int a (add_big_int max_int one)

  let add a b =
    mod_unsigned (add_big_int a b)

  let sub a b =
    mod_unsigned (sub_big_int a b)

  let mul a b =
    mod_unsigned (mult_big_int a b)

  let div a b =
    div_big_int a b

  let rem a b =
    mod_big_int a b

  let compare a b =
    compare_big_int a b

end

module Int256_Emu = struct
   open Big_int
   type t = big_int

  let zero = zero_big_int
  let one = unit_big_int
  let max_int = big_int_of_string int256_max_str
  let min_int = big_int_of_string int256_min_str

  let of_string s = big_int_of_string s
  let to_string i = string_of_big_int i

  (* take module of result of some operation so that it is in range of Int256. *)
  let mod_signed a =
    let t = mod_big_int a (add_big_int Uint256_Emu.max_int one) in
    if (compare_big_int t max_int) > 0 
    then
      (* negative *)
      sub_big_int t (add_big_int Uint256_Emu.max_int one)
    else
      t

  let add a b =
    mod_signed (add_big_int a b)

  let sub a b =
    mod_signed (sub_big_int a b)

  let mul a b =
    mod_signed (mult_big_int a b)

  let isneg a =
    sign_big_int a = -1

  let divrem x y =
    (* We want the OCaml standard for reminders, where
     * reminder has the sign of the dividend. Big_int follows
     * the convention that reminder is always positive.
     *)
    let divrem_impl x y =
      if isneg x then
        let x' = minus_big_int x in
        if isneg y then
          let y' = minus_big_int y in
          let (q, r) = quomod_big_int x' y' in
          (q, minus_big_int r)
        else
          let (q, r) = quomod_big_int x' y in
          (minus_big_int q, minus_big_int r)
      else
        if isneg y then
          let y' = minus_big_int y in
          let (q, r) = quomod_big_int x y' in
          (minus_big_int q, r)
        else
          quomod_big_int x y
    in
    (* A wrapper to ensure that divrem computation is valid. *)
    let (q, r) = divrem_impl x y in
    if compare_big_int x (add_big_int (mult_big_int y q) r) <> 0 then
      raise (Failure "div/rem fail: N != D*q + r")
    else
      (* Modulo arithmetic in stdint hides/wraps overflow, so do that.
       * (Int256 mimics stdint behaviour w.r.t overflows).
       * The motivation is "int_min / -1", where the overflowed result
       * "-int_max" wraps to "int_min". This fails the validation
       * check above, hence we do "mod_signed" after the validation.
       * See https://stackoverflow.com/a/30400252/2128804. *)
      (mod_signed q, mod_signed r)

  let div a b =
    let (q, _) = divrem a b in
      q

  let rem a b =
    let (_, r) = divrem a b in
      r

  let compare a b =
    compare_big_int a b

end

open OUnit2
open Integer256
open Stdint

(* basic tests *)
let t1_uint = test_case (fun _ ->
  let b = (Uint256.to_string Uint256.max_int) = uint256_max_str in
  assert_bool "Uint256.max_int invalid" b)

let t2_uint = test_case (fun _ ->
  let c = Uint256.of_string (Uint256.to_string Uint256.max_int) in
  let b = (compare Uint256.max_int c) = 0 in
  assert_bool "Uint256 string conversion fail" b)

let t1_int = test_case (fun _ ->
  let b = (Int256.to_string Int256.max_int) = int256_max_str in
  assert_bool "Int256.max_int invalid" b)

let t2_int = test_case (fun _ ->
  let c = Int256.of_string (Int256.to_string Int256.max_int) in
  let b = (compare Int256.max_int c) = 0 in
  assert_bool "Int256 string conversion fail" b)

let t3_int = test_case (fun _ ->
  let c = Int256.of_string (Int256.to_string Int256.min_int) in
  let b = (compare Int256.min_int c) = 0 in
  assert_bool "Int256 string conversion fail" b)

module type IntRep = sig
  type t
  val compare : t -> t -> int
  val of_string : string -> t
  val to_string : t -> string
  val add: t -> t -> t
  val sub: t -> t -> t
  val mul: t -> t -> t
  val div: t -> t -> t
  val rem: t -> t -> t
  val zero: t
  val one: t
  val min_int: t
  val max_int: t
end

(* inputs for unsigned integer operations. *)
let binary_inputs_uint =
  [
    ("0", "0");
    ("0", "1");
    ("1", "1");
    ("1", "2");
    (* (0, max_int) *)
    ("0", uint256_max_str);
    (* (max_int, max_int) *)
    (Uint256.to_string Uint256.max_int, uint256_max_str);
    (* (max_int/2, "2") *)
    ("57896044618658097711785492504343953926634992332820282019728792003956564819967", "2");
    (* (max_int/4, "5") *)
    ("28948022309329048855892746252171976963317496166410141009864396001978282409983", "5");
    (* (max_int, 1) *)
    (uint256_max_str, "1");
    (* (max_int, 0) *)
    (uint256_max_str, "0");
    (* (Uint128.max_int, Uint128.max_int) *)
    (Uint128.to_string Uint128.max_int, Uint128.to_string Uint128.max_int);
    (* (Uint128.max_int / 4, Uint128.max_int * 8) *)
    (Uint128.to_string (Uint128.div Uint128.max_int (Uint128.of_string "4")),
      Uint128.to_string (Uint128.mul Uint128.max_int (Uint128.of_string "8")));
  ]

let binary_inputs_int =
  [
    ("0", "0");
    ("0", "1");
    ("0", "-1");
    ("-2", "-3");
    ("-4", "-2");
    ("-41", "21");
    (* (0, max_int) *)
    ("0", int256_max_str);
    (* (max_int, max_int) *)
    (Int256.to_string Int256.max_int, int256_max_str);
    (* (min_int, "-1") *)
    (int256_min_str, "-1");
    (* (max_int, min_int) *)
    (Int256.to_string Int256.max_int, int256_min_str);
    (* (min_int, min_int) *)
    (Int256.to_string Int256.min_int, int256_min_str);
    (* (min_int/4, max_int/5) *)
    (Int256.to_string (Int256.div Int256.min_int (Int256.of_string "4")), 
      Int256.to_string (Int256.div Int256.max_int (Int256.of_string "5")));
    (* (min_int/411112256444332224444, max_int/566633221114444777777777777) *)
    (Int256.to_string (Int256.div Int256.min_int (Int256.of_string "411112256444332224444")), 
      Int256.to_string (Int256.div Int256.max_int (Int256.of_string "566633221114444777777777777")));
  ]

module IntTester (IR1 : IntRep) (IR2 : IntRep) = struct
(* Create a test for binary op b/w two numbers represented as strings *)
let binary_test_create a b ops = test_case (fun _ ->
  let aui = IR1.of_string a in
  let bui = IR1.of_string b in
  let auiemu = IR2.of_string a in
  let buiemu = IR2.of_string b in
  let compareIR1 a' b' =
    (* compareIR1 will return 0,1,2 for <,=,> *)
    IR1.of_string (string_of_int ((IR1.compare a' b')+1))
  in
  let compareIR2 a' b' =
    (* compareIR2 will return 0,1,2 for <,=,> *)
    IR2.of_string (string_of_int ((IR2.compare a' b')+1))
  in
  let (op1, op2) = 
    match ops with 
    | "add" -> (IR1.add, IR2.add)
    | "sub" -> (IR1.sub, IR2.sub)
    | "mul" -> (IR1.mul, IR2.mul)
    | "div" -> (IR1.div, IR2.div)
    | "rem" -> (IR1.rem, IR2.rem)
    | "compare" -> (compareIR1, compareIR2)
    | _ -> assert_failure "Invalid integer binary test"
  in
  let fail1 = ref false in
  let fail2 = ref false in
  let cui = 
    try op1 aui bui with | Division_by_zero | Failure _ -> fail1 := true; IR1.zero
  in
  let cuiemu = 
    try op2 auiemu buiemu with | Division_by_zero | Failure _ -> fail2 := true; IR2.zero
  in
  let str1 = IR1.to_string cui in
  let str2 = IR2.to_string cuiemu in
  let fail_str = "IntTester: (" ^ ops ^ " " ^ a ^ " " ^ b ^ ") fail: ("
   ^ str1 ^ "," ^ string_of_bool !fail1 ^ ") vs (" ^ str2 ^ "," ^ string_of_bool !fail2 ^ ")\n" in
  let res = (str1 = str2) && !fail1 = !fail2 in
    assert_bool fail_str res
  )

let binary_tests inputs = List.fold_left (fun tl (lhs, rhs) ->
  (* all operations for all inputs *)
  let add = binary_test_create lhs rhs "add" in
  let sub = binary_test_create lhs rhs "sub" in
  let mul = binary_test_create lhs rhs "mul" in
  let div = binary_test_create lhs rhs "div" in
  let rem = binary_test_create lhs rhs "rem" in
  let compare = binary_test_create lhs rhs "compare" in
  (* repeat tests with LHS and RHS interchanged. *)
  let add_rev = binary_test_create rhs lhs "add" in
  let sub_rev = binary_test_create rhs lhs "sub" in
  let mul_rev = binary_test_create rhs lhs "mul" in
  let div_rev = binary_test_create rhs lhs "div" in
  let rem_rev = binary_test_create rhs lhs "rem" in
  let compare_rev = binary_test_create rhs lhs "compare" in
    add::sub::mul::div::rem::compare::add_rev::sub_rev::mul_rev::div_rev::rem_rev::compare_rev::tl
) [] inputs

end

let non_arithmetic_tests = test_case (fun _ ->
  let err = "Error in logical operation test(s)." in

  let ofs = Int256.of_string in
  assert_bool err (Int256.logand (ofs "1") (ofs "1") = (ofs "1"));
  assert_bool err (Int256.logand (ofs "1") (ofs "0") = (ofs "0"));
  assert_bool err (Int256.logor (ofs "0") (ofs "1") = (ofs "1"));
  assert_bool err (Int256.logor (ofs "0") (ofs "0") = (ofs "0"));
  assert_bool err (Int256.logxor (ofs "0") (ofs "1") = (ofs "1"));
  assert_bool err (Int256.logxor (ofs "0") (ofs "0") = (ofs "0"));
  assert_bool err (Int256.logxor (ofs "1") (ofs "1") = (ofs "0"));
  assert_bool err (Int256.shift_left (ofs "1") 0 = (ofs "1"));
  assert_bool err (Int256.shift_left (ofs "1") 1 = (ofs "2"));
  assert_bool err (Int256.shift_left (ofs "1") 129 =
        (ofs "680564733841876926926749214863536422912")); (* 2 ^ 129 *)
  assert_bool err (Int256.shift_left (ofs "1") 255 = (ofs int256_min_str));
  assert_bool err (Int256.shift_left (ofs "1") 256 = (ofs "0"));
  assert_bool err (Int256.shift_right_logical (ofs "1") 1 = (ofs "0"));
  assert_bool err (Int256.shift_right_logical (ofs "-1") 1 = (ofs int256_max_str));

  assert_bool err (Int256.setbit (ofs "0") 0 = (ofs "1"));
  assert_bool err (Int256.setbit (ofs "0") 255 = (ofs int256_min_str));
  assert_bool err (Int256.clearbit (ofs "1") 0 = (ofs "0"));
  assert_bool err (Int256.clearbit (ofs int256_min_str) 255 = (ofs "0"));
  (* Test little-endian. *)
  let buf = Bytes.create 32 in
  let _ = Int256.to_bytes_little_endian (ofs "1") buf 0 in
  let s1 = Bytes.to_string buf in
  let _ = Int256.to_bytes_big_endian (Int256.shift_left (ofs "1") 248) buf 0 in
  let s2 = Bytes.to_string buf in
  assert_bool err (s1 = s2);
  assert_bool err (Int256.abs (ofs "-1") = (ofs "1"));
  let _ = Int256.to_bytes_big_endian (Int256.max_int) buf 0 in
  let max_int' = Int256.of_bytes_big_endian buf 0 in
  assert_bool err ((Int256.compare Int256.max_int max_int') = 0);
  let _ = Int256.to_bytes_little_endian (Int256.max_int) buf 0 in
  let max_int' = Int256.of_bytes_little_endian buf 0 in
  assert_bool err ((Int256.compare Int256.max_int max_int') = 0);

  let ofs = Uint256.of_string in
  assert_bool err (Uint256.logand (ofs "1") (ofs "1") = (ofs "1"));
  assert_bool err (Uint256.logand (ofs "1") (ofs "0") = (ofs "0"));
  assert_bool err (Uint256.logor (ofs "0") (ofs "1") = (ofs "1"));
  assert_bool err (Uint256.logor (ofs "0") (ofs "0") = (ofs "0"));
  assert_bool err (Uint256.logxor (ofs "0") (ofs "1") = (ofs "1"));
  assert_bool err (Uint256.logxor (ofs "0") (ofs "0") = (ofs "0"));
  assert_bool err (Uint256.logxor (ofs "1") (ofs "1") = (ofs "0"));
  assert_bool err (Uint256.shift_left (ofs "1") 0 = (ofs "1"));
  assert_bool err (Uint256.shift_left (ofs "1") 1 = (ofs "2"));
  assert_bool err (Uint256.shift_left (ofs "1") 129 =
        (ofs "680564733841876926926749214863536422912")); (* 2 ^ 129 *)
  assert_bool err (Uint256.shift_left (ofs "1") 255 =
        (Uint256.add (ofs "1") (ofs int256_max_str)));
  assert_bool err (Uint256.shift_left (ofs "1") 256 = (ofs "0"));
  assert_bool err (Uint256.shift_right_logical (ofs "1") 1 = (ofs "0"));
  assert_bool err (Uint256.shift_right_logical (ofs uint256_max_str) 255 = (ofs"1"));
  assert_bool err (Uint256.shift_right_logical (ofs uint256_max_str) 256 = (ofs "0"));
  assert_bool err (Uint256.shift_right_logical (ofs uint256_max_str) 128 = (ofs (Uint128.to_string Uint128.max_int)));
  assert_bool err (Uint256.shift_right_logical (ofs uint256_max_str) 255 = (ofs"1"));
  (* both the "shift_right"s are same for unsigned integers *)
  assert_bool err (Uint256.shift_right (ofs "1") 1 = (ofs "0"));
  assert_bool err (Uint256.shift_right (ofs uint256_max_str) 255 = (ofs"1"));
  assert_bool err (Uint256.setbit (ofs "0") 0 = (ofs "1"));
  assert_bool err (Uint256.setbit (ofs "0") 255 = (Uint256.add (Uint256.div (ofs uint256_max_str) (ofs "2")) (ofs "1")));
  assert_bool err (Uint256.clearbit (ofs "1") 0 = (ofs "0"));
  assert_bool err (Uint256.clearbit (Uint256.add (Uint256.div (ofs uint256_max_str) (ofs "2")) (ofs "1")) 255 = (ofs "0"));
  assert_bool err (Uint256.abs (ofs "1") = (ofs "1"));

)

module Uint256Tester = IntTester (Uint256) (Uint256_Emu)
let list_uint256 = (Uint256Tester.binary_tests binary_inputs_uint)
let uint256_tests_list = List.append (t1_uint::t2_uint::[]) list_uint256
let uint256_tests = "uint256_tests" >::: uint256_tests_list

module Int256Tester = IntTester (Int256) (Int256_Emu)
let list_int256 = (Int256Tester.binary_tests binary_inputs_int)
let int256_tests_list = List.append (t1_int::t2_int::t3_int::[]) list_int256
let int256_tests = "int256_tests" >::: int256_tests_list

(* The test to be called from Testsuite. *)
let integer256_tests = "integer256_tests" >::: (uint256_tests::int256_tests::non_arithmetic_tests::[])
