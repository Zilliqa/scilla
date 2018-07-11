
let uint256_max_str = "115792089237316195423570985008687907853269984665640564039457584007913129639935"

module Uint256_Emu = struct
   open Big_int
   type t = big_int

  let zero = zero_big_int
  let one = add_int_big_int 1 zero
  let max_int = big_int_of_string uint256_max_str
  let min_int = zero

  let of_string s = big_int_of_string s
  let to_string i = string_of_big_int i

  let add a b =
    (* modulo arithmetic for unsigned integers *)
    mod_big_int (add_big_int a b) (add_big_int max_int one)

  let sub a b =
    (* modulo arithmetic for unsigned integers *)
    let c = sub_big_int a b in
    if (compare_big_int c zero) < 0
    (* if c < 0 then (max_int+c+1) *)
    then add_big_int (add_big_int max_int c) one
    else
      c

  let mul a b =
    (* modulo arithmetic for unsigned integers *)
    mod_big_int (mult_big_int a b) (add_big_int max_int one)

  let div a b =
    div_big_int a b

  let rem a b =
    mod_big_int a b

  let compare a b =
    compare_big_int a b

  let abs a =
    abs_big_int a

end

open OUnit2
open Integer256

let t1 = test_case (fun test_ctxt ->
  let b = (Uint256.to_string Uint256.max_int) = uint256_max_str in
  assert_bool "Uint256.max_int invalid" b)

let t2 = test_case (fun test_ctxt ->
  let c = Uint256.of_string (Uint256.to_string Uint256.max_int) in
  let b = (compare Uint256.max_int c) = 0 in
  assert_bool "Uint256 string conversion fail" b)

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
  val abs: t -> t
  val zero: t
  val one: t
  val min_int: t
  val max_int: t
end

module IntTester (IR1 : IntRep) (IR2 : IntRep) = struct
(* Create a test for binary op b/w two numbers represented as strings *)
let binary_test_create a b ops = test_case (fun test_ctxt ->
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

end

module Uint256Tester = IntTester (Uint256) (Uint256_Emu)

let binary_inputs =
  [ ("0", "0");
    ("0", "1");
    ("1", "1");
    ("1", "2");
    (* (max_int, max_int) *)
    (Uint256.to_string Uint256.max_int, uint256_max_str);
    (* (max_int/2, "2") *)
    ("57896044618658097711785492504343953926634992332820282019728792003956564819967", "2");
    (* (max_int/4, "4") *)
    ("28948022309329048855892746252171976963317496166410141009864396001978282409983", "5");
    (* (max_int, 1) *)
    (uint256_max_str, "1");
    (* (max_int, 0) *)
    (uint256_max_str, "0");
    ("1", "0"); ]

let rec binary_tests = List.fold_left (fun tl (lhs, rhs) ->
  let add = Uint256Tester.binary_test_create lhs rhs "add" in
  let sub = Uint256Tester.binary_test_create lhs rhs "sub" in
  let mul = Uint256Tester.binary_test_create lhs rhs "mul" in
  let div = Uint256Tester.binary_test_create lhs rhs "div" in
  let rem = Uint256Tester.binary_test_create lhs rhs "rem" in
  let compare = Uint256Tester.binary_test_create lhs rhs "compare" in
    add::sub::mul::div::rem::compare::tl
) [] binary_inputs

let uint256_tests_list = List.append (t1::t2::[]) binary_tests

(* The test to be called from Testsuite. *)
let uint256_tests = "uint256_tests" >::: uint256_tests_list
