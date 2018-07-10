open OUnit2
open Integer256

let t1 = test_case (fun test_ctxt ->
  let b = (Uint256.to_string Uint256.max_int) = "115792089237316195423570985008687907853269984665640564039457584007913129639935" in
  assert_bool "Uint256.max_int invalid" b)

let t2 = test_case (fun test_ctxt ->
  let c = Uint256.of_string (Uint256.to_string Uint256.max_int) in
  let b = (compare Uint256.max_int c) = 0 in
  assert_bool "Uint256 string conversion fail" b)

let uint256_tests_list = (t1::t2::[])

(* The test to be called from Testsuite. *)
let uint256_tests = "uint256_tests" >::: uint256_tests_list
