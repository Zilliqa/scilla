open OUnit2
open Schnorr

let t1 = test_case (fun _ ->
  let privK, pubK = genKeyPair () in
  let msg = "Hello world\n" in
  let signature = sign privK pubK msg in
  let succ = verify pubK msg signature in
  assert_bool "Signature verification failed" succ)

let t2 = test_case (fun _ ->
  let privK, pubK = genKeyPair () in
  let msg = "Hello world\n" in
  let signature = sign privK pubK msg in
  let succ = verify pubK (msg ^ "\n") signature in
  assert_bool "Signature incorrectly verified" (not succ))

let schnorr_tests _ = "schnorr_tests" >::: [t1;t2]
