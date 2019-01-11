open OUnit2
open Core.Result

let t1 = test_case (fun _ ->
  let open Schnorr in
  let privK, pubK = genKeyPair () in
  let msg = "Hello world\n" in
  let signature = sign privK pubK msg in
  let succ = verify pubK msg signature in
  assert_bool "Signature verification failed" succ)

let t2 = test_case (fun _ ->
  let open Schnorr in
  let privK, pubK = genKeyPair () in
  let msg = "Hello world\n" in
  let signature = sign privK pubK msg in
  let succ = verify pubK (msg ^ "\n") signature in
  assert_bool "Signature incorrectly verified" (not succ))

let schnorr_tests = "schnorr_tests" >::: [t1;t2]

let t1' = test_case (fun _ ->
  let open Secp256k1Wrapper in
  let privK, pubK = Schnorr.genKeyPair() in
  let msg = "Hello world\n" in
  (* Verify public key's match b/w implementations. *)
  let pubK' = pk_from_sk privK in
  let pubK'' = 
    (match pubK' with
    | Ok pubK'' -> pubK''
    | Error _ -> "")
  in
  (assert_bool "Public key mis-match b/w Schnorr and ECDSA" (pubK = pubK''));
  let succ =
    (match sign privK msg with
    | Ok signature ->
      (match verify pubK msg signature with
      | Ok succ -> succ
      | Error s ->
        let s' = (ErrorUtils.sprint_scilla_error_list s) in
        Printf.fprintf stderr "Error: %s\n" s';
        false
      )
    | Error s ->
      let s' = (ErrorUtils.sprint_scilla_error_list s) in
      Printf.fprintf stderr "Error: %s\n" s';
      false
    )
  in
  assert_bool "Signature verification failed" succ)

let t2' = test_case (fun _ ->
  let open Secp256k1Wrapper in
  let privK, pubK = Schnorr.genKeyPair() in
  let msg = "Hello world\n" in
  (* Verify public key's match b/w implementations. *)
  let pubK' = pk_from_sk privK in
  let pubK'' = 
    (match pubK' with
    | Ok pubK'' -> pubK''
    | Error _ -> "")
  in
  (assert_bool "Public key mis-match b/w Schnorr and ECDSA" (pubK = pubK''));
  let succ =
    (match sign privK msg with
    | Ok signature ->
      (match verify pubK (msg ^ "\n") signature with
      | Ok succ -> succ
      | Error s ->
        let s' = (ErrorUtils.sprint_scilla_error_list s) in
        Printf.fprintf stderr "Error: %s\n" s';
        false
      )
    | Error s ->
      let s' = (ErrorUtils.sprint_scilla_error_list s) in
      Printf.fprintf stderr "Error: %s\n" s';
      false
    )
  in
  assert_bool "Signature incorrectly verified" (not succ))

let ecdsa_tests = "ecda_tests" >::: [t1';t2']

let signature_tests _ = "signature_tests" >::: [schnorr_tests; ecdsa_tests]
