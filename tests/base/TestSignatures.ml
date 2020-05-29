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
open Core_kernel
open! Int.Replace_polymorphic_compare
open Result
open OUnit2
open Scilla_base
open Scilla_crypto

let t1 =
  test_case (fun _ ->
      let open Schnorr in
      try
        let privK, pubK = Option.value_exn (genKeyPair ()) in
        let msg = "Hello world\n" in
        let signature = Option.value_exn (sign privK pubK msg) in
        let succ = Option.value_exn (verify pubK msg signature) in
        assert_bool "Signature verification failed" succ
      with (* Check if Option.value_exn() failed. *)
      | Invalid_argument _ ->
        assert_failure "Schnorr function errored when called from testsuite")

let t2 =
  test_case (fun _ ->
      let open Schnorr in
      try
        let privK, pubK = Option.value_exn (genKeyPair ()) in
        let msg = "Hello world\n" in
        let signature = Option.value_exn (sign privK pubK msg) in
        let succ = Option.value_exn (verify pubK (msg ^ "\n") signature) in
        assert_bool "Signature incorrectly verified" (not succ)
      with (* Check if Option.value_exn () failed. *)
      | Invalid_argument _ ->
        assert_failure "Schnorr function errored when called from testsuite")

let schnorr_tests = "schnorr_tests" >::: [ t1; t2 ]

let t1' =
  test_case (fun _ ->
      let open Secp256k1Wrapper in
      match Schnorr.genKeyPair () with
      | None ->
          assert_failure "Schnorr function errored when called from testsuite"
      | Some (privK, pubK) ->
          let msg = "Hello world\n" in
          (* Verify public key's match b/w implementations. *)
          let pubK' = pk_from_sk privK in
          let pubK'' = match pubK' with Ok pubK'' -> pubK'' | Error _ -> "" in
          assert_bool "Public key mis-match b/w Schnorr and ECDSA"
            String.(pubK = pubK'');
          let succ =
            match sign privK msg with
            | Ok signature -> (
                match verify pubK msg signature with
                | Ok succ -> succ
                | Error s ->
                    let s' = ErrorUtils.sprint_scilla_error_list s in
                    Printf.fprintf stderr "Error: %s\n" s';
                    false )
            | Error s ->
                let s' = ErrorUtils.sprint_scilla_error_list s in
                Printf.fprintf stderr "Error: %s\n" s';
                false
          in
          assert_bool "Signature verification failed" succ)

let t2' =
  test_case (fun _ ->
      let open Secp256k1Wrapper in
      match Schnorr.genKeyPair () with
      | None ->
          assert_failure "Schnorr function errored when called from testsuite"
      | Some (privK, pubK) ->
          let msg = "Hello world\n" in
          (* Verify public key's match b/w implementations. *)
          let pubK' = pk_from_sk privK in
          let pubK'' = match pubK' with Ok pubK'' -> pubK'' | Error _ -> "" in
          assert_bool "Public key mis-match b/w Schnorr and ECDSA"
            String.(pubK = pubK'');
          let succ =
            match sign privK msg with
            | Ok signature -> (
                match verify pubK (msg ^ "\n") signature with
                | Ok succ -> succ
                | Error s ->
                    let s' = ErrorUtils.sprint_scilla_error_list s in
                    Printf.fprintf stderr "Error: %s\n" s';
                    false )
            | Error s ->
                let s' = ErrorUtils.sprint_scilla_error_list s in
                Printf.fprintf stderr "Error: %s\n" s';
                false
          in
          assert_bool "Signature incorrectly verified" (not succ))

let ecdsa_tests = "ecda_tests" >::: [ t1'; t2' ]

module All = struct
  let tests _ = "signature_tests" >::: [ schnorr_tests; ecdsa_tests ]
end
