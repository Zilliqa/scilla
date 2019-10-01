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

(* The tests in this file are modelled after Ethereum's unit tests for zkSNARKs.
   https://github.com/ethereum/aleth/blob/master/test/unittests/libdevcrypto/LibSnark.cpp *)

open Core
open OUnit2
open Snark
open Integer256

(* Convert a decimal string to a binary string of 32 bytes. *)
let decimal_to_bystr32_raw s =
  let b = Bytes.create 32 in
  Uint256.to_bytes_big_endian (Uint256.of_string s) b 0;
  Bytes.to_string b

let scalars_to_point x y = x ^ y

let test_add_zero = test_case (fun _ ->
  (*  "0 + 0 == 0" *)
  let zero_zero_p = scalars_to_point (decimal_to_bystr32_raw "0") (decimal_to_bystr32_raw "0") in
  match alt_bn128_G1_add zero_zero_p zero_zero_p with
  | Some result -> assert_bool "TestSnark failed: test_add_zero: incorrect result" (result = zero_zero_p)
  | None -> assert_failure "TestSnark failed: test_add_zero: failed."
)

let test_invalid = test_case (fun _ ->
  let x = scalars_to_point
    (decimal_to_bystr32_raw "6851077925310461602867742977619883934042581405263014789956638244065803308498")
    (decimal_to_bystr32_raw "10336382210592135525880811046708757754106524561907815205241508542912494488506")
  in
  let invalid = (String.sub x ~pos:0 ~len:3) ^
    (((Char.to_int x.[3]) lxor 1) |> Char.of_int_exn |> String.of_char) ^
    (String.sub x ~pos:4 ~len:(String.length x - 4))
  in
  match alt_bn128_G1_add x invalid with
  | Some _ -> assert_failure "TestSnark failed: test_invalid: alt_bn128_add succeeded on invalid input"
  | None -> ()
)

let test_mul_add = test_case (fun _ ->
  let p = scalars_to_point
    (decimal_to_bystr32_raw "6851077925310461602867742977619883934042581405263014789956638244065803308498")
    (decimal_to_bystr32_raw "10336382210592135525880811046708757754106524561907815205241508542912494488506")
  in
  let s = (decimal_to_bystr32_raw "2") in
  match alt_bn128_G1_add p p, alt_bn128_G1_mul p s with
  | Some sum, Some prod ->
    assert_bool "TestSnark failed: test_mul_add: comparison failed" (prod = sum) 
  | _ -> assert_failure "TestSnark failed: test_mul_add: alt_bn128_(add/mul) failed"
)

let snark_tests _ = "snark_tests" >::: [
  test_add_zero;
  test_invalid;
  test_mul_add
]
