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
let dec2bystr32 s =
  let b = Bytes.create 32 in
  Uint256.to_bytes_big_endian (Uint256.of_string s) b 0;
  Bytes.to_string b

let group_order = dec2bystr32 "21888242871839275222246405745257275088548364400416034343698204186575808495617"
let group_order_m1 = dec2bystr32 "21888242871839275222246405745257275088548364400416034343698204186575808495616"

let add_helper p1 p2 =
  match alt_bn128_G1_add p1 p2 with
  | Some r -> r
  | None -> assert_failure "TestSnark: add_bn128_G1_add failed"

let mul_helper p s =
  match alt_bn128_G1_mul p s with
  | Some r -> r
  | None -> assert_failure "TestSnark: add_bn128_G1_mul failed"

let negateG1 p =
    match alt_bn128_G1_mul p group_order_m1 with
    | Some r -> r
    | None -> assert_failure "TestSnark: negateG1 failed"

let test_add_zero = test_case (fun _ ->
  (* "0 + 0 == 0" *)
  let zero_zero_p = {g1x = (dec2bystr32 "0"); g1y = (dec2bystr32 "0")} in
  match alt_bn128_G1_add zero_zero_p zero_zero_p with
  | Some result -> assert_bool "TestSnark failed: test_add_zero: incorrect result" (result = zero_zero_p)
  | None -> assert_failure "TestSnark failed: test_add_zero: failed."
)

let test_invalid = test_case (fun _ ->
  let (x, y) = 
    (dec2bystr32 "6851077925310461602867742977619883934042581405263014789956638244065803308498"),
    (dec2bystr32 "10336382210592135525880811046708757754106524561907815205241508542912494488506")
  in
  let (invalid_x, invalid_y) = (String.sub x ~pos:0 ~len:3) ^
    (((Char.to_int x.[3]) lxor 1) |> Char.of_int_exn |> String.of_char) ^
    (String.sub x ~pos:4 ~len:(String.length x - 4))
    , y
  in
  match alt_bn128_G1_add {g1x = x; g1y = y} {g1x = invalid_x; g1y = invalid_y} with
  | Some _ -> assert_failure "TestSnark failed: test_invalid: alt_bn128_add succeeded on invalid input"
  | None -> ()
)

let test_mul_add = test_case (fun _ ->
  let p =
    { g1x = (dec2bystr32 "6851077925310461602867742977619883934042581405263014789956638244065803308498");
      g1y = (dec2bystr32 "10336382210592135525880811046708757754106524561907815205241508542912494488506")}
  in
  let s = (dec2bystr32 "2") in
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
