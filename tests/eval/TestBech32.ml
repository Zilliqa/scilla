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
open Bech32
open Syntax
open Utils
open Core

let hex_to_raw_bytes h = Bystr.parse_hex h |> Bystr.to_raw_bytes

(* Inputs and outputs matched in https://www.coinhako.com/zil-check. *)
let test1 = test_case (fun _ ->
  let decoded_gold =  hex_to_raw_bytes "0x7aa7ea9f4534d8d70224b9c2fb165242f321f12b" in
  let bech32_addr = "zil102n74869xnvdwq3yh8p0k9jjgtejruft268tg8" in
  match decode_bech32_addr ~prefix:"zil" ~addr:bech32_addr with
  | Some b -> assert_bool "Bech32 address decode failed" 
                          (b = decoded_gold && is_valid_bech32 ~prefix:"zil" ~addr:bech32_addr)
  | None -> assert_failure "Bech32 address validity check failed"
)
let test2 = test_case (fun _ ->
  let decoded_gold =  hex_to_raw_bytes "0xa9d24392469ea49a3a3ab031e37a206c789e7155" in
  let bech32_addr = "zil148fy8yjxn6jf5w36kqc7x73qd3ufuu24a4u8t9" in
  match decode_bech32_addr ~prefix:"zil" ~addr:bech32_addr with
  | Some b -> assert_bool "Bech32 address decode failed"
                          (b = decoded_gold && is_valid_bech32 ~prefix:"zil" ~addr:bech32_addr)
  | None -> assert_failure "Bech32 address validity check failed"
)

let test3 = test_case (fun _ ->
  (* This address below is same as test2's, but with the last checksum character modified. *)
  let bech32_addr = "zil148fy8yjxn6jf5w36kqc7x73qd3ufuu24a4u8t8" in
  match decode_bech32_addr ~prefix:"zil" ~addr:bech32_addr with
  | Some _ -> assert_failure "Bech32 address decode incorrectly validated"
  | None -> assert_bool "Bech32 address incorrectly validated" 
              (not (is_valid_bech32 ~prefix:"zil" ~addr:bech32_addr))
)

let test4 = test_case (fun _ ->
  let input = hex_to_raw_bytes "0x7aa7ea9f4534d8d70224b9c2fb165242f321f12b" in
  let gold_output = "zil102n74869xnvdwq3yh8p0k9jjgtejruft268tg8" in
  match encode_bech32_addr ~prefix:"zil" ~addr:input with
  | Some output -> assert_bool "Bech32 encode mismatch" (output = gold_output)
  | None -> assert_failure "Bech32 encoding failed"
)

let test5 = test_case (fun _ ->
  let input = hex_to_raw_bytes "0xa9d24392469ea49a3a3ab031e37a206c789e7155" in
  let gold_output = "zil148fy8yjxn6jf5w36kqc7x73qd3ufuu24a4u8t9" in
  match encode_bech32_addr ~prefix:"zil" ~addr:input with
  | Some output -> assert_bool "Bech32 encode mismatch" (output = gold_output)
  | None -> assert_failure "Bech32 encoding failed"
)

(* Takes in a random number and uses that to run a random test of bech32 encoding/decoding.
 * Returns another random number for use in next call. *)
let random_test seed =
  Random.init seed;
  let bystr20_r = int_fold 20 ~init:"" ~f:(fun acc _ ->
    (* Cha.chr takes an integer in the range 0-255. So generate randomly for that. *)
    let r_int = Random.int 256 in
    let r_c = Caml.Char.chr r_int in
    acc ^ (String.of_char r_c)
  ) in
  let errmsg = Printf.sprintf 
    "Failed: Random test for bech32 encoding / decoding with seed %d" seed
  in
  match encode_bech32_addr ~prefix:"zil" ~addr:bystr20_r with
  | Some encoded_str ->
    (match decode_bech32_addr ~prefix:"zil" ~addr:encoded_str with
    | Some decoded_str -> assert_bool errmsg (bystr20_r = decoded_str)
    | None -> assert_failure errmsg)
  | None -> assert_failure errmsg

let random_tests = test_case (fun _ -> 
  (* NOTE: Is it better to use a pre-determined master seed? *)
  Random.self_init ();
  (* r is used re-seed before each test, to enable reproducing individual tests easily. *)
  let r = Random.bits () in
  for i = 0 to 100 do random_test (i+r) done
)

let bech32_tests = "bech32_tests" >::: [test1;test2;test3;test4;test5;random_tests]
