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

let hex_to_raw_bytes h = Bystr.parse_hex h |> Bystr.to_raw_bytes

(* Inputs and outputs matched in https://www.coinhako.com/zil-check. *)
let test1 = test_case (fun _ ->
  let decoded_gold =  hex_to_raw_bytes "0x7aa7ea9f4534d8d70224b9c2fb165242f321f12b" in
  match decode_bech32_addr "zil" "zil102n74869xnvdwq3yh8p0k9jjgtejruft268tg8" with
  | Some b -> assert_bool "Bech32 address decode failed" (b = decoded_gold)
  | None -> assert_failure "Bech32 address validity check failed"
)
let test2 = test_case (fun _ ->
  let decoded_gold =  hex_to_raw_bytes "0xa9d24392469ea49a3a3ab031e37a206c789e7155" in
  match decode_bech32_addr "zil" "zil148fy8yjxn6jf5w36kqc7x73qd3ufuu24a4u8t9" with
  | Some b -> assert_bool "Bech32 address decode failed" (b = decoded_gold)
  | None -> assert_failure "Bech32 address validity check failed"
)

let bech32_tests = "bech32_tests" >::: [test1;test2]
