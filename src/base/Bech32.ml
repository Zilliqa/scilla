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

open Core
open Bitstring
open Utils

(* https://github.com/Zilliqa/Zilliqa/wiki/Address-Standard#specification *)
let bech32_addr_len ~prefix = (String.length prefix) + 1 + 32 + 6
let bech32_payload_len = 32 (* Ignoring prefix, separator and checksum characters. *)
let bech32_chk_len = 6

(* The set of core characters in a bech32 address. *)
let charset = Array.of_list 
  ['q';'p';'z';'r';'y';'9';'x';'8';'g';'f';'2';'t';'v';'d';'w';'0';'s';'3';'j';'n';'5';'4';'k';'h';'c';'e';'6';'m';'u';'a';'7';'l']

(* Mapping from a 5-bit value to it's bech32 character above. *)
let charset_rev = Array.of_list [
    -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1;
    -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1;
    -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1;
    15; -1; 10; 17; 21; 20; 26; 30;  7;  5; -1; -1; -1; -1; -1; -1;
    -1; 29; -1; 24; 13; 25;  9;  8; 23; -1; 18; 22; 31; 27; 19; -1;
     1;  0;  3; 16; 11; 28; 12; 14;  6;  4;  2; -1; -1; -1; -1; -1;
    -1; 29; -1; 24; 13; 25;  9;  8; 23; -1; 18; 22; 31; 27; 19; -1;
     1;  0;  3; 16; 11; 28; 12; 14;  6;  4;  2; -1; -1; -1; -1; -1
]

(* https://github.com/sipa/bech32/blob/master/ref/c/segwit_addr.c *)
let bech32_polymod_step pre  =
    let b = pre lsr 25 in
    ((pre land 0x1FFFFFF) lsl 5) lxor
        (-((b lsr 0) land 1) land 0x3b6a57b2) lxor
        (-((b lsr 1) land 1) land 0x26508e6d) lxor
        (-((b lsr 2) land 1) land 0x1ea119fa) lxor
        (-((b lsr 3) land 1) land 0x3d4233dd) lxor
        (-((b lsr 4) land 1) land 0x2a1462b3)

let ascii_of_char c = Caml.Char.code c
let char_of_ascii i = Caml.Char.chr i
let bytes_of_bitstring bs = Bytes.of_string @@ string_of_bitstring bs

(* Decodes a bech32 address string to a string of 20 bytes.
 * Signature: prefix -> bech32_addr -> bystr20 address option. *)
let decode_bech32_addr ~prefix ~addr:str =
  (* 1. Must be of the right length. *)
  if String.length str <> (bech32_addr_len ~prefix) then None else
  (* 2. Must begin with prefix. *)
  if (String.sub str ~pos:0 ~len:(String.length prefix)) <> prefix then None else
  (* 3. Must have separator "1" after prefix. *)
  if (String.sub str ~pos:(String.length prefix) ~len:1) <> "1" then None else

  (* 4. Scan the prefix for errors. *)
  let (chk, err, (have_lower, have_upper)) = 
    List.fold (String.to_list prefix)  ~init:(1, false, (false, false)) ~f:(fun (chk, err, (have_lower, have_upper)) ch ->
      if err || (ascii_of_char ch) < 33 || (ascii_of_char ch) > 126 then (chk, true, (have_lower, have_upper)) else
 
      let (ch', have_lower', have_upper') =
        if (ascii_of_char ch) >= (ascii_of_char 'a') && (ascii_of_char ch) <= (ascii_of_char 'z') then 
          (ascii_of_char ch, true, have_upper)
        else if (ascii_of_char ch) >= (ascii_of_char 'A') && (ascii_of_char ch) <= (ascii_of_char 'Z') then
          ((ascii_of_char ch) - (ascii_of_char 'A') + (ascii_of_char 'a'), have_lower, true)
        else
          (ascii_of_char ch, have_lower, have_upper)
      in
      let chk' = (bech32_polymod_step chk) lxor (ch' lsr 5) in
      (chk', err, (have_lower', have_upper'))
    )
  in
  if err || (have_lower && have_upper) then None else

  (* 5. Checksum the prefix *)
  let chk = bech32_polymod_step chk in
  let chk = List.fold (String.to_list prefix) ~init:chk ~f:(fun acc_chk c ->
    (bech32_polymod_step acc_chk) lxor ((ascii_of_char c) land 0x1f)
  ) in

  (* 6. Scan the addr and checksum for errors. *)
  let addr_chk_str = String.sub str ~pos:((String.length prefix)+1) ~len:(bech32_payload_len+bech32_chk_len) in
  (* Create a buffer of bits for the result (20-byte raw address). *)
  let bitacc = Buffer.create () in
  let (chk, err, (have_lower, have_upper), _) = List.fold (String.to_list addr_chk_str) ~init:(chk, false, (false, false), 0)
   ~f:(fun (chk, err, (have_lower, have_upper), index) c ->
    (* do nothing if we've already seen an error. *)
    if err then (chk, err, (have_lower, have_upper), index+1) else

    let c' =
      (* it's an error if any bit other than the least 7 bits are 1. *)
      if (ascii_of_char c) land (Int.minus_one lsl 7) <> 0 then -1 else
      (* charset_rev contains -1 for invalid bech32 characters. *)
      charset_rev.(ascii_of_char c)
    in
    (* Check for errors so far. *)
    if c' = -1 then (chk, true, (have_lower, have_upper), index+1) else

    (* Do checksum on c' *)
    let chk' = (bech32_polymod_step chk) lxor c' in

    (* Accumulate the lower 5 bits of c' into our bit buffer *)
    if index < bech32_payload_len then
      (let shifted_5_bits = (Bytes.make 1 (char_of_ascii (c' lsl (8 - 5)))) in
       Buffer.add_bits bitacc shifted_5_bits 5);

    let have_lower' = c' >= (ascii_of_char 'a') && c' <= (ascii_of_char 'z') in
    let have_upper' = c' >= (ascii_of_char 'A') && c' <= (ascii_of_char 'Z') in
    (chk', err, (have_lower', have_upper'), index+1)
  ) in
  if err || (have_lower && have_upper) then None else
  if chk = 1 then Some (string_of_bitstring (Buffer.contents bitacc)) else None

(* Check if a bech32 address string is valid, based on the specification in
 * https://github.com/Zilliqa/Zilliqa/wiki/Address-Standard#specification *)
let is_valid_bech32 ~prefix ~addr =
  Option.is_some (decode_bech32_addr ~prefix ~addr)

(* Encodes a 20-byte string into the bech32 address format.
 * Signature: prefix -> bystr20 address -> bech32_addr. *)
let encode_bech32_addr ~prefix ~addr:bys =
  (* 1. Must be of the right length. *)
  if String.length bys <> 20 then None else
  (* 2. Scan the prefix for errors. *)
  let (chk, err) = 
    List.fold (String.to_list prefix)  ~init:(1, false) ~f:(fun (chk, err) ch ->
      if err || (ascii_of_char ch) < 33 || (ascii_of_char ch) > 126 ||
        (* Only lower case letters are acceptable as "prefix". *)
        ((ascii_of_char ch) >= ascii_of_char 'A' && (ascii_of_char ch) <= (ascii_of_char 'Z'))
      then (chk, true) else

      let chk' = (bech32_polymod_step chk) lxor ((ascii_of_char ch) lsr 5) in
      (chk', err)
    )
  in
  if err then None else

  (* 3. Checksum the prefix. *)
  let chk = bech32_polymod_step chk in
  let chk = List.fold (String.to_list prefix) ~init:chk ~f:(fun acc_chk c ->
    (bech32_polymod_step acc_chk) lxor ((ascii_of_char c) land 0x1f)
  ) in

  (* 4. Translate the input 8*20 bits into 5*32 bits. *)
  let input_bs = bitstring_of_string bys in
  let buf = Buffer.create () in
  for i = 0 to 31 do
    let this_5_bits = subbitstring input_bs (i*5) 5 in
    let pad = zeroes_bitstring 3 in
    let byte = concat [pad; this_5_bits] in
    Buffer.add_bits buf (bytes_of_bitstring byte) 8
  done;
  (* We can now have a 32 byte string, each byte is a padded
   * 5-bit value representing the index into charset. *)
  let input_padded = string_of_bitstring @@ Buffer.contents buf in

  (* 6. Scan through the padded input and build the checksum and outputs. *)
  let (chk, err, bech32_str) = 
    List.fold (String.to_list input_padded)  ~init:(chk, false, "") ~f:(fun (chk, err, bech32_acc) ch ->
      (* We should able to use "ch" to index into charset. *)
      if err || ((ascii_of_char ch) lsr 5) <> 0 then (chk, true, bech32_acc) else
      (* Checksum *)
      let chk' = (bech32_polymod_step chk) lxor (ascii_of_char ch) in
      let ch' = String.of_char @@ charset.(ascii_of_char ch) in
      (chk', err, bech32_acc ^ ch')
    )
  in
  if err then None else

  (* 7. We are left with building up the last 6 / checksum characters. *)
  let chk = int_fold ~init:chk ~f:(fun chkacc _ -> bech32_polymod_step chkacc) 6 in
  let chk = chk lxor 1 in
  let chkstr = int_fold 6 ~init:"" ~f:(fun acc i ->
    let ch = charset.((chk lsr ((5 - i) * 5)) land 0x1f) in
    acc ^ (String.of_char ch)
  ) in

  (* 8. Finally build up the bech32 encoded address. *)
  Some (prefix ^ "1" ^ bech32_str ^ chkstr)
