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
open Syntax
open Stdint
open Integer256

let int32_str = "Int32"
let int64_str = "Int64"
let int128_str = "Int128"
let int256_str = "Int256"
let uint32_str = "Uint32"
let uint64_str = "Uint64"
let uint128_str = "Uint128"
let uint256_str = "Uint256"
let string_str = "String"
let bnum_str = "BNum"
let msg_str = "Message"
let bystr_str = "ByStr"
(* ByStrX will have X appended below. *)
let bystrx_str = "ByStr"

let int32_typ = PrimType int32_str
let int64_typ = PrimType int64_str
let int128_typ = PrimType int128_str
let int256_typ = PrimType int256_str
let uint32_typ = PrimType uint32_str
let uint64_typ = PrimType uint64_str
let uint128_typ = PrimType uint128_str
let uint256_typ = PrimType uint256_str
let string_typ = PrimType string_str
let bnum_typ = PrimType bnum_str
let msg_typ = PrimType msg_str
let bystr_typ = PrimType bystr_str
let bystrx_typ b = PrimType (bystrx_str ^ Int.to_string b)

(* Given a ByStrX string, return integer X *)
let bystrx_width t =
  match t with
  | PrimType s ->
    let re = Str.regexp "ByStr\\([0-9]+\\)$" in
    if Str.string_match re s 0 then
      let b = Int.of_string (Str.matched_group 1 s) in
      Some b
    else
      None
  | _ -> None

let prim_types_except_bystrx =
    [int32_typ; int64_typ; int128_typ; int256_typ;
     uint32_typ; uint64_typ; uint128_typ; uint256_typ;
     string_typ; bnum_typ; msg_typ; bystr_typ]

let is_prim_type t =
  match t with
  | PrimType _ ->
    (match (bystrx_width t) with
    | Some _ -> true (* bystrx_typ *)
    | None ->
      List.mem ~equal:(fun t1 t2 -> t1 = t2) prim_types_except_bystrx t)
  | _ -> false

let is_int_type = function
  | x when x = int32_typ ||
           x = int64_typ ||
           x = int128_typ ||
           x = int256_typ -> true
  | _ -> false

let is_uint_type = function
  | x when x = uint32_typ ||
           x = uint64_typ ||
           x = uint128_typ ||
           x = uint256_typ -> true
  | _ -> false

let is_bystrx_type t =
  match bystrx_width t with
  | Some _ -> true
  | None -> false

(****************************************************************)
(*                 Primitive Literal Utilities                  *)
(****************************************************************)


(* Validate Int* and Uint* literals (wx, x), whether the
   string x they contain can be represented in wx bits  *)
let validate_int_literal i =
  try
    match i with
    | IntLit (wx, x) ->
      (match wx with
      | 32 -> Int32.to_string (Int32.of_string x) = x
      | 64 -> Int64.to_string (Int64.of_string x) = x
      | 128 -> Int128.to_string (Int128.of_string x) = x
      | 256 -> Int256.to_string (Int256.of_string x) = x
      | _ -> false
      )
    | UintLit (wx, x) ->
      (match wx with
      | 32 -> Uint32.to_string (Uint32.of_string x) = x
      | 64 -> Uint64.to_string (Uint64.of_string x) = x
      | 128 -> Uint128.to_string (Uint128.of_string x) = x
      | 256 -> Uint256.to_string (Uint256.of_string x) = x
      | _ -> false
      )
    | _ -> false
  with
  | _ -> false

(* Given an integer type (as string) and the value (as string),
   build IntLit or UintLit out of it. *)
let build_int t v =
  let validator_wrapper l = 
    if validate_int_literal l then Some l else None
  in
  match t with
  | x when x = int32_typ   -> validator_wrapper (IntLit(32, v))
  | x when x = int64_typ   -> validator_wrapper (IntLit(64, v))
  | x when x = int128_typ  -> validator_wrapper (IntLit(128, v))
  | x when x = int256_typ  -> validator_wrapper (IntLit(256, v))
  | x when x = uint32_typ  -> validator_wrapper (UintLit(32, v))
  | x when x = uint64_typ  -> validator_wrapper (UintLit(64, v))
  | x when x = uint128_typ -> validator_wrapper (UintLit(128, v))
  | x when x = uint256_typ -> validator_wrapper (UintLit(256, v))
  | _ -> None

let validate_bnum_literal b = match b with
  | BNum v ->
    let s, re =
    v, Str.regexp "[0-9]+$" in
    if Str.string_match re s 0
    then
      true
    else
      false
  | _ -> false

(* Given an integer string, build a BNum literal,
   or return None on invalid input. *)
let build_bnum v =
  let b = BNum (v) in
  if validate_bnum_literal b then Some b else None

let validate_bystrx_literal b = match b with
  | ByStrX (w, s) ->
    let s, re, l =
      s, Str.regexp "0x[0-9a-f]+$", w in
    if Str.string_match re s 0 && (String.length s)-2 = (l * 2)
    then true else false
  | _ -> false

(* Given a hexadecimal byte string, build a ByStrX
   literal or return None on invalid input. *)
let build_bystrx t v =
  let w = bystrx_width t in
  match w with
  | Some b ->
    let v' = String.lowercase v in
    let bs = ByStrX (b, v') in
    if validate_bystrx_literal bs then Some bs else None
  | None -> None

let validate_bystr_literal b = match b with
  | ByStr s ->
    let s, re =
      s, Str.regexp "0x[0-9a-f]+$" in
    if Str.string_match re s 0 && ((String.length s)-2) % 2 = 0
    then true else false
  | _ -> false

(* Given a hexadecimal byte string, build a ByStr
   literal or return None on invalid input. *)
let build_bystr v =
  let v' = String.lowercase v in
  let bs = ByStr v' in
  if validate_bystr_literal bs then Some bs else None

let build_prim_literal t v =
  match t with
  | x when x = string_typ -> Some (StringLit v)
  | x when x = bnum_typ -> build_bnum v
  | x when x = bystr_typ -> build_bystr v
  | x when is_bystrx_type x -> build_bystrx t v
  | x when is_int_type x || is_uint_type x -> build_int t v
  | _ -> None
