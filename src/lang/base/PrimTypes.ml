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

let int32_typ = PrimType (Int_typ Bits32)
let int64_typ = PrimType (Int_typ Bits64)
let int128_typ = PrimType (Int_typ Bits128)
let int256_typ = PrimType (Int_typ Bits256)
let uint32_typ = PrimType (Uint_typ Bits32)
let uint64_typ = PrimType (Uint_typ Bits64)
let uint128_typ = PrimType (Uint_typ Bits128)
let uint256_typ = PrimType (Uint_typ Bits256)
let string_typ = PrimType String_typ
let bnum_typ = PrimType Bnum_typ
let msg_typ = PrimType Msg_typ
let event_typ = PrimType Event_typ
let exception_typ = PrimType Exception_typ
let bystr_typ = PrimType Bystr_typ
let bystrx_typ b = PrimType (Bystrx_typ b)

let int_width t =
  if t = int32_typ then Some 32 else
  if t = int64_typ then Some 64 else
  if t = int128_typ then Some 128 else
  if t = int256_typ then Some 256 else
  if t = uint32_typ then Some 32 else
  if t = uint64_typ then Some 64 else 
  if t = uint128_typ then Some 128 else 
  if t = uint256_typ then Some 256 else
  None

(* Given a ByStrX string, return integer X *)
let bystrx_width = function
  | PrimType (Bystrx_typ w) -> Some w
  | _ -> None

let is_prim_type = function
  | PrimType _ -> true
  | _ -> false

let is_int_type = function
  | PrimType (Int_typ _) -> true
  | _ -> false

let is_uint_type = function
  | PrimType (Uint_typ _) -> true
  | _ -> false

let is_bystrx_type = function
  | PrimType (Bystrx_typ _) -> true
  | _ -> false

(****************************************************************)
(*                 Primitive Literal Utilities                  *)
(****************************************************************)

(* Is string representation of integer valid for integer typ. *)
let validate_int_string pt x =
    try
      match pt with
      | Int_typ Bits32 -> Int32.to_string (Int32.of_string x) = x
      | Int_typ Bits64 -> Int64.to_string (Int64.of_string x) = x
      | Int_typ Bits128 -> Int128.to_string (Int128.of_string x) = x
      | Int_typ Bits256 -> Int256.to_string (Int256.of_string x) = x
      | Uint_typ Bits32 -> Uint32.to_string (Uint32.of_string x) = x
      | Uint_typ Bits64 -> Uint64.to_string (Uint64.of_string x) = x
      | Uint_typ Bits128 -> Uint128.to_string (Uint128.of_string x) = x
      | Uint_typ Bits256 -> Uint256.to_string (Uint256.of_string x) = x
      | _ -> false
    with | _ -> false

(* Given an integer type and the value (as string),
   build IntLit or UintLit out of it. *)
let build_int pt v =
  let validator_wrapper l =
    Option.some_if (validate_int_string pt v) l
  in
  try
    match pt with
    | Int_typ Bits32 -> validator_wrapper (IntLit (Int32L (Int32.of_string v)))
    | Int_typ Bits64 -> validator_wrapper (IntLit (Int64L (Int64.of_string v)))
    | Int_typ Bits128 -> validator_wrapper (IntLit (Int128L (Stdint.Int128.of_string v)))
    | Int_typ Bits256 -> validator_wrapper (IntLit (Int256L (Int256.of_string v)))
    | Uint_typ Bits32 -> validator_wrapper (UintLit (Uint32L (Stdint.Uint32.of_string v)))
    | Uint_typ Bits64 -> validator_wrapper (UintLit (Uint64L (Stdint.Uint64.of_string v)))
    | Uint_typ Bits128 -> validator_wrapper (UintLit (Uint128L (Stdint.Uint128.of_string v)))
    | Uint_typ Bits256 -> validator_wrapper (UintLit (Uint256L (Uint256.of_string v)))
    | _ -> None
  with | _ -> None

let int_lit_width = function
  | Int32L _ -> 32 | Int64L _ -> 64 | Int128L _ -> 128 | Int256L _ -> 256

let string_of_int_lit = function
  | Int32L i' -> Int32.to_string i'
  | Int64L i' -> Int64.to_string i'
  | Int128L i' -> Int128.to_string i'
  | Int256L i' -> Int256.to_string i'

let uint_lit_width = function
  | Uint32L _ -> 32 | Uint64L _ -> 64 | Uint128L _ -> 128 | Uint256L _ -> 256

let string_of_uint_lit = function
  | Uint32L i' -> Uint32.to_string i'
  | Uint64L i' -> Uint64.to_string i'
  | Uint128L i' -> Uint128.to_string i'
  | Uint256L i' -> Uint256.to_string i'

let build_prim_literal pt v =
  match pt with
  | Int_typ _ | Uint_typ _ -> build_int pt v
  | String_typ -> Some (StringLit v)
  | Bnum_typ ->
      Option.some_if (let re = Str.regexp "[0-9]+$" in
                      Str.string_match re v 0)
                     (BNum v)
  | Bystr_typ -> Some (ByStr (Bystr.parse_hex v))
  | Bystrx_typ _ -> Some (ByStrX (Bystrx.parse_hex v))
  | _ -> None
