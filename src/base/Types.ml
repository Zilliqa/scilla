(*
  This file is part of scilla.

  Copyright (c) 2020 - present Zilliqa Research Pvt. Ltd.
  
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
open Sexplib.Std
open ErrorUtils
open Identifiers

(*******************************************************)
(*                         Types                       *)
(*******************************************************)

type int_bit_width = Bits32 | Bits64 | Bits128 | Bits256
[@@deriving sexp, equal]

type prim_typ =
  | Int_typ of int_bit_width
  | Uint_typ of int_bit_width
  | String_typ
  | Bnum_typ
  | Msg_typ
  | Event_typ
  | Exception_typ
  | Bystr_typ
  | Bystrx_typ of int
[@@deriving equal]

let sexp_of_prim_typ = function
  | Int_typ Bits32 -> Sexp.Atom "Int32"
  | Int_typ Bits64 -> Sexp.Atom "Int64"
  | Int_typ Bits128 -> Sexp.Atom "Int128"
  | Int_typ Bits256 -> Sexp.Atom "Int256"
  | Uint_typ Bits32 -> Sexp.Atom "Uint32"
  | Uint_typ Bits64 -> Sexp.Atom "Uint64"
  | Uint_typ Bits128 -> Sexp.Atom "Uint128"
  | Uint_typ Bits256 -> Sexp.Atom "Uint256"
  | String_typ -> Sexp.Atom "String"
  | Bnum_typ -> Sexp.Atom "BNum"
  | Msg_typ -> Sexp.Atom "Message"
  | Event_typ -> Sexp.Atom "Event"
  | Exception_typ -> Sexp.Atom "Exception"
  | Bystr_typ -> Sexp.Atom "ByStr"
  | Bystrx_typ b -> Sexp.Atom ("ByStr" ^ Int.to_string b)

let prim_typ_of_sexp _ = failwith "prim_typ_of_sexp is not implemented"

type typ =
  | PrimType of prim_typ
  | MapType of typ * typ
  | FunType of typ * typ
  | ADT of loc ident * typ list
  | TypeVar of string
  | PolyFun of string * typ
  | Unit
[@@deriving sexp]

let int_bit_width_to_string = function
  | Bits32 -> "32"
  | Bits64 -> "64"
  | Bits128 -> "128"
  | Bits256 -> "256"

let pp_prim_typ = function
  | Int_typ bw -> "Int" ^ int_bit_width_to_string bw
  | Uint_typ bw -> "Uint" ^ int_bit_width_to_string bw
  | String_typ -> "String"
  | Bnum_typ -> "BNum"
  | Msg_typ -> "Message"
  | Event_typ -> "Event"
  | Exception_typ -> "Exception"
  | Bystr_typ -> "ByStr"
  | Bystrx_typ b -> "ByStr" ^ Int.to_string b

let rec pp_typ = function
  | PrimType t -> pp_prim_typ t
  | MapType (kt, vt) -> sprintf "Map (%s) (%s)" (pp_typ kt) (pp_typ vt)
  | ADT (name, targs) ->
      let elems =
        get_id name :: List.map targs ~f:(fun t -> sprintf "(%s)" (pp_typ t))
      in
      String.concat ~sep:" " elems
  | FunType (at, vt) -> sprintf "%s -> %s" (with_paren at) (pp_typ vt)
  | TypeVar tv -> tv
  | PolyFun (tv, bt) -> sprintf "forall %s. %s" tv (pp_typ bt)
  | Unit -> sprintf "()"

and with_paren t =
  match t with
  | FunType _ | PolyFun _ -> sprintf "(%s)" (pp_typ t)
  | _ -> pp_typ t
