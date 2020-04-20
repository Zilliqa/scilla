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

open ErrorUtils

type int_bit_width = Bits32 | Bits64 | Bits128 | Bits256

val int_bit_width_to_string : int_bit_width -> string

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

val pp_prim_typ : prim_typ -> string

type t =
  | PrimType of prim_typ
  | MapType of t * t
  | FunType of t * t
  | ADT of loc Identifier.t * t list
  | TypeVar of string
  | PolyFun of string * t
  | Unit
[@@deriving sexp]

val pp_typ : t -> string

(****************************************************************)
(*                     Type substitutions                       *)
(****************************************************************)

val free_tvars : t -> string list

val mk_fresh_var : string list -> string -> string

val refresh_tfun : t -> string list -> t

val canonicalize_tfun : t -> t

val equal : t -> t -> bool

val subst_type_in_type : string -> t -> t -> t

val subst_types_in_type : (string * t) list -> t -> t

val subst_type_in_type' : 'a Identifier.t -> t -> t -> t

(****************************************************************)
(*                     PrimType utilities                       *)
(****************************************************************)

val is_prim_type : t -> bool

val is_int_type : t -> bool

val is_uint_type : t -> bool

val is_bystrx_type : t -> bool

val int_width : t -> int option

val int32_typ : t

val int64_typ : t

val int128_typ : t

val int256_typ : t

val uint32_typ : t

val uint64_typ : t

val uint128_typ : t

val uint256_typ : t

val string_typ : t

val bnum_typ : t

val msg_typ : t

val event_typ : t

val exception_typ : t

val bystr_typ : t

val bystrx_typ : int -> t

(* Given a ByStrX, return integer X *)
val bystrx_width : t -> int option
