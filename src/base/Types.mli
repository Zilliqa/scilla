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

type typ =
  | PrimType of prim_typ
  | MapType of typ * typ
  | FunType of typ * typ
  | ADT of loc Identifier.t * typ list
  | TypeVar of string
  | PolyFun of string * typ
  | Unit
[@@deriving sexp]

val pp_typ : typ -> string

(****************************************************************)
(*                     Type substitutions                       *)
(****************************************************************)

val free_tvars : typ -> string list

val mk_fresh_var : string list -> string -> string

val refresh_tfun : typ -> string list -> typ

val canonicalize_tfun : typ -> typ

val equal_typ : typ -> typ -> bool

val subst_type_in_type : string -> typ -> typ -> typ

val subst_types_in_type : (string * typ) list -> typ -> typ

val subst_type_in_type' : 'a Identifier.t -> typ -> typ -> typ
