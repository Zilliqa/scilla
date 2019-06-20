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


open Syntax

(****************************************************************)
(*                     PrimType utilities                       *)
(****************************************************************)

val is_prim_type : typ -> bool
val is_int_type : typ -> bool
val is_uint_type : typ -> bool
val is_bystrx_type : typ -> bool
val int_width : typ -> int option

val int32_typ : typ
val int64_typ : typ
val int128_typ : typ
val int256_typ : typ
val uint32_typ : typ
val uint64_typ : typ
val uint128_typ : typ
val uint256_typ : typ
val string_typ : typ
val bnum_typ : typ
val msg_typ : typ
val event_typ : typ
val exception_typ : typ
val bystr_typ : typ
val bystrx_typ : int -> typ
(* Given a ByStrX, return integer X *)
val bystrx_width : typ -> int option

(****************************************************************)
(*            PrimType Literal utilities                        *)
(****************************************************************)

val build_prim_literal : prim_typ -> string -> literal option
(* Is string representation of integer valid for integer typ. *)
val validate_int_string : prim_typ -> string -> bool
(* Get bit-width if int_lit. *)
val int_lit_width : int_lit -> int
(* Get bit-width if uint_lit. *)
val uint_lit_width : uint_lit -> int
(* String conversion from int_typ *)
val string_of_int_lit : int_lit -> string
(* String conversion from uint_typ *)
val string_of_uint_lit : uint_lit -> string
