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
(*                     PrimType utilities                        *)
(****************************************************************)

val is_prim_type : typ -> bool
val is_int_type : typ -> bool
val is_uint_type : typ -> bool
val is_bystr_type : typ -> bool

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
val bystr_typ : int -> typ
(* Given a ByStrX, return integer X *)
val bystr_width : typ -> int option

(****************************************************************)
(*            PrimType Literal utilities                        *)
(****************************************************************)

val build_prim_literal : typ -> string -> literal option
(* Validate Int* and Uint* literals (wx, x), whether the
   string x they contain can be represented in wx bits  *)
val validate_int_literal : literal -> bool
(* Is input literal a valid BNum? *)
val validate_bnum_literal : literal -> bool
(* Is input a valid ByStr literal? *)
val validate_bystr_literal : literal -> bool
