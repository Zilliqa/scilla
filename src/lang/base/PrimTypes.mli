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
open Sexplib.Std

val prim_types : typ list
val is_prim_type : typ -> bool

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
val address_typ : typ
val hash_typ : typ
val msg_typ : typ

