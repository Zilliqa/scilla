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
let address_str = "Address"
let hash_str = "Hash"
let msg_str = "Message"

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
let address_typ = PrimType address_str
let hash_typ = PrimType hash_str
let msg_typ = PrimType msg_str
  
let prim_types =
    [int32_typ; int64_typ; int128_typ; int256_typ;
     uint32_typ; uint64_typ; uint128_typ; uint256_typ;
     string_typ; bnum_typ; address_typ;
     hash_typ; msg_typ]  

let is_prim_type t = List.mem ~equal:(fun t1 t2 -> t1 = t2) prim_types t
