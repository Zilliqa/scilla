(*
 * Copyright (c) 2018 - present. 
 * Zilliqa, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)


open Core
open Syntax
open Sexplib.Std
open Yojson
open Big_int
open Stdint

let int32_str = "Int32"
let int64_str = "Int64"
let int128_str = "Int128"
let uint32_str = "Uint32"
let uint64_str = "Uint64"
let uint128_str = "Uint128"
let string_str = "String"
let bnum_str = "BNum"
let address_str = "Address"
let hash_str = "Hash"
let msg_str = "Message"

let int32_typ = PrimType int32_str
let int64_typ = PrimType int64_str
let int128_typ = PrimType int128_str
let uint32_typ = PrimType uint32_str
let uint64_typ = PrimType uint64_str
let uint128_typ = PrimType uint128_str
let string_typ = PrimType string_str
let bnum_typ = PrimType bnum_str
let address_typ = PrimType address_str
let hash_typ = PrimType hash_str
let msg_typ = PrimType msg_str
  
let prim_types =
    [int32_typ; int64_typ; int128_typ;
     uint32_typ; uint64_typ; uint128_typ;
     string_typ; bnum_typ; address_typ;
     hash_typ; msg_typ]  

let is_prim_type t = List.mem ~equal:(fun t1 t2 -> t1 = t2) prim_types t
