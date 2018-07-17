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

val prim_types : typ list
val is_prim_type : typ -> bool

val int32_typ : typ
val int64_typ : typ
val int128_typ : typ
val uint32_typ : typ
val uint64_typ : typ
val uint128_typ : typ
val string_typ : typ
val bnum_typ : typ
val address_typ : typ
val hash_typ : typ
val msg_typ : typ

