(*
 * Copyright (c) 2018 - present Zilliqa, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open Sexplib.Std

type expr = 
    Var of string
  | Not of expr
  | And of expr * expr
  | Or of expr * expr [@@deriving sexp]

val exp_to_string : expr -> string      
