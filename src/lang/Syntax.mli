(*
 * Copyright (c) 2018 - present Zilliqa, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open Sexplib.Std

type 'rep ident =
  | Ident of string * 'rep
  [@@deriving sexp]

type typ  =
  | PrimType of string
  | MapType of typ * typ
  | FunType of typ * typ
  | ADT of string * typ list
  | TypeVar of string
  | PolyFun of string * typ
[@@deriving sexp]

type pattern =
  | Wildcard
  | Binder of string
  | Constructor of string * (pattern list)
[@@deriving sexp]

type literal = 
  | IntLit of int
  | BNum of int
  | Address of string
  | Sha256 of string
  | Map of literal * literal list
[@@deriving sexp]

type payload =
  | MTag of string
  | MLit of literal
  | MVar of string
[@@deriving sexp]

type 'rep expr =
  | Let of 'rep ident * typ option * 'rep expr * 'rep expr
  | Var of 'rep ident
  | Literal of literal
  | Message of (string * payload) list
  | Builtin of 'rep ident * 'rep ident list 
  | Fun of 'rep ident * typ * 'rep expr
  | App of 'rep ident * 'rep ident list
  | TFun of 'rep ident * 'rep expr
  | TApp of 'rep ident * typ list
  | Constr of string * typ list * 'rep ident list
  | Match of 'rep ident * (pattern * 'rep expr) list
[@@deriving sexp]

type 'rep lib_entry =
  { lname : 'rep ident ;
    lexp  : 'rep expr }
[@@deriving sexp]
