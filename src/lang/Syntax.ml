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

type 'rep typ  =
  | PrimType of string
  | MapType of 'rep  typ * 'rep typ
  | FunType of 'rep  typ * 'rep typ
  | ADT of string * 'rep typ list
  | TypeVar of string
  | PolyFun of string * 'rep typ
[@@deriving sexp]

type 'rep pattern =
  | Wildcard
  | Binder of string
  | Constructor of 'rep pattern list 
[@@deriving sexp]

type 'rep literal = 
  | IntLit of int
  | BNum of int
  | Address of string
  | Sha256 of string
  | EmpMap of 'rep typ * 'rep typ
[@@deriving sexp]

type 'rep expr =
  | Let of 'rep ident * 'rep typ option * 'rep expr * 'rep expr
  | Var of 'rep ident
  | Literal of 'rep literal
  | Message of ('rep ident * 'rep expr) list
  | Builtin of 'rep ident * 'rep ident list 
  | Fun of 'rep ident * 'rep typ * 'rep expr
  | App of 'rep ident * 'rep ident list
  | TFun of 'rep ident * 'rep expr
  | TApp of 'rep ident * string list
  | Constr of string * ('rep typ list) * ('rep expr list)
  | Match of 'rep ident * ('rep pattern list)
[@@deriving sexp]

let rec exp_to_string e = match e with
  | _ -> "expr"
    
(*     | And (p1, p2) -> "(" ^ exp_to_string p1 ^ " ∧ " ^ exp_to_string p2 ^ ")" *)
(*     | Or (p1, p2) -> "(" ^ exp_to_string p1 ^ " ∨ " ^ exp_to_string p2 ^ ")" *)
(*     | Var s -> s *)
