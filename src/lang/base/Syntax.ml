(*
 * Copyright (c) 2018 - present , Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open Core
open Sexplib.Std
open Yojson
open Big_int
open Stdint

exception SyntaxError of string

(* Location info, since Lexing.position does not support sexp *)
type loc = {
  fname : string; (* file name *)
  lnum : int;     (* line number *)
  bol : int;      (* beginning of line *)
  cnum : int;     (* column number *)
}
[@@deriving sexp]

let toLoc (p : Lexing.position) : loc = {
  fname = p.pos_fname;
  lnum = p.pos_lnum; 
  bol = p.pos_bol; 
  cnum = p.pos_cnum;
}

let dummy_loc =
  toLoc Lexing.dummy_pos

type 'rep ident =
  | Ident of string * 'rep
[@@deriving sexp]

let asId i = Ident (i, dummy_loc)
let asIdL i loc = Ident(i, loc)

let get_id i = match i with Ident (x, _) -> x
let get_loc i : loc = match i with Ident (_, l) -> l
let get_loc_str (l : loc) : string =
  l.fname ^ ":" ^ Int.to_string l.lnum ^ 
      ":" ^ Int.to_string (l.cnum - l.bol + 1)

type bigint = Big_int.big_int

type typ  =
  | PrimType of string
  | MapType of typ * typ
  | FunType of typ * typ
  | ADT of string * typ list
  | TypeVar of string
  | PolyFun of string * typ
[@@deriving sexp]

let pp_typ t = sexp_of_typ t |> Sexplib.Sexp.to_string

type 'rep pattern =
  | Wildcard
  | Binder of 'rep ident
  | Constructor of string * ('rep pattern list)
[@@deriving sexp]

(* FIXME: Both integers and block numbers are encoded as strings,
 * in order to accommodate big_ints. The reason why I didn't use
 * big_ints is because for some reason the sexp pre-processor does
 * not support them. Once we write a custom pretty-printer for
 * literals and expressions, there will be no need for this atrocity,
 * and we can switch back to big ints. *)

(* The first component is a primitive type *)
type mtype = typ * typ
[@@deriving sexp]

type literal =
  | StringLit of string
  (* (bit-width, value) *)
  | IntLit of int * string
  (* (bit-width, value) *)
  | UintLit of int * string
  | BNum of string
  | Address of string
  | Sha256 of string
  (* Message: an associative array *)    
  | Msg of (string * literal) list
  (* A dynamic map of literals *)    
  | Map of mtype * (literal * literal) list
  (* A constructor in HNF *)      
  | ADTValue of string * typ list * literal list
[@@deriving sexp]

let pp_literal l = sexp_of_literal l |> Sexplib.Sexp.to_string

type 'rep payload =
  | MTag of string 
  | MLit of literal
  | MVar of 'rep ident
[@@deriving sexp]

type 'rep expr =
  | Literal of literal
  | Var of 'rep ident
  | Let of 'rep ident * typ option * 'rep expr * 'rep expr
  | Message of (string * 'rep payload) list
  | Fun of 'rep ident * typ * 'rep expr
  | App of 'rep ident * 'rep ident list
  | Constr of string * typ list * 'rep ident list
  | MatchExpr of 'rep ident * ('rep pattern * 'rep expr) list
  | Builtin of 'rep ident * 'rep ident list 
  (* Advanced features: to be added in Scilla 0.2 *)                 
  | TFun of 'rep ident * 'rep expr
  | TApp of 'rep ident * typ list
  (* Fixpoint combinator: used to implement recursion principles *)                 
  | Fixpoint of 'rep ident * typ * 'rep expr
[@@deriving sexp]

type 'rep stmt =
  | Load of 'rep ident * 'rep ident
  | Store of 'rep ident * 'rep ident
  | Bind of 'rep ident * 'rep expr
  | MatchStmt of 'rep ident * ('rep pattern * 'rep stmt list) list
  | ReadFromBC of 'rep ident * string
  | AcceptPayment
  | SendMsgs of 'rep ident
  | Event of string * string
  | Throw of 'rep ident
[@@deriving sexp]

type 'rep transition = 
  { tname   : 'rep ident;
    tparams : ('rep ident  * typ) list;
    tbody   : 'rep stmt list }
[@@deriving sexp]

type 'rep lib_entry =
  { lname : 'rep ident;
    lexp  : 'rep expr }
[@@deriving sexp]

type 'rep contract =
  { cname   : 'rep ident;
    cparams : ('rep ident  * typ) list;
    cfields : ('rep ident * typ * 'rep expr) list;
    ctrans  : 'rep transition list; }
[@@deriving sexp]

let pp_cparams ps =
  let cs = List.map ps ~f:(fun (i, t) ->
      get_id i ^ " : " ^
      (sexp_of_typ t |> Sexplib.Sexp.to_string)) in
  "[" ^ (String.concat ~sep:", " cs) ^ "]"

(* Contract module: libary + contract definiton *)
type 'rep cmodule =
  { cname : 'rep ident;
    libs  : 'rep lib_entry list;
    contr : 'rep contract }
[@@deriving sexp]

let stmt_loc (s : 'rep stmt) : loc option = 
  match s with
  | Load (i, _) | Store(i, _) | ReadFromBC (i, _) 
  | MatchStmt (i, _)
  | SendMsgs i -> 
    let l = get_loc i in
      if (l.cnum <> -1) then Some l else None
  | _ -> None

let expr_loc (e : 'rep expr) : loc option =
  match e with
  | Fun (i, _, _) | App (i, _) | Builtin (i, _)
  | MatchExpr (i, _) | TFun (i, _) | TApp (i, _) -> 
    let l = get_loc i in
      if (l.cnum <> -1) then Some l else None
  | _ -> None

(* TODO: replace with proper type inference *)
let literal_tag l = match l with
  | StringLit _ -> "String"
  | IntLit (w, _) -> "Int" ^ (Int.to_string w)
  | UintLit (w,_) -> "Uint" ^ (Int.to_string w)
  | BNum _ -> "BNum"
  | Address _ -> "Address"
  | Sha256 _ -> "Hash"
  | Msg _ -> "Message"
  | Map _ -> "Map"
  | ADTValue _ -> "ADT"

(* Validate Int* and Uint* literals (wx, x), whether the
   string x they contain can be represented in wx bits  *)
let validate_int_literal i =
  try
    match i with
    | IntLit (wx, x) ->
      (match wx with
      | 32 -> Int32.to_string (Int32.of_string x) = x
      | 64 -> Int64.to_string (Int64.of_string x) = x
      | 128 -> Int128.to_string (Int128.of_string x) = x
      | _ -> false
      )
    | UintLit (wx, x) ->
      (match wx with
      | 32 -> Uint32.to_string (Uint32.of_string x) = x
      | 64 -> Uint64.to_string (Uint64.of_string x) = x
      | 128 -> Uint128.to_string (Uint128.of_string x) = x
      | _ -> false
      )
    | _ -> false
  with
  | _ -> false

(* Given an integer type (as string) and the value (as string),
   build IntLit or UintLit out of it. TODO: Validate. *)
let build_int t v = 
  let validator_wrapper l = 
    if validate_int_literal l then Some l else None
  in
  match t with
  | "Int32" -> validator_wrapper (IntLit(32, v))
  | "Int64" -> validator_wrapper (IntLit(64, v))
  | "Int128" -> validator_wrapper (IntLit(128, v))
  | "Uint32" -> validator_wrapper (UintLit(32, v))
  | "Uint64" -> validator_wrapper (UintLit(64, v))
  | "Uint128" -> validator_wrapper (UintLit(128, v))
  | _ -> None

let is_int_type = function
  | "Int32" | "Int64" | "Int128" -> true
  | _ -> false

let is_uint_type = function
  | "Uint32" | "Uint64" | "Uint128" -> true
  | _ -> false
