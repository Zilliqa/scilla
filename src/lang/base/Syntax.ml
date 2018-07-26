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
open Sexplib.Std
open Yojson
open Big_int
open Stdint
open Integer256

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

(*******************************************************)
(*                         Types                       *)
(*******************************************************)
                
type typ  =
  | PrimType of string
  | MapType of typ * typ
  | FunType of typ * typ
  | ADT of string * typ list
  | TypeVar of string
  | PolyFun of string * typ
[@@deriving sexp]

let rec pp_typ t = match t with
  | PrimType t -> t
  | MapType (kt, vt) ->
      sprintf "Map (%s) (%s)" (pp_typ kt) (pp_typ vt )
  | ADT (name, targs) ->
      let elems = name :: (List.map targs
          ~f:(fun t -> sprintf "(%s)" (pp_typ t)))
      in
      String.concat ~sep:" " elems
  | FunType (at, vt) -> sprintf "%s -> %s" (with_paren at) (pp_typ vt)
  | TypeVar tv -> tv
  | PolyFun (tv, bt) -> sprintf "forall %s. %s" tv (pp_typ bt)
and with_paren t = match t with
  | FunType _ | PolyFun _ -> sprintf "(%s)" (pp_typ t)
  | _ -> pp_typ t

(*******************************************************)
(*                      Literals                       *)
(*******************************************************)

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

(*******************************************************)
(*                   Expressions                       *)
(*******************************************************)

type 'rep payload =
  | MTag of string 
  | MLit of literal
  | MVar of 'rep ident
[@@deriving sexp]

type 'rep pattern =
  | Wildcard
  | Binder of 'rep ident
  | Constructor of string * ('rep pattern list)
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

let expr_loc (e : 'rep expr) : loc option =
  match e with
  | Fun (i, _, _) | App (i, _) | Builtin (i, _)
  | MatchExpr (i, _) | TFun (i, _) | TApp (i, _) -> 
    let l = get_loc i in
      if (l.cnum <> -1) then Some l else None
  | _ -> None

(* TODO: make normal pretty-printing *)
let expr_str e =
  sexp_of_expr sexp_of_loc e |> Sexplib.Sexp.to_string

(*******************************************************)
(*                   Statements                        *)
(*******************************************************)

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

let stmt_str s =
  sexp_of_stmt sexp_of_loc s |> Sexplib.Sexp.to_string

let stmt_loc (s : 'rep stmt) : loc option = 
  match s with
  | Load (i, _) | Store(i, _) | ReadFromBC (i, _) 
  | MatchStmt (i, _)
  | SendMsgs i -> 
    let l = get_loc i in
      if (l.cnum <> -1) then Some l else None
  | _ -> None

(*******************************************************)
(*                    Contracts                        *)
(*******************************************************)

type 'rep transition = 
  { tname   : 'rep ident;
    tparams : ('rep ident  * typ) list;
    tbody   : 'rep stmt list }
[@@deriving sexp]

type 'rep lib_entry =
  { lname : 'rep ident;
    lexp  : 'rep expr }
[@@deriving sexp]

type 'rep library =
  { lname : 'rep ident;
    lentries : 'rep lib_entry list }
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
    libs  : 'rep library;     (* lib functions defined in the module *)
    elibs : 'rep ident list;  (* list of imports / external libs *)
    contr : 'rep contract }
[@@deriving sexp]
