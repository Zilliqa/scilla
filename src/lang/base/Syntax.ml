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
open Sexplib.Std

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

let mk_ident s = Ident (s, dummy_loc)      

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

let address_length = 40
let hash_length = 64

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

let pp_literal_map s =
  let ps = List.map s
      ~f:(fun (k, v) -> sprintf " [%s -> %s]" k (pp_literal v)) in
  let cs = String.concat ~sep:",\n " ps in
  sprintf "{%s }" cs
    
let pp_literal_list ls =
  let ps = List.map ls
      ~f:(fun l -> sprintf " %s" (pp_literal l)) in
  let cs = String.concat ~sep:",\n " ps in
  sprintf "[ %s]" cs


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

type 'rep expr_annot = 'rep expr * 'rep
and 'rep expr =
  | Literal of literal
  | Var of 'rep ident
  | Let of 'rep ident * typ option * 'rep expr_annot * 'rep expr_annot
  | Message of (string * 'rep payload) list
  | Fun of 'rep ident * typ * 'rep expr_annot
  | App of 'rep ident * 'rep ident list
  | Constr of string * typ list * 'rep ident list
  | MatchExpr of 'rep ident * ('rep pattern * 'rep expr_annot) list
  | Builtin of 'rep ident * 'rep ident list 
  (* Advanced features: to be added in Scilla 0.2 *)                 
  | TFun of 'rep ident * 'rep expr_annot
  | TApp of 'rep ident * typ list
  (* Fixpoint combinator: used to implement recursion principles *)                 
  | Fixpoint of 'rep ident * typ * 'rep expr_annot
[@@deriving sexp]

let expr_loc (e : 'rep expr) : loc option =
  match e with
  | Fun (i, _, _) | App (i, _) | Builtin (i, _)
  | MatchExpr (i, _) | TFun (i, _) | TApp (i, _) -> 
    let l = get_loc i in
      if (l.cnum <> -1) then Some l else None
  | _ -> None

(* TODO: make normal pretty-printing *)
let expr_str (erep : 'rep expr_annot) =
  let (e, _) = erep in
  let eexp = sexp_of_expr sexp_of_loc e in
  Sexplib.Sexp.to_string eexp

(*******************************************************)
(*                   Statements                        *)
(*******************************************************)

(* module Stmt = functor (SR : Rep) (ER : Rep) -> struct
  type stmt =
    | Load of ER.rep ident * ER.rep ident * SR.rep
    | Store of ER.rep ident * ER.rep ident * SR.rep
    | Bind of ER.rep ident * ER.rep expr_annot * SR.rep
    | MatchStmt of ER.rep ident * (ER.rep pattern * stmt list) list * SR.rep
    | ReadFromBC of ER.rep ident * string * SR.rep
    | AcceptPayment of SR.rep
    | SendMsgs of ER.rep ident * SR.rep
    | Event of string * string * SR.rep
    | Throw of ER.rep ident * SR.rep

  let get_rep s =
    match s with
    | Load (_, _, rep)
    | Store (_, _, rep)
    | Bind (_, _, rep)
    | MatchStmt (_, _, rep)
    | ReadFromBC (_, _, rep)
    | AcceptPayment rep
    | SendMsgs (_, rep)
    | Event (_, _, rep)
    | Throw (_, rep) -> rep

  include SR
end
*)



type ('rep, 'erep) stmt_annot = ('rep, 'erep) stmt * 'rep
and ('rep, 'erep) stmt =
  | Load of 'erep ident * 'erep ident
  | Store of 'erep ident * 'erep ident
  | Bind of 'erep ident * 'erep expr_annot
  | MatchStmt of 'erep ident * ('erep pattern * ('rep, 'erep) stmt_annot list) list
  | ReadFromBC of 'erep ident * string
  | AcceptPayment
  | SendMsgs of 'erep ident
  | Event of string * string
  | Throw of 'erep ident
[@@deriving sexp]

let stmt_str (srep : ('rep, 'erep) stmt_annot) =
  let (s, _) = srep in 
  let sexp = sexp_of_stmt sexp_of_loc sexp_of_loc s in
  Sexplib.Sexp.to_string sexp

let stmt_loc (s : ('rep, 'erep) stmt) : loc option = 
  match s with
  | Load (i, _) | Store(i, _) | ReadFromBC (i, _) 
  | MatchStmt (i, _)
  | SendMsgs i -> 
    let l = get_loc i in
      if (l.cnum <> -1) then Some l else None
  | _ -> None


(*******************************************************)
(*                   Annotations                       *)
(*******************************************************)

module type Rep = sig
  type rep
end

(*******************************************************)
(*                    Contracts                        *)
(*******************************************************)

module type Contract_sig = sig
  type exp_nametype
  type stmt_nametype
  type param_type
  type exp_type
  type stmt_type
  type field_type
  type libs_type
  type erep
  type srep
  
  type transition = { tname : stmt_nametype;
                      tparams : param_type list;
                      tbody : stmt_type list }

  type lib_entry = { lname : exp_nametype;
                     lexp : exp_type }

  type library = { lname : stmt_nametype;
                   lentries : lib_entry list }

  type contract = { cname : stmt_nametype;
                    cparams : param_type list;
                    cfields : field_type list;
                    ctrans : transition list }

  type cmodule = { cname : stmt_nametype;
                   libs : library option;
                   elibs : libs_type list;
                   contr : contract }

end

module Contract (SR : Rep) (ER : Rep) : Contract_sig = struct
  type exp_nametype = ER.rep ident
  type stmt_nametype = SR.rep ident
  type param_type = (ER.rep ident * typ)
  type exp_type = ER.rep expr_annot
  type stmt_type = (SR.rep, ER.rep) stmt_annot
  type field_type = (ER.rep ident * typ * ER.rep expr_annot)
  type libs_type = SR.rep ident
  type erep = ER.rep
  type srep = SR.rep
  
  type transition = 
    { tname   : SR.rep ident;
      tparams : (ER.rep ident  * typ) list;
      tbody   : (SR.rep, ER.rep) stmt_annot list }

  type lib_entry =
    { lname : ER.rep ident;
      lexp  : ER.rep expr_annot }

  type library =
    { lname : SR.rep ident;
      lentries : lib_entry list }
  
  type contract =
    { cname   : SR.rep ident;
      cparams : (ER.rep ident  * typ) list;
      cfields : (ER.rep ident * typ * ER.rep expr_annot) list;
      ctrans  : transition list; }

  (* Contract module: libary + contract definiton *)
  type cmodule =
    { cname : SR.rep ident;
      libs  : library option;     (* lib functions defined in the module *)
      elibs : SR.rep ident list;  (* list of imports / external libs *)
      contr : contract }

end

(*
type ('rep, 'erep) transition = 
  { tname   : 'erep ident;
    tparams : ('erep ident  * typ) list;
    tbody   : ('rep, 'erep) stmt_annot list }
[@@deriving sexp]

type 'rep lib_entry =
  { lname : 'rep ident;
    lexp  : 'rep expr_annot }
[@@deriving sexp]

type 'rep library =
  { lname : 'rep ident;
    lentries : 'rep lib_entry list }
[@@deriving sexp]
  
type ('rep, 'erep) contract =
  { cname   : 'erep ident;
    cparams : ('erep ident  * typ) list;
    cfields : ('erep ident * typ * 'erep expr_annot) list;
    ctrans  : ('rep, 'erep) transition list; }
[@@deriving sexp]

let pp_cparams ps =
  let cs = List.map ps ~f:(fun (i, t) ->
      get_id i ^ " : " ^
      (sexp_of_typ t |> Sexplib.Sexp.to_string)) in
  "[" ^ (String.concat ~sep:", " cs) ^ "]"

(* Contract module: libary + contract definiton *)
type ('rep, 'erep) cmodule =
  { cname : 'erep ident;
    libs  : ('erep library) option;     (* lib functions defined in the module *)
    elibs : 'erep ident list;  (* list of imports / external libs *)
    contr : ('rep, 'erep) contract }
[@@deriving sexp]
*)
