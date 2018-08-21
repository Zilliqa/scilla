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
let get_rep i = match i with Ident (_, l) -> l
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
let pp_mtype (kt, vt) = pp_typ (MapType(kt, vt))

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

let rec pp_literal l =
    let open Int in
    match l with
    | StringLit s -> "(String " ^ "\"" ^ s ^ "\"" ^ ")"
    (* (bit-width, value) *)
    | IntLit (b, i) -> "(Int" ^ (to_string b) ^ " " ^ i ^ ")"
    (* (bit-width, value) *)
    | UintLit (b, ui) -> "(Int" ^ (to_string b) ^ " " ^ ui ^ ")"
    | BNum b -> "(BNum " ^ b ^ ")"
    | Address a -> "(Address " ^ a ^ ")"
    | Sha256 h -> "(Hash " ^ h ^ ")"
    | Msg m ->
      let items = "[" ^
        List.fold_left m ~init:"" ~f:(fun a (s, l') ->
          let t = "(" ^ s ^ " : " ^ (pp_literal l') ^ ")" in
            if String.is_empty a then t else a ^ " ; " ^ t
          ) ^ "]" in
      ("(Message " ^ items ^ ")")
    | Map ((_, _), kv) ->
      (* we don't print mtype as that's printed for every entry. *)
      let items = "[" ^
        List.fold_left kv ~init:"" ~f:(fun a (k, v) ->
          let t = "(" ^ (pp_literal k) ^ " => " ^ (pp_literal v) ^ ")" in
            if String.is_empty a then t else a ^ "; " ^ t
          ) ^ "]" in
      ("(Map " ^ items ^ ")")
    | ADTValue (cn, _, al) ->
        (match cn with
        | "Cons" ->
          (* Print non-empty lists in a readable way. *)
          let rec pcons largs =
            if List.length largs = 0 then "(Nil)" else
            let this = (pp_literal (List.nth_exn largs 0)) ^ ", " in
            let next =
              if List.length largs <> 2 then "(Malformed List)" else
              (match (List.nth_exn largs 1) with
              | ADTValue(_, _, al') ->
                pcons al'
              | _ -> "(Malformed List") in
            (this ^ next)
          in
            "(List " ^ pcons al ^ ")"
        | "Zero" | "Succ" ->
            let rec counter largs =
              if List.length largs = 0 then "0" else
              if List.length largs <> 1 then "(Malformed Nat)" else
              (match (List.nth_exn largs 0) with
              | ADTValue (_, _, al') ->
                Int.to_string ((Int.of_string (counter al')) + 1)
              | _ -> "(Malformed Nat)")
            in
              "(Nat "^ (counter al) ^ ")"
        | _ ->
          (* Generic printing for other ADTs. *)
          "(" ^ cn ^
          List.fold_left al ~init:"" ~f:(fun a l' -> a ^ " " ^ (pp_literal l'))
          ^ ")"
        )

(* SExp version for structural printing. *)
let spp_literal l =
  sexp_of_literal l |> Sexplib.Sexp.to_string

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

let pp_named_literal_list nls =
  let ps = List.map nls
      ~f:(fun (n, l) -> sprintf "<%s : %s>" n (pp_literal l)) in
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
    let l = get_rep i in
      if (l.cnum <> -1) then Some l else None
  | _ -> None

(* SExp printing for Expr for structural printing. *)
let spp_expr e =
  sexp_of_expr sexp_of_loc e |> Sexplib.Sexp.to_string

(* TODO: add pretty printing for expressions. *)
let pp_expr e =
  spp_expr e

(*******************************************************)
(*                   Annotations                       *)
(*******************************************************)

module type Rep = sig
  type rep
  val get_loc : rep -> loc
  val mk_msg_payload_id_address : string -> rep ident
  val mk_msg_payload_id_uint128 : string -> rep ident

  val rep_of_sexp : Sexp.t -> rep
  val sexp_of_rep : rep -> Sexp.t
  
  (* TODO: This needs to be looked through properly *)
  val parse_rep : string -> rep
  val get_rep_str: rep -> string
end

(*******************************************************)
(*          Annotated scilla syntax                    *)
(*******************************************************)

module Contract (SR : Rep) (ER : Rep) = struct

  (*******************************************************)
  (*                   Statements                        *)
  (*******************************************************)

  type stmt_annot = stmt * SR.rep
  and stmt =
    | Load of ER.rep ident * ER.rep ident
    | Store of ER.rep ident * ER.rep ident
    | Bind of ER.rep ident * ER.rep expr_annot
    | MatchStmt of ER.rep ident * (ER.rep pattern * stmt_annot list) list
    | ReadFromBC of ER.rep ident * string
    | AcceptPayment
    | SendMsgs of ER.rep ident
    | CreateEvnt of string * ER.rep ident
    | Throw of ER.rep ident
  [@@deriving sexp]
  
  let stmt_str (srep : stmt_annot) =
    (* TODO: print rep as well? *)
    let (s, _) = srep in 
    let sexp = sexp_of_stmt s in
    Sexplib.Sexp.to_string sexp

  let stmt_rep srep = snd srep
  
  let stmt_loc s = SR.get_loc (stmt_rep s)

  (*******************************************************)
  (*                    Contracts                        *)
  (*******************************************************)

  type transition = 
    { tname   : SR.rep ident;
      tparams : (ER.rep ident  * typ) list;
      tbody   : stmt_annot list }

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

  let pp_cparams ps =
    let cs = List.map ps ~f:(fun (i, t) ->
        get_id i ^ " : " ^
        (sexp_of_typ t |> Sexplib.Sexp.to_string)) in
    "[" ^ (String.concat ~sep:", " cs) ^ "]"

end

