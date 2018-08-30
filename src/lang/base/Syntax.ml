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
  | Unit
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
  | Unit -> sprintf "()"
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

let address_length = 20
let hash_length = 32

type literal =
  | StringLit of string
  (* (bit-width, value) *)
  | IntLit of int * string
  (* (bit-width, value) *)
  | UintLit of int * string
  | BNum of string
  (* (bit-width, value) *)
  | ByStrX of int * string
  (* Hexadecimal byte string without a statically known length. *)
  | ByStr of string
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
    | ByStr s -> "(ByStr " ^ s ^ ")"
    | ByStrX (i, s) -> "(ByStr" ^ (to_string i) ^ " " ^ s ^ ")"
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

module ScillaSyntax (SR : Rep) (ER : Rep) = struct

(*******************************************************)
(*                   Expressions                       *)
(*******************************************************)

  type payload =
    | MTag of string 
    | MLit of literal
    | MVar of ER.rep ident
  [@@deriving sexp]

  type pattern =
    | Wildcard
    | Binder of ER.rep ident
    | Constructor of string * (pattern list)
  [@@deriving sexp]

  type expr_annot = expr * ER.rep
  and expr =
    | Literal of literal
    | Var of ER.rep ident
    | Let of ER.rep ident * typ option * expr_annot * expr_annot
    | Message of (string * payload) list
    | Fun of ER.rep ident * typ * expr_annot
    | App of ER.rep ident * ER.rep ident list
    | Constr of string * typ list * ER.rep ident list
    | MatchExpr of ER.rep ident * (pattern * expr_annot) list
    | Builtin of ER.rep ident * ER.rep ident list 
    (* Advanced features: to be added in Scilla 0.2 *)                 
    | TFun of ER.rep ident * expr_annot
    | TApp of ER.rep ident * typ list
    (* Fixpoint combinator: used to implement recursion principles *)                 
    | Fixpoint of ER.rep ident * typ * expr_annot
  [@@deriving sexp]

  let expr_rep erep = snd erep
      
  let expr_loc erep =
    let l = ER.get_loc (expr_rep erep) in
    if l.cnum <> -1
    then Some l
    else None

  (* SExp printing for Expr for structural printing. *)
  let spp_expr e =
    sexp_of_expr e |> Sexplib.Sexp.to_string

  (* TODO: add pretty printing for expressions. *)
  let pp_expr e =
    spp_expr e

  (*******************************************************)
  (*                   Statements                        *)
  (*******************************************************)

  type stmt_annot = stmt * SR.rep
  and stmt =
    | Load of ER.rep ident * ER.rep ident
    | Store of ER.rep ident * ER.rep ident
    | Bind of ER.rep ident * expr_annot
    | MatchStmt of ER.rep ident * (pattern * stmt_annot list) list
    | ReadFromBC of ER.rep ident * string
    | AcceptPayment
    | SendMsgs of ER.rep ident
    | CreateEvnt of ER.rep ident
    | Throw of ER.rep ident
  [@@deriving sexp]
  
  let stmt_rep srep = snd srep
  
  let stmt_loc s = SR.get_loc (stmt_rep s)

  let spp_stmt s =
    sexp_of_stmt s |> Sexplib.Sexp.to_string

  let pp_stmt s =
    spp_stmt s
      
  (*******************************************************)
  (*                    Contracts                        *)
  (*******************************************************)

  type transition = 
    { tname   : SR.rep ident;
      tparams : (ER.rep ident  * typ) list;
      tbody   : stmt_annot list }

  type lib_entry =
    { lname : ER.rep ident;
      lexp  : expr_annot }

  type library =
    { lname : SR.rep ident;
      lentries : lib_entry list }
  
  type contract =
    { cname   : SR.rep ident;
      cparams : (ER.rep ident  * typ) list;
      cfields : (ER.rep ident * typ * expr_annot) list;
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

  (****************************************************************)
  (*                  Type substitutions                          *)
  (****************************************************************)

  (* Return free tvars in tp *)
  let free_tvars tp =
    let add vs tv = tv :: List.filter ~f:(fun v -> v = tv) vs in
    let rem vs tv = List.filter ~f:(fun v -> v <> tv) vs in
    let rec go t acc = (match t with
        | PrimType _ | Unit -> acc
        | MapType (kt, vt) -> go kt acc |> go vt
        | FunType (at, rt) -> go at acc |> go rt
        | TypeVar n -> add acc n
        | ADT (_, ts) ->
            List.fold_left ts ~init:acc ~f:(fun z tt -> go tt z)
        | PolyFun (arg, bt) ->
            let acc' = go bt acc in
            rem acc' arg) in
    go tp []

  let mk_fresh_var taken init =
    let tmp = ref init in
    let counter = ref 1 in
    while List.mem taken !tmp ~equal:(fun a b -> a = b) do
      let cnt = !counter in
      tmp := init ^ (Int.to_string cnt);
      counter := cnt + 1;
    done;
    !tmp


  let rec subst_type_in_type tvar tp tm = match tm with
    | PrimType _ | Unit as p -> p
    (* Make sure the map's type is still primitive! *)
    | MapType (kt, vt) -> 
        let kts = subst_type_in_type tvar tp kt in
        let vts = subst_type_in_type tvar tp vt in
        MapType (kts, vts)
    | FunType (at, rt) -> 
        let ats = subst_type_in_type tvar tp at in
        let rts = subst_type_in_type tvar tp rt in
        FunType (ats, rts)
    | TypeVar n as tv ->
        if tvar = n then tp else tv
    | ADT (s, ts) ->
        let ts' = List.map ts ~f:(fun t -> subst_type_in_type tvar tp t) in
        ADT (s, ts')
    | PolyFun (arg, t) as pf -> 
        if tvar = arg then pf
        else PolyFun (arg, subst_type_in_type tvar tp t)

  let subst_types_in_type sbst tm =
    List.fold_left sbst ~init:tm
      ~f:(fun acc (tvar, tp) -> subst_type_in_type tvar tp acc)

  let rec refresh_tfun t taken = match t with
    | MapType (kt, vt) -> MapType (kt, refresh_tfun vt taken)
    | FunType (at, rt) ->
        FunType (refresh_tfun at taken, refresh_tfun rt taken)
    | ADT (n, ts) ->
        let ts' = List.map ts ~f:(fun w -> refresh_tfun w taken) in
        ADT (n, ts')
    | PrimType _ | TypeVar _ | Unit -> t
    | PolyFun (arg, bt) ->
        let arg' = mk_fresh_var taken arg in
        let tv_new = TypeVar arg' in
        let bt1 = subst_type_in_type arg tv_new bt in
        let taken' = arg' :: taken in
        let bt2 = refresh_tfun bt1 taken' in
        PolyFun (arg', bt2)

  (* Alpha renaming to canonical (pre-determined) names. *)
  let canonicalize_tfun t =
    let taken = free_tvars t in
    let get_new_name counter = "'_A" ^ Int.to_string counter in
    let rec refresh t taken counter = match t with
      | MapType (kt, vt) -> MapType (kt, refresh vt taken counter)
      | FunType (at, rt) ->
          FunType (refresh at taken counter, refresh rt taken counter)
      | ADT (n, ts) ->
          let ts' = List.map ts ~f:(fun w -> refresh w taken counter) in
          ADT (n, ts')
      | PrimType _ | TypeVar _ | Unit -> t
      | PolyFun (arg, bt) ->
          let arg' = get_new_name counter in
          let tv_new = TypeVar arg' in
          let bt1 = subst_type_in_type arg tv_new bt in
          let taken' = arg' :: taken in
          let bt2 = refresh bt1 taken' (counter+1) in
          PolyFun (arg', bt2)
    in
    refresh t taken 1

  (* The same as above, but for a variable with locations *)
  let subst_type_in_type' tv = subst_type_in_type (get_id tv)

  let rec subst_type_in_literal tvar tp l = match l with
    | Map ((kt, vt), ls) -> 
        let kts = subst_type_in_type' tvar tp kt in
        let vts = subst_type_in_type' tvar tp vt in
        let ls' = List.map ls ~f:(fun (k, v) ->
            let k' = subst_type_in_literal tvar tp k in
            let v' = subst_type_in_literal tvar tp v in 
            (k', v')) in
        Map ((kts, vts), ls')
    | ADTValue (n, ts, ls) ->
        let ts' = List.map ts ~f:(fun t -> subst_type_in_type' tvar tp t) in
        let ls' = List.map ls ~f:(fun l -> subst_type_in_literal tvar tp l) in
        ADTValue (n, ts', ls')
    | _ -> l


  (* Substitute type for a type variable *)
  let rec subst_type_in_expr tvar tp erep =
    let (e, rep) = erep in
    match e with
    | Literal l -> (Literal (subst_type_in_literal tvar tp l), rep)
    | Var _ as v -> (v, rep)
    | Fun (f, t, body) ->
        let t_subst = subst_type_in_type' tvar tp t in 
        let body_subst = subst_type_in_expr tvar tp body in
        (Fun (f, t_subst, body_subst), rep)
    | TFun (tv, body) as tf ->
        if get_id tv = get_id tvar
        then (tf, rep)
        else 
          let body_subst = subst_type_in_expr tvar tp body in
          (TFun (tv, body_subst), rep)
    | Constr (n, ts, es) ->
        let ts' = List.map ts ~f:(fun t -> subst_type_in_type' tvar tp t) in
        (Constr (n, ts', es), rep)
    | App _ as app -> (app, rep)
    | Builtin _ as bi -> (bi, rep)
    | Let (i, tann, lhs, rhs) ->
        let tann' = Option.map tann ~f:(fun t -> subst_type_in_type' tvar tp t) in
        let lhs' = subst_type_in_expr tvar tp lhs in
        let rhs' = subst_type_in_expr tvar tp rhs in
        (Let (i, tann', lhs', rhs'), rep)
    | Message _ as m -> (m, rep)
    | MatchExpr (e, cs) ->
        let cs' = List.map cs ~f:(fun (p, b) -> (p, subst_type_in_expr tvar tp b)) in
        (MatchExpr(e, cs'), rep)
    | TApp (tf, tl) -> 
        let tl' = List.map tl ~f:(fun t -> subst_type_in_type' tvar tp t) in
        (TApp (tf, tl'), rep)
    | Fixpoint (f, t, body) ->
        let t' = subst_type_in_type' tvar tp t in
        let body' = subst_type_in_expr tvar tp body in
        (Fixpoint (f, t', body'), rep)

  (****************************************************************)
  (*                  Better error reporting                      *)
  (****************************************************************)
  let get_failure_msg erep phase opt =
    let (e, rep) = erep in
    let locstr = get_loc_str (ER.get_loc rep) in
    match e with
    | Literal _ ->
        sprintf "[%s] Type error in literal. %s\n"
          locstr phase
    | Var i ->
        sprintf "[%s] Type error in variable `%s`:\n"
          locstr (get_id i)
    | Let (i, _, _, _) ->
        sprintf "[%s] Type error in the initialiser of `%s`:\n"
          locstr (get_id i)
    | Message _ ->
        sprintf "[%s] Type error in message.\n"
          locstr
    | Fun _ ->
        sprintf "[%s] Type error in function:\n"
          locstr
    | App (f, _) ->
        sprintf "[%s] Type error in application of `%s`:\n"
          locstr (get_id f)
    | Constr (s, _, _) ->
        sprintf "[%s] Type error in constructor `%s`:\n"
          locstr s
    | MatchExpr (x, _) ->
        sprintf
          "[%s] Type error in pattern matching on `%s`%s (or one of its branches):\n"
          locstr (get_id x) opt 
    | Builtin (i, _) ->
        sprintf "[%s] Type error in built-in application of `%s`:\n"
          locstr (get_id i)
    | TApp (tf, _) ->
        sprintf "[%s] Type error in type application of `%s`:\n"
          locstr (get_id tf)
    | TFun (tf, _) ->
        sprintf "[%s] Type error in type function `%s`:\n"
          locstr (get_id tf)
    | Fixpoint (f, _, _) ->
        sprintf "Type error in fixpoint application with an argument `%s`:\n"
          (get_id f)              

  let get_failure_msg_stmt srep phase opt =
    let (s, rep) = srep in
    let locstr = get_loc_str (SR.get_loc rep) in
    match s with
    | Load (x, f) ->
        sprintf "[%s] Type error in reading value of `%s` into `%s`:\n %s"
          locstr (get_id f) (get_id x) phase
    | Store (f, r) ->
        sprintf "[%s] Type error in storing value of `%s` into the field `%s`:\n"
          locstr (get_id r) (get_id f)
    | Bind (x, _) ->
        sprintf "[%s] Type error in the binding to into `%s`:\n"
          locstr (get_id x)
    | MatchStmt (x, _) ->
        sprintf
          "[%s] Type error in pattern matching on `%s`%s (or one of its branches):\n"
          locstr (get_id x) opt 
    | ReadFromBC (x, _) ->
        sprintf "[%s] Error in reading from blockchain state into `%s`:\n"
          locstr (get_id x)
    | AcceptPayment ->
        sprintf "[%s] Error in accepting payment\n"
          locstr
    | SendMsgs i ->
        sprintf "[%s] Error in sending messages `%s`:\n"
          locstr (get_id i)
    | CreateEvnt i ->
        sprintf "[%s] Error in create event `%s`:\n"
          locstr (get_id i)
    | Throw i ->
        sprintf "[%s] Error in throw of '%s':\n"
          locstr (get_id i)

  let wrap_with_info msg res = match res with
    | Ok _ -> res
    | Error msg' -> Error (sprintf "%s%s" msg msg')

  let wrap_err e phase ?opt:(opt = "") = wrap_with_info (get_failure_msg e phase opt)

  let wrap_serr s phase ?opt:(opt = "") =
    wrap_with_info (get_failure_msg_stmt s phase opt)
  
end

