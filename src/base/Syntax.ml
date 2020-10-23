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

open Core_kernel
open Sexplib.Std
open ErrorUtils
open Literal
open GasCharge

exception SyntaxError of string * loc

(* Version of the interpreter (major, minor, patch) *)
let scilla_version = (0, 9, 0)

let address_length = 20

let hash_length = 32

(* Builtins *)
type builtin =
  | Builtin_eq
  | Builtin_concat
  | Builtin_substr
  | Builtin_strlen
  | Builtin_to_string
  | Builtin_blt
  | Builtin_badd
  | Builtin_bsub
  | Builtin_to_uint256
  | Builtin_sha256hash
  | Builtin_keccak256hash
  | Builtin_ripemd160hash
  | Builtin_to_bystr
  | Builtin_bech32_to_bystr20
  | Builtin_bystr20_to_bech32
  | Builtin_schnorr_verify
  | Builtin_ecdsa_verify
  | Builtin_alt_bn128_G1_add
  | Builtin_alt_bn128_G1_mul
  | Builtin_alt_bn128_pairing_product
  (* https://github.com/Zilliqa/scilla/pull/486#discussion_r266069221 *)
  (*
  | Builtin_ec_gen_key_pair (* in gas coster only *)
  | Builtin_schnorr_gen_key_pair (* in cashflow checker only *)
  | Builtin_schnorr_sign (* in cashflow checker only *)
  | Builtin_ecdsa_sign (* in gas coster only *)
  *)
  | Builtin_contains
  | Builtin_put
  | Builtin_get
  | Builtin_remove
  | Builtin_to_list
  | Builtin_size
  | Builtin_lt
  | Builtin_add
  | Builtin_sub
  | Builtin_mul
  | Builtin_div
  | Builtin_rem
  | Builtin_pow
  | Builtin_isqrt
  | Builtin_to_int32
  | Builtin_to_int64
  | Builtin_to_int128
  | Builtin_to_int256
  | Builtin_to_uint32
  | Builtin_to_uint64
  | Builtin_to_uint128
  | Builtin_to_nat
  | Builtin_schnorr_get_address
[@@deriving sexp, equal]

type 'rep builtin_annot = builtin * 'rep [@@deriving sexp]

let pp_builtin b =
  match b with
  | Builtin_eq -> "eq"
  | Builtin_concat -> "concat"
  | Builtin_substr -> "substr"
  | Builtin_strlen -> "strlen"
  | Builtin_to_string -> "to_string"
  | Builtin_blt -> "blt"
  | Builtin_badd -> "badd"
  | Builtin_bsub -> "bsub"
  | Builtin_to_uint256 -> "to_uint256"
  | Builtin_sha256hash -> "sha256hash"
  | Builtin_keccak256hash -> "keccak256hash"
  | Builtin_ripemd160hash -> "ripemd160hash"
  | Builtin_to_bystr -> "to_bystr"
  | Builtin_bech32_to_bystr20 -> "bech32_to_bystr20"
  | Builtin_bystr20_to_bech32 -> "bystr20_to_bech32"
  | Builtin_schnorr_verify -> "schnorr_verify"
  | Builtin_ecdsa_verify -> "ecdsa_verify"
  | Builtin_schnorr_get_address -> "schnorr_get_address"
  | Builtin_alt_bn128_G1_add -> "alt_bn128_G1_add"
  | Builtin_alt_bn128_G1_mul -> "alt_bn128_G1_mul"
  | Builtin_alt_bn128_pairing_product -> "alt_bn128_pairing_product"
  | Builtin_contains -> "contains"
  | Builtin_put -> "put"
  | Builtin_get -> "get"
  | Builtin_remove -> "remove"
  | Builtin_to_list -> "to_list"
  | Builtin_size -> "size"
  | Builtin_lt -> "lt"
  | Builtin_add -> "add"
  | Builtin_sub -> "sub"
  | Builtin_mul -> "mul"
  | Builtin_div -> "div"
  | Builtin_rem -> "rem"
  | Builtin_pow -> "pow"
  | Builtin_isqrt -> "isqrt"
  | Builtin_to_int32 -> "to_int32"
  | Builtin_to_int64 -> "to_int64"
  | Builtin_to_int128 -> "to_int128"
  | Builtin_to_int256 -> "to_int256"
  | Builtin_to_uint32 -> "to_uint32"
  | Builtin_to_uint64 -> "to_uint64"
  | Builtin_to_uint128 -> "to_uint128"
  | Builtin_to_nat -> "to_nat"

let parse_builtin s loc =
  match s with
  | "eq" -> Builtin_eq
  | "concat" -> Builtin_concat
  | "substr" -> Builtin_substr
  | "strlen" -> Builtin_strlen
  | "to_string" -> Builtin_to_string
  | "blt" -> Builtin_blt
  | "badd" -> Builtin_badd
  | "bsub" -> Builtin_bsub
  | "to_uint256" -> Builtin_to_uint256
  | "sha256hash" -> Builtin_sha256hash
  | "keccak256hash" -> Builtin_keccak256hash
  | "ripemd160hash" -> Builtin_ripemd160hash
  | "to_bystr" -> Builtin_to_bystr
  | "bech32_to_bystr20" -> Builtin_bech32_to_bystr20
  | "bystr20_to_bech32" -> Builtin_bystr20_to_bech32
  | "schnorr_verify" -> Builtin_schnorr_verify
  | "ecdsa_verify" -> Builtin_ecdsa_verify
  | "schnorr_get_address" -> Builtin_schnorr_get_address
  | "alt_bn128_G1_add" -> Builtin_alt_bn128_G1_add
  | "alt_bn128_G1_mul" -> Builtin_alt_bn128_G1_mul
  | "alt_bn128_pairing_product" -> Builtin_alt_bn128_pairing_product
  | "contains" -> Builtin_contains
  | "put" -> Builtin_put
  | "get" -> Builtin_get
  | "remove" -> Builtin_remove
  | "to_list" -> Builtin_to_list
  | "size" -> Builtin_size
  | "lt" -> Builtin_lt
  | "add" -> Builtin_add
  | "sub" -> Builtin_sub
  | "mul" -> Builtin_mul
  | "div" -> Builtin_div
  | "rem" -> Builtin_rem
  | "pow" -> Builtin_pow
  | "isqrt" -> Builtin_isqrt
  | "to_int32" -> Builtin_to_int32
  | "to_int64" -> Builtin_to_int64
  | "to_int128" -> Builtin_to_int128
  | "to_int256" -> Builtin_to_int256
  | "to_uint32" -> Builtin_to_uint32
  | "to_uint64" -> Builtin_to_uint64
  | "to_uint128" -> Builtin_to_uint128
  | "to_nat" -> Builtin_to_nat
  | _ -> raise (SyntaxError (sprintf "\"%s\" is not a builtin" s, loc))

(*******************************************************)
(*               Types of components                   *)
(*******************************************************)

type component_type = CompTrans | CompProc

let component_type_to_string ctp =
  match ctp with CompTrans -> "transition" | CompProc -> "procedure"

(*******************************************************)
(*                   Annotations                       *)
(*******************************************************)

module type Rep = sig
  type rep

  val dummy_rep : rep

  val get_loc : rep -> loc

  val address_rep : rep

  val uint128_rep : rep

  val uint32_rep : rep

  val bnum_rep : rep

  val string_rep : rep

  val rep_of_sexp : Sexp.t -> rep

  val sexp_of_rep : rep -> Sexp.t

  (* TODO, Issue #179: These functions are only used in TypeCache.ml.
     See if they can be eliminated somehow *)
  val parse_rep : string -> rep

  val get_rep_str : rep -> string
end

(*******************************************************)
(*          Annotated scilla syntax                    *)
(*******************************************************)

module ScillaSyntax (SR : Rep) (ER : Rep) (Literal : ScillaLiteral) = struct
  module SLiteral = Literal
  module SType = SLiteral.LType
  module SIdentifier = SType.TIdentifier
  module SGasCharge = ScillaGasCharge (SIdentifier.Name)

  (*******************************************************)
  (*                   Expressions                       *)
  (*******************************************************)

  type payload = MLit of SLiteral.t | MVar of ER.rep SIdentifier.t
  [@@deriving sexp]

  type pattern =
    | Wildcard
    | Binder of ER.rep SIdentifier.t
    | Constructor of SR.rep SIdentifier.t * pattern list
  [@@deriving sexp]

  type expr_annot = expr * ER.rep

  and expr =
    | Literal of SLiteral.t
    | Var of ER.rep SIdentifier.t
    | Let of ER.rep SIdentifier.t * SType.t option * expr_annot * expr_annot
    | Message of (string * payload) list
    | Fun of ER.rep SIdentifier.t * SType.t * expr_annot
    | App of ER.rep SIdentifier.t * ER.rep SIdentifier.t list
    | Constr of SR.rep SIdentifier.t * SType.t list * ER.rep SIdentifier.t list
    | MatchExpr of ER.rep SIdentifier.t * (pattern * expr_annot) list
    | Builtin of ER.rep builtin_annot * ER.rep SIdentifier.t list
    | TFun of ER.rep SIdentifier.t * expr_annot
    | TApp of ER.rep SIdentifier.t * SType.t list
    (* Fixpoint combinator: used to implement recursion principles *)
    | Fixpoint of ER.rep SIdentifier.t * SType.t * expr_annot
    | GasExpr of SGasCharge.gas_charge * expr_annot
  [@@deriving sexp]

  let expr_rep erep = snd erep

  let expr_loc erep =
    let l = ER.get_loc (expr_rep erep) in
    Option.some_if (l.cnum <> -1) l

  (* SExp printing for Expr for structural printing. *)
  let spp_expr e = sexp_of_expr e |> Sexplib.Sexp.to_string

  let pp_expr e = spp_expr e

  (*******************************************************)
  (*                   Statements                        *)
  (*******************************************************)

  type stmt_annot = stmt * SR.rep

  and stmt =
    | Load of ER.rep SIdentifier.t * ER.rep SIdentifier.t
    | Store of ER.rep SIdentifier.t * ER.rep SIdentifier.t
    | Bind of ER.rep SIdentifier.t * expr_annot
    (* m[k1][k2][..] := v OR delete m[k1][k2][...] *)
    | MapUpdate of
        ER.rep SIdentifier.t
        * ER.rep SIdentifier.t list
        * ER.rep SIdentifier.t option
    (* v <- m[k1][k2][...] OR b <- exists m[k1][k2][...] *)
    (* If the bool is set, then we interpret this as value retrieve,
       otherwise as an "exists" query. *)
    | MapGet of
        ER.rep SIdentifier.t
        * ER.rep SIdentifier.t
        * ER.rep SIdentifier.t list
        * bool
    | MatchStmt of ER.rep SIdentifier.t * (pattern * stmt_annot list) list
    | ReadFromBC of ER.rep SIdentifier.t * string
    | AcceptPayment
    (* forall l p *)
    | Iterate of ER.rep SIdentifier.t * SR.rep SIdentifier.t
    | SendMsgs of ER.rep SIdentifier.t
    | CreateEvnt of ER.rep SIdentifier.t
    | CallProc of SR.rep SIdentifier.t * ER.rep SIdentifier.t list
    | Throw of ER.rep SIdentifier.t option
    | GasStmt of SGasCharge.gas_charge
  [@@deriving sexp]

  let stmt_rep srep = snd srep

  let stmt_loc s = SR.get_loc (stmt_rep s)

  let spp_stmt s = sexp_of_stmt s |> Sexplib.Sexp.to_string

  let pp_stmt s = spp_stmt s

  (*******************************************************)
  (*                    Contracts                        *)
  (*******************************************************)

  type component = {
    comp_type : component_type;
    comp_name : SR.rep SIdentifier.t;
    comp_params : (ER.rep SIdentifier.t * SType.t) list;
    comp_body : stmt_annot list;
  }

  type ctr_def = { cname : ER.rep SIdentifier.t; c_arg_types : SType.t list }

  type lib_entry =
    | LibVar of ER.rep SIdentifier.t * SType.t option * expr_annot
    | LibTyp of ER.rep SIdentifier.t * ctr_def list

  type library = { lname : SR.rep SIdentifier.t; lentries : lib_entry list }

  type contract = {
    cname : SR.rep SIdentifier.t;
    cparams : (ER.rep SIdentifier.t * SType.t) list;
    cconstraint : expr_annot;
    cfields : (ER.rep SIdentifier.t * SType.t * expr_annot) list;
    ccomps : component list;
  }

  (* Contract module: libary + contract definiton *)
  type cmodule = {
    smver : int;
    (* Scilla major version of the contract. *)
    libs : library option;
    (* lib functions defined in the module *)
    (* List of imports / external libs with an optional namespace. *)
    elibs : (SR.rep SIdentifier.t * SR.rep SIdentifier.t option) list;
    contr : contract;
  }

  (* Library module *)
  type lmodule = {
    smver : int;
    (* Scilla major version of the library. *)
    (* List of imports / external libs with an optional namespace. *)
    elibs : (SR.rep SIdentifier.t * SR.rep SIdentifier.t option) list;
    libs : library; (* lib functions defined in the module *)
  }

  (* A tree of libraries linked to their dependents *)
  type libtree = {
    libn : library;
    (* The library this node represents *)
    deps : libtree list; (* List of dependent libraries *)
  }

  let pp_cparams ps =
    let cs =
      List.map ps ~f:(fun (i, t) ->
          SIdentifier.as_string i ^ " : "
          ^ (SType.sexp_of_t t |> Sexplib.Sexp.to_string))
    in
    "[" ^ String.concat ~sep:", " cs ^ "]"

  (* Substitute type for a type variable *)
  let rec subst_type_in_expr tvar tp erep =
    let open SLiteral in
    let open SType in
    let e, rep = erep in
    match e with
    | Literal l -> (Literal (subst_type_in_literal tvar tp l), rep)
    | Var _ as v -> (v, rep)
    | Fun (f, t, body) ->
        let t_subst = subst_type_in_type' tvar tp t in
        let body_subst = subst_type_in_expr tvar tp body in
        (Fun (f, t_subst, body_subst), rep)
    | TFun (tv, body) as tf ->
        if SIdentifier.equal tv tvar then (tf, rep)
        else
          let body_subst = subst_type_in_expr tvar tp body in
          (TFun (tv, body_subst), rep)
    | Constr (n, ts, es) ->
        let ts' = List.map ts ~f:(fun t -> subst_type_in_type' tvar tp t) in
        (Constr (n, ts', es), rep)
    | App _ as app -> (app, rep)
    | Builtin _ as bi -> (bi, rep)
    | Let (i, tann, lhs, rhs) ->
        let tann' =
          Option.map tann ~f:(fun t -> subst_type_in_type' tvar tp t)
        in
        let lhs' = subst_type_in_expr tvar tp lhs in
        let rhs' = subst_type_in_expr tvar tp rhs in
        (Let (i, tann', lhs', rhs'), rep)
    | Message _ as m -> (m, rep)
    | MatchExpr (e, cs) ->
        let cs' =
          List.map cs ~f:(fun (p, b) -> (p, subst_type_in_expr tvar tp b))
        in
        (MatchExpr (e, cs'), rep)
    | TApp (tf, tl) ->
        let tl' = List.map tl ~f:(fun t -> subst_type_in_type' tvar tp t) in
        (TApp (tf, tl'), rep)
    | Fixpoint (f, t, body) ->
        let t' = subst_type_in_type' tvar tp t in
        let body' = subst_type_in_expr tvar tp body in
        (Fixpoint (f, t', body'), rep)
    | GasExpr (g, e) -> (GasExpr (g, subst_type_in_expr tvar tp e), rep)

  (* get variables that get bound in pattern. *)
  let get_pattern_bounds p =
    let rec accfunc p acc =
      match p with
      | Wildcard -> acc
      | Binder i -> i :: acc
      | Constructor (_, plist) ->
          List.fold plist ~init:acc ~f:(fun acc p' -> accfunc p' acc)
    in
    accfunc p []

  (* Returns a list of free variables in expr. *)
  let free_vars_in_expr erep =
    (* get elements in "l" that are not in bound_vars. *)
    let get_free l bound_vars =
      List.filter l ~f:(fun i -> not (SIdentifier.is_mem_id i bound_vars))
    in

    (* The main function that does the job. *)
    let rec recurser erep bound_vars acc =
      let e, _ = erep in
      match e with
      | Literal _ -> acc
      | Var v | TApp (v, _) ->
          if SIdentifier.is_mem_id v bound_vars then acc else v :: acc
      | Fun (f, _, body) | Fixpoint (f, _, body) ->
          recurser body (f :: bound_vars) acc
      | TFun (_, body) -> recurser body bound_vars acc
      | Constr (_, _, es) -> get_free es bound_vars @ acc
      | App (f, args) -> get_free (f :: args) bound_vars @ acc
      | Builtin (_f, args) -> get_free args bound_vars @ acc
      | Let (i, _, lhs, rhs) ->
          let acc_lhs = recurser lhs bound_vars acc in
          recurser rhs (i :: bound_vars) acc_lhs
      | Message margs ->
          List.fold margs ~init:acc ~f:(fun acc (_, x) ->
              match x with
              | MLit _ -> acc
              | MVar v ->
                  if SIdentifier.is_mem_id v bound_vars then acc else v :: acc)
      | MatchExpr (v, cs) ->
          let fv =
            if SIdentifier.is_mem_id v bound_vars then acc else v :: acc
          in
          List.fold cs ~init:fv ~f:(fun acc (p, e) ->
              (* bind variables in pattern and recurse for expression. *)
              let bound_vars' = get_pattern_bounds p @ bound_vars in
              recurser e bound_vars' acc)
      | GasExpr (_, sube) -> recurser sube bound_vars acc
    in
    let fvs = recurser erep [] [] in
    SIdentifier.dedup_id_list fvs

  (* Is expr dependent on any ident in blist.
   * This is the same as checking if a free var
   * in expr is present in blist. *)
  let free_vars_dep_check erep blist =
    (* Utility: is any m in ml, in l. *)
    let any_is_mem ml l =
      List.exists ml ~f:(fun i -> SIdentifier.is_mem_id i l)
    in
    (* Get list of free variables in expression *)
    let fvs = free_vars_in_expr erep in
    (* and check if any of them are in blist. *)
    any_is_mem fvs blist

  (****************************************************************)
  (*                  Better error reporting                      *)
  (****************************************************************)
  let get_failure_msg erep phase opt =
    let open SIdentifier in
    let e, rep = erep in
    let sloc = ER.get_loc rep in
    ( ( match e with
      | Literal _ -> sprintf "Type error in literal. %s\n" phase
      | Var i -> sprintf "Type error in variable `%s`:\n" (as_error_string i)
      | Let (i, _, _, _) ->
          sprintf "Type error in the initialiser of `%s`:\n" (as_error_string i)
      | Message _ -> sprintf "Type error in message.\n"
      | Fun _ -> sprintf "Type error in function:\n"
      | App (f, _) ->
          sprintf "Type error in application of `%s`:\n" (as_error_string f)
      | Constr (s, _, _) ->
          sprintf "Type error in constructor `%s`:\n" (as_error_string s)
      | MatchExpr (x, _) ->
          sprintf
            "Type error in pattern matching on `%s`%s (or one of its branches):\n"
            (as_error_string x) opt
      | Builtin ((i, _), _) ->
          sprintf "Type error in built-in application of `%s`:\n" (pp_builtin i)
      | TApp (tf, _) ->
          sprintf "Type error in type application of `%s`:\n"
            (as_error_string tf)
      | TFun (tf, _) ->
          sprintf "Type error in type function `%s`:\n" (as_error_string tf)
      | GasExpr _ -> "Type error in charging gas :-O, this can't occur.\n"
      | Fixpoint (f, _, _) ->
          sprintf "Type error in fixpoint application with an argument `%s`:\n"
            (as_error_string f) ),
      sloc )

  let get_failure_msg_stmt srep phase opt =
    let open SIdentifier in
    let s, rep = srep in
    let sloc = SR.get_loc rep in
    ( ( match s with
      | Load (x, f) ->
          sprintf "Type error in reading value of `%s` into `%s`:\n %s"
            (as_error_string f) (as_error_string x) phase
      | Store (f, r) ->
          sprintf "Type error in storing value of `%s` into the field `%s`:\n"
            (as_error_string r) (as_error_string f)
      | Bind (x, _) ->
          sprintf "Type error in the binding to into `%s`:\n"
            (as_error_string x)
      | MapGet (_, m, keys, _) ->
          sprintf "Type error in getting map value %s" (as_error_string m)
          ^ List.fold keys ~init:"" ~f:(fun acc k ->
                acc ^ "[" ^ as_error_string k ^ "]")
          ^ "\n"
      | MapUpdate (m, keys, _) ->
          sprintf "Type error in updating map %s" (as_error_string m)
          ^ List.fold keys ~init:"" ~f:(fun acc k ->
                acc ^ "[" ^ as_error_string k ^ "]")
          ^ "\n"
      | MatchStmt (x, _) ->
          sprintf
            "Type error in pattern matching on `%s`%s (or one of its branches):\n"
            (as_error_string x) opt
      | ReadFromBC (x, _) ->
          sprintf "Error in reading from blockchain state into `%s`:\n"
            (as_error_string x)
      | AcceptPayment -> sprintf "Error in accepting payment\n"
      | Iterate (l, p) ->
          sprintf "Error iterating `%s` over elements in list `%s`:\n"
            (as_error_string p) (as_error_string l)
      | SendMsgs i ->
          sprintf "Error in sending messages `%s`:\n" (as_error_string i)
      | CreateEvnt i ->
          sprintf "Error in create event `%s`:\n" (as_error_string i)
      | CallProc (p, _) ->
          sprintf "Error in call of procedure '%s':\n" (as_error_string p)
      | GasStmt _ -> "Error in type checking gas charge. This shouldn't happen."
      | Throw i ->
          let is =
            match i with
            | Some id -> "of '" ^ as_error_string id ^ "'"
            | None -> ""
          in
          sprintf "Error in throw %s:\n" is ),
      sloc )

  let wrap_with_info (msg, sloc) res =
    match res with
    | Ok _ -> res
    | Error e -> Error ({ emsg = msg; startl = sloc; endl = dummy_loc } :: e)

  let wrap_err e phase ?(opt = "") res =
    match res with
    | Ok _ -> res
    (* Handle a special case where we're dealing with the most precise error. *)
    | Error [ e' ] ->
        let m, l = get_failure_msg e phase opt in
        if [%equal: loc] e'.startl dummy_loc then
          Error (mk_error1 (m ^ e'.emsg) l)
        else Error (mk_error2 (m ^ e'.emsg) e'.startl e'.endl)
    | _ -> wrap_with_info (get_failure_msg e phase opt) res

  let wrap_serr s phase ?(opt = "") res =
    match res with
    | Ok _ -> res
    (* Handle a special case where we're dealing with the most precise error. *)
    | Error [ e' ] ->
        let m, l = get_failure_msg_stmt s phase opt in
        if [%equal: loc] e'.startl dummy_loc then
          Error (mk_error1 (m ^ e'.emsg) l)
        else Error (mk_error2 (m ^ e'.emsg) e'.startl e'.endl)
    | _ -> wrap_with_info (get_failure_msg_stmt s phase opt) res
end
