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
open Syntax
open MonadUtil

(****************************************************************)
(*                  Type substitutions                          *)
(****************************************************************)

let rec subst_type_in_type tvar tp tm = match tm with
  | PrimType _ as p -> p
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
      if get_id tvar = n then tp else tv
  | ADT (s, ts) ->
      let ts' = List.map ts ~f:(fun t -> subst_type_in_type tvar tp t) in
      ADT (s, ts')
  | PolyFun (arg, t) as pf -> 
      if get_id tvar = arg then pf
      else PolyFun (arg, subst_type_in_type tvar tp t)

let rec subst_type_in_literal tvar tp l = match l with
  | Map ((kt, vt), ls) -> 
      let kts = subst_type_in_type tvar tp kt in
      let vts = subst_type_in_type tvar tp vt in
      let ls' = List.map ls (fun (k, v) ->
        let k' = subst_type_in_literal tvar tp k in
        let v' = subst_type_in_literal tvar tp v in 
        (k', v')) in
      Map ((kts, vts), ls')
  | ADTValue (n, ts, ls) ->
      let ts' = List.map ts ~f:(fun t -> subst_type_in_type tvar tp t) in
      let ls' = List.map ls ~f:(fun l -> subst_type_in_literal tvar tp l) in
      ADTValue (n, ts', ls')
  | _ -> l


(* Substitute type for a type variable *)
let rec subst_type_in_expr tvar tp e = match e with
  | Literal l -> Literal (subst_type_in_literal tvar tp l)
  | Var _ as v -> v
  | Fun (f, t, body) ->
      let t_subst = subst_type_in_type tvar tp t in 
      let body_subst = subst_type_in_expr tvar tp body in
      Fun (f, t_subst, body_subst)
  | TFun (tv, body) as tf ->
      if get_id tv = get_id tvar
      then tf
      else 
        let body_subst = subst_type_in_expr tvar tp body in
        TFun (tv, body_subst)
  | Constr (n, ts, es) ->
      let ts' = List.map ts ~f:(fun t -> subst_type_in_type tvar tp t) in
      Constr (n, ts', es)
  | App _ as app -> app
  | Builtin _ as bi -> bi
  | Let (i, tann, lhs, rhs) ->
      let tann' = Option.map tann ~f:(fun t -> subst_type_in_type tvar tp t) in
      let lhs' = subst_type_in_expr tvar tp lhs in
      let rhs' = subst_type_in_expr tvar tp rhs in
      Let (i, tann', lhs', rhs')
  | Message _ as m -> m
  | MatchExpr (e, cs) ->
      let cs' = List.map cs ~f:(fun (p, b) -> (p, subst_type_in_expr tvar tp b)) in
      MatchExpr(e, cs')
  | TApp (tf, tl) -> 
      let tl' = List.map tl (fun t -> subst_type_in_type tvar tp t) in
      TApp (tf, tl')
  | Fixpoint (f, t, body) ->
      let t' = subst_type_in_type tvar tp t in
      let body' = subst_type_in_expr tvar tp body in
      Fixpoint (f, t', body')

(****************************************************************)
(*                Inferred types and qualifiers                 *)
(****************************************************************)

type 'rep inferred_type = {
  tp   : typ;
  qual : 'rep
} [@@deriving sexp]

module type QualifiedTypes = sig
  type t
  val mk_qualified_type : typ -> t inferred_type      
end


(****************************************************************)
(*                   Typing environments                        *)
(****************************************************************)
module type MakeTEnvFunctor = functor (Q: QualifiedTypes) -> sig
  (* Resolving results *)
  type resolve_result
  val rr_loc : resolve_result -> loc
  val rr_typ : resolve_result -> Q.t inferred_type
  val rr_pp  : resolve_result -> string
  val mk_qual_tp : typ -> Q.t inferred_type
  
  module TEnv : sig
    type t
    (* Make new type environment *)
    val mk : t
    (* Add to type environment *)
    val addT : t -> loc ident -> typ -> t
    (* Resolve the identifier *)
    val resolveT : 
      ?lopt:(loc option) -> t -> string -> (resolve_result, string) result
    (* Copy the environment *)
    val copy : t -> t
    (* Convert to list *)
    val to_list : t -> (string * resolve_result) list
    (* Print the type environment *)
    val pp : ?f:(string * resolve_result -> bool) -> t -> string        
    (* TODO: Add support for tvars *)
  end
end


(* Typing environment, parameterised by a qualifier *)
module MakeTEnv: MakeTEnvFunctor = functor (Q: QualifiedTypes) -> struct
  type resolve_result = {qt : Q.t inferred_type; loc : loc }
  let rr_loc rr = rr.loc
  let rr_typ rr = rr.qt
  (*  TODO: Also print location *)
  let rr_pp  rr = (rr_typ rr).tp |> pp_typ
  let mk_qual_tp tp =  Q.mk_qualified_type tp
                        
  module TEnv = struct
    type t = {
      tenv  : (string, resolve_result) Hashtbl.t;
      tvars : (string, loc) Hashtbl.t
    }
      
    let mk =
      let t1 = Hashtbl.create 50 in
      let t2 = Hashtbl.create 10 in
      {tenv = t1; tvars = t2}
      
    let addT env id tp =
      let _ = Hashtbl.add env.tenv (get_id id)
          {qt = Q.mk_qualified_type tp; loc = get_loc id} in
      env 

    let to_list env =
      Hashtbl.fold (fun key data z -> (key, data) :: z) env.tenv []
        
    let pp ?f:(f = fun _ -> true) env  =
      let lst = List.filter (to_list env) ~f:f in
      let ps = List.map lst
          ~f:(fun (k, v) -> " [" ^ k ^ " -> " ^ (rr_pp v) ^ "]") in
      let cs = String.concat ~sep:",\n " ps in
      "{" ^ cs ^ " }"

    let resolveT ?lopt:(lopt = None) env id =
      match Hashtbl.find_opt env.tenv id with
      | Some r -> pure r
      | None ->
          let loc_str = (match lopt with
              | Some l -> sprintf " at a location [%s]" (get_loc_str l)
              | None -> "") in
          fail @@ sprintf
            "Couldn't resolve the identifier %s%s in the type environment\n%s"
            id loc_str (pp env)

    let copy e = {
      tenv = Hashtbl.copy e.tenv;
      tvars = Hashtbl.copy e.tvars
    }

  end
end


(****************************************************************)
(*               Specific instantiations                        *)
(****************************************************************)

module PlainTypes : QualifiedTypes = struct
  type t = unit
  let mk_qualified_type t = {tp = t; qual = ()}
end
