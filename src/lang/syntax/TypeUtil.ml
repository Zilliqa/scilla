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
