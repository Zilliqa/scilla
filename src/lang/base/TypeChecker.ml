(*
 * Copyright (c) 2018 - present. 
 * Zilliqa, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open Syntax
open Core
open Result.Let_syntax
open MonadUtil
open TypeUtil
open Datatypes
open BuiltIns

(* Instantiated the type environment *)
module SimpleTEnv = MakeTEnv(PlainTypes)
open SimpleTEnv

(* TODO: Check if the type is well-formed: support type variables *)
let rec get_type e tenv = match e with
  | Literal l ->
      (* TODO: Check that literal is well-formed *)
      let%bind lt = literal_type l in
      pure @@ mk_qual_tp lt
  | Var i ->
      let%bind r = TEnv.resolveT tenv (get_id i) ~lopt:(Some (get_loc i)) in
      pure @@ (rr_typ r)
  |  Fun (arg, t, body) ->
      let tenv' = TEnv.addT (TEnv.copy tenv) arg t in
      let%bind bt = get_type body tenv' in
      pure @@ mk_qual_tp (FunType (t, bt.tp))
  | Builtin (i, actuals) ->
      let%bind tresults = mapM actuals
          ~f:(fun arg -> TEnv.resolveT tenv (get_id arg) ~lopt:(Some (get_loc arg))) in
      let targs = List.map tresults ~f:(fun rr -> (rr_typ rr).tp) in
      let%bind (_, ret_typ, _) = BuiltInDictionary.find_builtin_op i targs in
      pure @@ mk_qual_tp ret_typ
  | Let (i, t, lhs, rhs) ->
      (* TODO: Check that matches type *)
      let%bind ityp = get_type lhs tenv in
      let tenv' = TEnv.addT (TEnv.copy tenv) i ityp.tp in
      get_type rhs tenv'
      


  (* 1. Type-check primitive literals *)
  (* 2. ADTs and pattern-matching *)
  (* 3. Recursin principles (hard-coded) *)
  (* 4. Type-check maps *)
  (* 5. Type-check ADTs *)
      

  (* TODO: Implement other expressions *)
  | _ -> fail @@ sprintf
      "Failed to resolve the type of the expresssion:\n%s\n" (expr_str e)
