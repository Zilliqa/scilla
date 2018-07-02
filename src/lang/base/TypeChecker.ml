(*
 * Copyright (c) 2018 - present , Inc.
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

(* Instantiated the type environment *)
module SimpleTEnv = MakeTEnv(PlainTypes)
open SimpleTEnv

(* TODO: Check if the type is well-formed: support type variables *)
let rec get_type e tenv = match e with
  | Var i ->
      let%bind r = TEnv.resolveT tenv (get_id i) ~lopt:(Some (get_loc i)) in
      pure @@ (rr_typ r)
  |  Fun (arg, t, body) ->
      let tenv' = TEnv.addT (TEnv.copy tenv) arg t in
      let%bind bt = get_type body tenv' in
      pure @@ mk_qual_tp (FunType (t, bt.tp))

  (* 1. Type-check primitive literals *)
  (* 2. ADTs and pattern-matching *)
  (* 3. Recursin principles (hard-coded) *)
  (* 3. Built-ins // make this a functor taking built-in signatures *)
  (* 4. Type-check maps *)
  (* 5. Type-check ADTs *)
      

  (* TODO: Implement other expressions *)
  | _ -> fail @@ "Failed to resolve the type"
