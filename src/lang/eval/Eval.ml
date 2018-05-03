(*
 * Copyright (c) 2018 - present Zilliqa, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)


open Syntax
open Core
open Result.Let_syntax
open EvalUtil

let expr_str e =
  sexp_of_expr sexp_of_loc e
  |> Sexplib.Sexp.to_string

(* A monadic big-step evaluator for Scilla expressions *)
let rec exp_eval e env = match e with

  | Literal l ->
      pure (Env.ValLit l, env)

  | Var i ->
      let%bind v = Env.lookup env (get_id i) in
      pure @@ (v, env)

  | Let (i, t, lhs, rhs) ->
      let%bind (lval, _) = exp_eval lhs env in
      let env' = Env.bind env (get_id i) lval in
      exp_eval rhs env'

  | Fun (f, t, body) ->
      let clo = Env.ValClosure (f, t, e, env) in
      pure (clo, env)

  (* TODO: implement remaining clauses *)      

  | _ -> 
    let l = expr_loc e in
        match l with
        | Some l1 -> fail @@ "Expression in line " ^ 
            Int.to_string l1.lnum ^ " " ^ (expr_str e) ^ " is not supported yet"
        | None -> fail @@ "Expression " ^ (expr_str e) ^ " is not supported yet"
