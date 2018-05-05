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
open MonadUtil

let expr_str e =
  sexp_of_expr sexp_of_loc e
  |> Sexplib.Sexp.to_string

(* Serializable literals *)
let is_serializable_literal l = match l with
  | Msg _ | ADTValue _ | Map _ -> false
  | _ -> true

let sanitize_literal l =
  if is_serializable_literal l
  then pure l
  else fail @@ sprintf "Cannot serialize literal %s"
               (sexp_of_literal l |> Sexplib.Sexp.to_string)


(* A monadic big-step evaluator for Scilla expressions *)
let rec exp_eval e env = match e with

  | Literal l ->
      pure (Env.ValLit l, env)

  | Var i ->
      let%bind v = Env.lookup env i in
      pure @@ (v, env)

  | Let (i, t, lhs, rhs) ->
      let%bind (lval, _) = exp_eval lhs env in
      let env' = Env.bind env (get_id i) lval in
      exp_eval rhs env'

  | Message bs as m ->
      (* Resolve all message payload *)
      let resolve pld = match pld with
        | MTag s -> pure @@ (StringLit s)
        | MLit l  -> sanitize_literal l
        | MVar i ->
            let%bind v = Env.lookup env i in
            (match v with
             | ValLit l -> sanitize_literal l
             | ValClosure _ as v ->
                 fail @@ sprintf
                 "Cannot store a closure\n%s\nin a message\n%s\nwith a binding %s."
                   (Env.pp_value v)
                   (sexp_of_expr sexp_of_loc  m |>
                    Sexplib.Sexp.to_string)
                   (get_id i))
      in
      let%bind payload_resolved =
        mapM bs
          ~f:(fun (s, pld) -> mapPair2 s @@ resolve pld) in
      pure (Env.ValLit (Msg payload_resolved), env)

  | Fun (f, t, body) ->
      let clo = Env.ValClosure (f, t, e, env) in
      pure (clo, env)

  (* TODO: Constructor applications *)

  (* TODO: Function applications *)

  (* TODO: Pattern matching *)

  (* TODO: Map operations *)
        
  (* TODO: Built-ins and hashing *)

  | _ -> 
    let l = expr_loc e in
        match l with
        | Some l1 -> fail @@ "Expression in line " ^ 
            Int.to_string l1.lnum ^ " " ^ (expr_str e) ^ " is not supported yet"
        | None -> fail @@ "Expression " ^ (expr_str e) ^ " is not supported yet"
