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
open PatternMatching
open BuiltIns

(***************************************************)
(*                    Utilities                    *)      
(***************************************************)    

let expr_str e =
  sexp_of_expr sexp_of_loc e
  |> Sexplib.Sexp.to_string

let stmt_str s =
  sexp_of_stmt sexp_of_loc s
  |> Sexplib.Sexp.to_string


(* Serializable literals *)
let is_serializable_literal l = match l with
  | Msg _ | ADTValue _ | Map _ -> false
  | _ -> true

(* Sanitize before storing into a message *)
let sanitize_literal l =
  if is_serializable_literal l
  then pure l
  else fail @@ sprintf "Cannot serialize literal %s"
               (sexp_of_literal l |> Sexplib.Sexp.to_string)

let vals_to_literals vals =
  mapM vals ~f:(fun arg -> match arg with
      | Env.ValLit l -> pure l
      | Env.ValClosure _ ->
          fail @@
          sprintf "Closure arguments in ADT are not supported: %s."
            (Env.pp_value arg))

(***************************************************)

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
             (* Closures are not storable *)
             | ValClosure _ as v ->
                 fail @@ sprintf
                   "Cannot store a closure\n%s\nas %s\nin a message\n%s."
                   (Env.pp_value v)
                   (get_id i)
                   (sexp_of_expr sexp_of_loc  m |>
                    Sexplib.Sexp.to_string))
      in
      let%bind payload_resolved =
        (* Make sure we resolve all the payload *)
        mapM bs ~f:(fun (s, pld) -> liftPair2 s @@ resolve pld) in
      pure (Env.ValLit (Msg payload_resolved), env)

  | Fun (f, t, body) ->
      let clo = Env.ValClosure (f, t, body, env) in
      pure (clo, env)

  | App (f, actuals) ->
      (* Resolve the actuals *)
      let%bind args =
        mapM actuals ~f:(fun arg -> Env.lookup env arg) in
      let%bind ff = Env.lookup env f in
      (* Apply iteratively, also evaluating curried lambdas *)      
      let%bind fully_applied =
        List.fold_left args ~init:(pure ff)
          ~f:(fun res arg ->
              let%bind v = res in
              (* printf "Value to be applied: %s\n" (Env.pp_value v); *)
              (* printf "Argument: %s\n\n" (Env.pp_value arg); *)
              try_apply_as_closure v arg) in
      pure(fully_applied, env)          

  | Constr (cname, ts, actuals) ->
      (* Resolve the actuals *)
      let%bind args =
        mapM actuals ~f:(fun arg -> Env.lookup env arg) in
      (* Make sure we only pass "pure" literals, not closures *)
      let%bind arg_literals = vals_to_literals args in
      let lit = ADTValue (cname, ts, arg_literals) in
      pure (Env.ValLit lit, env)

  | MatchExpr (x, clauses) ->
      let%bind v = Env.lookup env x in
      (* Get the branch and the bindings *)
      let%bind ((_, e_branch), bnds) =
        tryM clauses
          ~msg:(sprintf "Value %s\ndoes not match any clause of\n%s."
                  (Env.pp_value v) (expr_str e))
          ~f:(fun (p, e') -> match_with_pattern v p) in
      (* Update the environment for the branch *)
      let env' = List.fold_left bnds ~init:env
          ~f:(fun z (i, w) -> Env.bind z (get_id i) w) in
      exp_eval e_branch env'      

  | Builtin (i, actuals) ->
      let opname = get_id i in
      let%bind args = mapM actuals ~f:(fun arg -> Env.lookup env arg) in
      let%bind arg_literals = vals_to_literals args in
      let tags = List.map arg_literals ~f:literal_tag in
      let%bind op = BuiltInDictionary.find_builtin_op opname tags in
      let%bind res = op arg_literals in 
      pure (Env.ValLit res, env)

  (* TODO: Implement type term operations *)
  | _ -> 
      match expr_loc e with
      | Some l1 -> fail @@
          sprintf "Expression in line %s: %s  is not supported yet."
            (Int.to_string l1.lnum) (expr_str e)
      | None -> fail @@
          sprintf  "Expression in line %s is not supported yet."
            (expr_str e)

(* Applying a function *)
and try_apply_as_closure v arg =
  match v with
  | Env.ValLit _ ->
      fail @@
      sprintf "Not a functional value: %s."
        (Env.pp_value v)
  | Env.ValClosure (formal, _, body, env) ->
      let env1 = Env.bind env (get_id formal) arg in
      let%bind (v, _) = exp_eval body env1 in
      pure v


(*******************************************************)
(* A monadic big-step evaluator for Scilla statemnts   *)
(*******************************************************)
let rec stmt_eval state stmts =
  match stmts with
  | [] -> pure state
  | s :: sts -> (match s with
      | Load (x, r) ->
          let%bind l = State.load state r in
          let state' = State.bind state (get_id x) (Env.ValLit l) in
          stmt_eval state' sts
      | Store (x, r) ->
          let%bind l = State.load state r in
          let%bind state' = State.store state (get_id x) l in
          stmt_eval state' sts
      | Bind (x, e) ->
          let%bind (lval, _) = exp_eval e state.env in
          let state' = State.bind state (get_id x) lval in
          stmt_eval state' sts
      | MatchStmt (x, clauses) ->
          let%bind v = Env.lookup state.env x in 
          let%bind ((_, branch_stmts), bnds) =
            tryM clauses
              ~msg:(sprintf "Value %s\ndoes not match any clause of\n%s."
                      (Env.pp_value v) (stmt_str s))
              ~f:(fun (p, e') -> match_with_pattern v p) in 
          (* Update the environment for the branch *)
          let state' = List.fold_left bnds ~init:state
              ~f:(fun z (i, w) -> State.bind z (get_id i) w) in
          let%bind state'' = stmt_eval state' branch_stmts in
          (* Restore initial immutable bindings *)
          let cont_state = {state'' with env = state.env} in
          stmt_eval cont_state sts

      (* TODO: implement the rest of the commands *)  
      | _ -> fail "FIXME"
    )
