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

(* Printing result *)
let pp_result r = match r with
  | Error s -> s
  | Ok (e, env) -> sprintf "%s,\n%s" (Env.pp_value e) (Env.pp env)

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

(*******************************************************)
(* A monadic big-step evaluator for Scilla expressions *)
(*******************************************************)
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
             (* Closures are not sendable by messages *)
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
let rec stmt_eval conf stmts =
  match stmts with
  | [] -> pure conf
  | s :: sts -> (match s with
      | Load (x, r) ->
          let%bind l = Configuration.load conf r in
          let conf' = Configuration.bind conf (get_id x) (Env.ValLit l) in
          stmt_eval conf' sts
      | Store (x, r) ->
          let%bind v = Configuration.lookup conf r in
          let%bind conf' = Configuration.store conf (get_id x) v in
          stmt_eval conf' sts
      | Bind (x, e) ->
          let%bind (lval, _) = exp_eval e conf.env in
          let conf' = Configuration.bind conf (get_id x) lval in
          stmt_eval conf' sts
      | MatchStmt (x, clauses) ->
          let%bind v = Env.lookup conf.env x in 
          let%bind ((_, branch_stmts), bnds) =
            tryM clauses
              ~msg:(sprintf "Value %s\ndoes not match any clause of\n%s."
                      (Env.pp_value v) (stmt_str s))
              ~f:(fun (p, e') -> match_with_pattern v p) in 
          (* Update the environment for the branch *)
          let conf' = List.fold_left bnds ~init:conf
              ~f:(fun z (i, w) -> Configuration.bind z (get_id i) w) in
          let%bind conf'' = stmt_eval conf' branch_stmts in
          (* Restore initial immutable bindings *)
          let cont_conf = {conf'' with env = conf.env} in
          stmt_eval cont_conf sts
      | AcceptPayment ->
          let%bind conf' = Configuration.accept_incoming conf in
          stmt_eval conf' sts
      (* Caution emitting messages does not change balance immediately! *)      
      | SendMsgs ms ->
          let%bind ms_resolved = Configuration.lookup conf ms in
          let%bind conf' = Configuration.send_messages conf ms_resolved in
          stmt_eval conf' sts

      (* TODO: Implement the rest *)
      | _ -> fail @@ sprintf "The statement %s is not supported yet."
            (stmt_str s)
    )

(*******************************************************)
(*              Contract initialization                *)
(*******************************************************)

(* Initializing libraries of a contract *)
let init_libraries libs =
  let init_lib_entry env {lname = id; lexp = e } = (
    let%bind (v, _) = exp_eval e env in
    let env' = Env.bind env (get_id id) v in
    pure env') in
  List.fold_left libs ~init:(pure [])
    ~f:(fun eres lentry ->
        let%bind env = eres in
        init_lib_entry env lentry)

let validate_init_bal b =
  let open Big_int in
  if ge_big_int b zero_big_int
  then pure b
  else fail @@
    sprintf "Negative initial balance: %s." (string_of_big_int b)

(* Initialize fields in a constant environment *)
let init_fields env fs =
  (* Initialize a field in aconstant enfirontment *)
  let init_field fname _t fexp =
    let%bind (v, _) = exp_eval fexp env in
    match v with
    | Env.ValClosure _ ->
        fail @@ sprintf "Closure cannot be stored in a field %s." fname
    | Env.ValLit l -> pure (fname, l)
  in
  mapM fs ~f:(fun (i, t, e) -> init_field (get_id i) t e)

(* TODO: implement type-checking *)
let init_contract libs cparams cfields args init_bal  =
  (* Initialize libraries *)
  let%bind libenv = init_libraries libs in
  let pnames = List.map cparams ~f:(fun (i, t) -> get_id i) in
  let valid_args = List.for_all pnames ~f:(fun p ->
      let selector = fun a -> fst a = p in
      let ex = List.exists args ~f:selector in
      let uniq = (List.count args ~f:selector) = 1 in
      (* printf "Param: %s, exists: %b, uniq: %b\n" p ex uniq; *)
      ex && uniq) in
  if not valid_args
  then fail @@ sprintf
      "Mismatch between the vector of arguments:\n%s\nand expected parameters%s\n"
      (Configuration.pp_literal_map args) (pp_cparams cparams)
  else
    (* Init parameters *)
    let params = List.map args ~f:(fun (n, l) -> (n, Env.ValLit l)) in
    (* Fold params into already initialized libraries, possibly shadowing *)
    let env = List.fold_left ~init:libenv params 
        ~f:(fun e (p, v) -> Env.bind e p v) in
    let%bind fields = init_fields env cfields in
    let%bind balance = validate_init_bal init_bal in
    let open ContractState in
    let cstate = {env; fields; balance} in
    pure cstate

(* Initialize a module with given arguments and initial balance *)
let init_module md args init_bal =
  let {cname ; libs; contr} = md in
  let {cname; cparams; cfields; ctrans} = contr in
  let%bind cstate =
    init_contract libs cparams cfields args init_bal in
  pure (contr, cstate)

(*******************************************************)
(*               Message processing                    *)
(*******************************************************)

let preprocess_message es = ""

let prepare_for_message m contr =
  match m with
  | Msg entries ->
      fail "???"
  | _ -> fail @@ sprintf "Not a message literal: %s." (pp_literal m)

    
(* TODO: Implement the following routines:
* Validate and parse message
* Run a transition for a message and produce updated contract state
*)


