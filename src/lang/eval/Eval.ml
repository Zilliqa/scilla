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


open Syntax
open Core
open EvalUtil
open MonadUtil
open EvalMonad
open EvalMonad.Let_syntax
open PatternMatching
open Stdint
open ContractUtil
open PrettyPrinters

open EvalTypeUtilities
open EvalSyntax

module CU = ScillaContractUtil (ParserUtil.ParserRep) (ParserUtil.ParserRep)
    
(***************************************************)
(*                    Utilities                    *)      
(***************************************************)    

let reserved_names =
  List.map ~f:(fun {lname = x; _} -> get_id x) Recursion.recursion_principles

(* Printing result *)
let pp_result r exclude_names = 
  let enames = List.append exclude_names reserved_names in
  match r with
  | Error (s, _) -> s
  | Ok ((e, env), _) ->
      let filter_prelude = fun (k, _) ->
        not (List.mem enames k ~equal:(fun s1 s2 -> s1 = s2))
      in
      sprintf "%s,\n%s" (Env.pp_value e) (Env.pp ~f:filter_prelude env)

(* Serializable literals *)
let is_serializable_literal l = match l with
  | Msg _ | ADTValue _ | Map _ -> false
  | _ -> true

(* Sanitize before storing into a message *)
let sanitize_literal l =
  if is_serializable_literal l
  then pure l
  else fail @@ sprintf "Cannot serialize literal %s" (pp_literal l)

let vals_to_literals vals =
  mapM vals ~f:(fun arg -> match arg with
      | Env.ValLit l -> pure l
      | Env.ValClosure _ | Env.ValFix _ | Env.ValTypeClosure _ ->
          fail @@
          sprintf "Closure arguments in ADT are not supported: %s."
            (Env.pp_value arg))

(*******************************************************)
(* A monadic big-step evaluator for Scilla expressions *)
(*******************************************************)

let rec exp_eval erep env =
  let (e, _) = erep in
  match e with
  | Literal l ->
      pure (Env.ValLit l, env)
  | Var i ->
      let%bind v = Env.lookup env i in
      pure @@ (v, env)
  | Let (i, _, lhs, rhs) ->
      let%bind (lval, _) = exp_eval_wrapper lhs env in
      let env' = Env.bind env (get_id i) lval in
      exp_eval_wrapper rhs env'
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
             | (ValClosure _ | ValFix _ | ValTypeClosure _) as v ->
                 fail @@ sprintf
                   "Cannot store a closure\n%s\nas %s\nin a message\n%s."
                   (Env.pp_value v)
                   (get_id i)
                   (pp_expr m))
      in
      let%bind payload_resolved =
        (* Make sure we resolve all the payload *)
        mapM bs ~f:(fun (s, pld) -> liftPair2 s @@ resolve pld) in
      pure (Env.ValLit (Msg payload_resolved), env)
  | Fun (arg, t, body) ->
      let clo = Env.ValClosure (arg, t, body, env) in
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
              (* printf "Value to be applied: %s\n" (Env.pp_value v);
               * printf "Argument: %s\n\n" (Env.pp_value arg); *)
              try_apply_as_closure v arg) in
      pure (fully_applied, env)          
  | Constr (cname, ts, actuals) ->
      let open Datatypes.DataTypeDictionary in 
      let%bind (_, constr) = fromR @@ lookup_constructor cname in
      let alen = List.length actuals in
      if (constr.arity <> alen)
      then fail @@ sprintf
          "Constructor %s expects %d arguments, but got %d."
          cname constr.arity alen
      else
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
          (* TODO: add location info to error message. *)
          ~msg:(sprintf "Match expression failed. No clause matched.")
          ~f:(fun (p, _) -> fromR @@ match_with_pattern v p) in
      (* Update the environment for the branch *)
      let env' = List.fold_left bnds ~init:env
          ~f:(fun z (i, w) -> Env.bind z (get_id i) w) in
      exp_eval_wrapper e_branch env'      
  | Builtin (i, actuals) ->
      let%bind args = mapM actuals ~f:(fun arg -> Env.lookup env arg) in
      let%bind arg_literals = vals_to_literals args in
      let%bind tps = fromR @@ MonadUtil.mapM arg_literals ~f:literal_type in
      let%bind res = builtin_executor i tps arg_literals in
      pure (Env.ValLit res, env)
  | Fixpoint (f, t, body) ->
      let fix = Env.ValFix (f, t, body, env) in
      pure (fix, env)
  | TFun (tvar, body) ->
      let tclo = Env.ValTypeClosure (tvar, body, env) in
      pure (tclo, env)
  | TApp (tf, arg_types) ->
      let%bind ff = Env.lookup env tf in
      let%bind fully_applied =
        List.fold_left arg_types ~init:(pure ff)
          ~f:(fun res arg_type -> let%bind v = res in
              try_apply_as_type_closure v arg_type) in
      pure (fully_applied, env)          

(* Applying a function *)
and try_apply_as_closure v arg =
  match v with
  | Env.ValLit _ ->
      fail @@ sprintf "Not a functional value: %s." (Env.pp_value v)
  | Env.ValClosure (formal, _, body, env) ->
      (* TODO: add runtime type-checking *)
      let env1 = Env.bind env (get_id formal) arg in
      let%bind (v, _) = exp_eval_wrapper body env1 in
      pure v
  | Env.ValFix (g, _, body, env) ->
      let env1 = Env.bind env (get_id g) v in
      let%bind (v, _) = exp_eval_wrapper body env1 in
      (match v with
       | Env.ValClosure (formal, _, cbody, cenv) ->
           let env2 = Env.bind cenv (get_id formal) arg in
           let%bind (v, _) = exp_eval_wrapper cbody env2 in
           pure v
       | _ ->  fail @@ sprintf "A fixpoint should take a function as a body")
  | Env.ValTypeClosure _ ->
      fail @@ sprintf "Cannot apply a type closure to a value argument: %s." (Env.pp_value arg)

and try_apply_as_type_closure v arg_type =
  match v with
  | Env.ValTypeClosure (tv, body, env) ->
      let body_subst = subst_type_in_expr tv arg_type body in
      let%bind (v, _) = exp_eval_wrapper body_subst env in
      pure v      
  | _ ->
      fail @@ sprintf "Not a type closure: %s." (Env.pp_value v)

(* Adding gas cost to the reduction *)
and exp_eval_wrapper expr env =
  let thunk () = exp_eval expr env in
  let%bind cost = fromR @@ EvalGas.expr_static_cost expr in
  let emsg = sprintf "Ran out of gas.\n" in
  checkwrap_op thunk cost emsg


open EvalSyntax
(*******************************************************)
(* A monadic big-step evaluator for Scilla statemnts   *)
(*******************************************************)
let rec stmt_eval conf stmts =
  match stmts with
  | [] -> pure conf
  | (s, _) :: sts -> (match s with
      | Load (x, r) ->
          let%bind (l, scon) = Configuration.load conf r in
          let conf' = Configuration.bind conf (get_id x) (Env.ValLit l) in
          let%bind _ = stmt_gas_wrap scon in
          stmt_eval conf' sts
      | Store (x, r) ->
          let%bind v = Configuration.lookup conf r in
          let%bind (conf', scon) = Configuration.store conf (get_id x) v in
          let%bind _ = stmt_gas_wrap scon in
          stmt_eval conf' sts
      | Bind (x, e) ->
          let%bind (lval, _) = exp_eval_wrapper e conf.env in
          let conf' = Configuration.bind conf (get_id x) lval in
          let%bind _ = stmt_gas_wrap G_Bind in
          stmt_eval conf' sts
      | ReadFromBC (x, bf) ->
          let%bind l = Configuration.bc_lookup conf bf in
          let conf' = Configuration.bind conf (get_id x) (Env.ValLit l) in
          let%bind _ = stmt_gas_wrap G_ReadFromBC in
          stmt_eval conf' sts                            
      | MatchStmt (x, clauses) ->
          let%bind v = Env.lookup conf.env x in 
          let%bind ((_, branch_stmts), bnds) =
            tryM clauses
              ~msg:(sprintf "Value %s\ndoes not match any clause of\n%s."
                      (Env.pp_value v) (pp_stmt s))
              ~f:(fun (p, _) -> fromR @@ match_with_pattern v p) in 
          (* Update the environment for the branch *)
          let conf' = List.fold_left bnds ~init:conf
              ~f:(fun z (i, w) -> Configuration.bind z (get_id i) w) in
          let%bind conf'' = stmt_eval conf' branch_stmts in
          (* Restore initial immutable bindings *)
          let cont_conf = {conf'' with env = conf.env} in
          let%bind _ = stmt_gas_wrap (G_MatchStmt (List.length clauses)) in
          stmt_eval cont_conf sts
      | AcceptPayment ->
          let%bind conf' = Configuration.accept_incoming conf in
          let%bind _ = stmt_gas_wrap G_AcceptPayment in
          stmt_eval conf' sts
      (* Caution emitting messages does not change balance immediately! *)      
      | SendMsgs ms ->
          let%bind ms_resolved = Configuration.lookup conf ms in
          let%bind (conf', scon) = Configuration.send_messages conf ms_resolved in
          let%bind _ = stmt_gas_wrap scon in
          stmt_eval conf' sts
      | CreateEvnt params ->
          let%bind eparams_resolved = Configuration.lookup conf params in
          let%bind (conf', scon) = Configuration.create_event conf eparams_resolved in
          let%bind _ = stmt_gas_wrap scon in
          stmt_eval conf' sts
      (* TODO: Implement Throw *)
      | Throw _ -> fail @@ sprintf "Throw statements are not supported yet."
    )

(*******************************************************)
(*          BlockchainState initialization             *)
(*******************************************************)

let check_blockchain_entries entries =
  let expected = [
      (TypeUtil.blocknum_name, BNum("0"))
  ] in
  (* every entry must be expected *)
  let c1 = List.for_all entries ~f:(fun (s, _) ->
    List.exists expected ~f:(fun (t, _) -> s = t)) in
  (* everything expected must be entered *)
  let c2 = List.for_all expected ~f:(fun (s, _) ->
    List.exists entries ~f:(fun (t, _) -> s = t)) in
  if c1 && c2 then
    pure entries
  else
    fail @@sprintf "Mismatch in input blockchain variables:\nexpected:\n%s\nprovided:\n%s\n"
      (pp_literal_map expected) (pp_literal_map entries)

(*******************************************************)
(*              Contract initialization                *)
(*******************************************************)

(* Combine external and contract lib functions. *)
let combine_libs clibs elibs =
  (* combine both the libs, with contract libs defined later 
     (to hide same name external libs *)
  let clib = match clibs with | Some clib -> (clib::[]) | None -> [] in
  let j = List.append elibs clib in
  let libs = 
    List.fold_right ~f:(fun l a -> List.append l.lentries a) ~init:[] j in
  libs

(* Initializing libraries of a contract *)
let init_libraries clibs elibs =
  let init_lib_entry env {lname = id; lexp = e } = (
    let%bind (v, _) = exp_eval_wrapper e env in
    let env' = Env.bind env (get_id id) v in
    pure env') in

  let libs = Recursion.recursion_principles @ (combine_libs clibs elibs) in

  DebugMessage.plog ("Loading library functions.");
  List.fold_left libs ~init:(pure Env.empty)
    ~f:(fun eres lentry ->
        let%bind env = eres in
        init_lib_entry env lentry)

(* Initialize fields in a constant environment *)
let init_fields env fs =
  (* Initialize a field in a constant environment *)
  let init_field fname _t fexp =
    let%bind (v, _) = exp_eval_wrapper fexp env in
    match v with
    | Env.ValClosure _ | Env.ValFix _ | Env.ValTypeClosure _ ->
        fail @@ sprintf "Closure cannot be stored in a field %s." fname
    | Env.ValLit l -> pure (fname, l)
  in
  mapM fs ~f:(fun (i, t, e) -> init_field (get_id i) t e)

(* TODO: implement type-checking *)
let init_contract clibs elibs cparams' cfields args init_bal  =
  (* All contracts take a few implicit parameters. *)
  let cparams = CU.append_implict_contract_params cparams' in
  (* Initialize libraries *)
  let%bind libenv = init_libraries clibs elibs in
  let pnames = List.map cparams ~f:(fun (i, _) -> get_id i) in
  let valid_args = List.for_all args ~f:(fun a ->
      (* For each argument there is a parameter *)
      let ex = List.exists pnames ~f:(fun p -> p = fst a) in
      (* Each argument name occurrs once *)
      let uniq = (List.count args ~f:(fun e -> fst e = fst a)) = 1 in
      (* printf "Param: %s, exists: %b, uniq: %b\n" p ex uniq; *)
      ex && uniq) in
  let valid_params = List.for_all pnames ~f:(fun p ->
      (* For each parameter there is an argument *)
      List.exists args ~f:(fun a -> p = fst a)) in  
  if not (valid_args && valid_params)
  then fail @@ sprintf
      "Mismatch between the vector of arguments:\n%s\nand expected parameters%s\n"
      (pp_literal_map args) (pp_cparams cparams)
  else
    (* Init parameters *)
    let params = List.map args ~f:(fun (n, l) -> (n, Env.ValLit l)) in
    (* Fold params into already initialized libraries, possibly shadowing *)
    let env = List.fold_left ~init:libenv params 
        ~f:(fun e (p, v) -> Env.bind e p v) in
    let%bind fields = init_fields env cfields in
    let balance = init_bal in
    let open ContractState in
    let cstate = {env; fields; balance} in
    pure cstate

(* Combine initialized state with info from current state *)
let create_cur_state_fields initcstate curcstate =
  (* If there's a field in curcstate that isn't in initcstate,
     flag it as invalid input state *)
  let invalid = List.exists curcstate ~f:(fun (s, _) ->
    not (List.exists initcstate ~f:(fun (t, _) -> s = t))) in
  (* Each entry name is unique *)
  let uniq_entries = List.for_all curcstate
      ~f:(fun e -> (List.count curcstate ~f:(fun e' -> fst e = fst e')) = 1) in
  if (not invalid) && uniq_entries then
    (* Get only those fields from initcstate that are not in curcstate *)
    let filtered_init = List.filter initcstate 
        ~f:(fun (s, _) -> not (List.exists curcstate 
            ~f:(fun (s1, _) -> s = s1))) in
        (* Combine filtered list and curcstate *)
        pure (filtered_init @ curcstate)
  else
    fail @@sprintf "Mismatch in input state variables:\nexpected:\n%s\nprovided:\n%s\n"
                   (pp_literal_map initcstate) (pp_literal_map curcstate)


let literal_list_gas llit =
  foldM ~f:(fun acc (_, lit) ->
    let%bind c = fromR @@ EvalGas.literal_cost lit in
    pure (c + acc)) ~init:0 llit

(* Initialize a module with given arguments and initial balance *)
let init_module md initargs curargs init_bal bstate elibs =
  let {libs; contr; _} = md in
  let {cparams; cfields; _} = contr in
  let%bind initcstate =
    init_contract libs elibs cparams cfields initargs init_bal in
  let%bind curfields = create_cur_state_fields initcstate.fields curargs in
  (* Gas accounting for params and state fields. *)
  let%bind _ = literal_list_gas curfields in
  let%bind _ = literal_list_gas initargs in
  (* blockchain input provided is only validated and not used here. *)
  let%bind _ = check_blockchain_entries bstate in
  let cstate = { initcstate with fields = curfields } in
    pure (contr, cstate)

(*******************************************************)
(*               Message processing                    *)
(*******************************************************)

(* Extract necessary bits from the message *)
let preprocess_message es =
  let%bind tag = fromR @@ MessagePayload.get_tag es in
  let%bind amount = fromR @@ MessagePayload.get_amount es in
  let other = MessagePayload.get_other_entries es in
  pure (tag, amount, other)

(* Retrieve transition based on the tag *)
let get_transition ctr tag =
  let ts = ctr.ctrans in
  match List.find ts ~f:(fun t -> (get_id t.tname) = tag) with
  | None -> fail @@ sprintf
        "No contract transition for tag %s found." tag
  | Some t ->
      let params = t.tparams in
      let body = t.tbody in
      pure (params, body)

(* Ensure match b/w transition defined params and passed arguments (entries) *)
let check_message_entries tparams_o entries =
  let tparams = CU.append_implict_trans_params tparams_o in
  (* There as an entry for each parameter *)
  let valid_entries = List.for_all tparams
      ~f:(fun p -> List.exists entries ~f:(fun e -> fst e = (get_id (fst p)))) in
  (* There is a parameter for each entry *)
  let valid_params = List.for_all entries
      ~f:(fun (s, _) -> List.exists tparams ~f:(fun (i, _) -> s = get_id i)) in
  (* Each entry name is unique *)
  let uniq_entries = List.for_all entries
      ~f:(fun e -> (List.count entries ~f:(fun e' -> fst e = fst e')) = 1) in
  if not (valid_entries && uniq_entries && valid_params)
  then fail @@ sprintf
      "Duplicate entries or mismatch b/w message entries:\n%s\nand expected transition parameters%s\n"
      (pp_literal_map entries) (pp_cparams tparams)
  else
    pure entries
      
(* Get the environment, incoming amount and body to execute*)
let prepare_for_message contr m =
  match m with
  | Msg entries ->
      let%bind (tag, incoming_amount, other) = preprocess_message entries in
      let%bind (tparams, tbody) = get_transition contr tag in
      let%bind tenv = check_message_entries tparams other in
      pure (tenv, incoming_amount, tbody)
  | _ -> fail @@ sprintf "Not a message literal: %s." (pp_literal m)

(* Subtract the amounts to be transferred *)
let post_process_msgs cstate outs =
  (* Evey outgoing message should carry an "_amount" tag *)
  let%bind amounts = mapM outs ~f:(fun l -> match l with
      | Msg es -> fromR @@ MessagePayload.get_amount es
      | _ -> fail @@ sprintf "Not a message literal: %s." (pp_literal l)) in
  let open Uint128 in
  let to_be_transferred = List.fold_left amounts ~init:zero
      ~f:(fun z a -> add z a) in
  let open ContractState in
  if (compare cstate.balance to_be_transferred) < 0
  then fail @@ sprintf
      "The balance is too low (%s) to transfer all the funds in the messages (%s)"
      (to_string cstate.balance) (to_string to_be_transferred)
  else
    let balance = sub cstate.balance to_be_transferred in
    pure {cstate with balance}

let append_accepted_state msgs accepted =
  let f m =
    match m with
    | Msg m' ->
      let s = Bool.to_string accepted in
      let i = (MessagePayload.accepted_label, StringLit s) in
        Msg (i :: m')
    | _ -> m (* Not a message. Is this possible? *)
  in
    List.map ~f:f msgs
(* 
Handle message:
* contr : Syntax.contract - code of the contract (containing transitions)
* cstate : ContractState.t - current contract state
* bstate : (string * literal) list - blockchain state
* m : Syntax.literal - incoming message 
*)        
let handle_message contr cstate bstate m =
  let%bind (tenv, incoming_funds, stmts) = prepare_for_message contr m in
  let open ContractState in
  let {env; fields; balance} = cstate in
  (* Add all values to the contract environment *)
  let actual_env = List.fold_left tenv ~init:env
      ~f:(fun e (n, l) -> Env.bind e n (Env.ValLit l)) in
  let open Configuration in

  (* Create configuration *)  
  let conf = {
    env = actual_env;
    fields = fields;
    balance = balance;
    accepted = false;
    blockchain_state = bstate;
    incoming_funds = incoming_funds;
    emitted = [];
    events = [];
  } in

  (* Finally, run the evaluator for statements *)
  let%bind conf' = stmt_eval conf stmts in
  let cstate' = {
    env = cstate.env;
    fields = conf'.fields;
    balance = conf'.balance
  } in
  let new_msgs = conf'.emitted in
  let new_msgs' = append_accepted_state new_msgs conf'.accepted in
  let new_events = conf'.events in
  (* Make sure that we aren't too generous and subract funds *)
  let%bind cstate'' = post_process_msgs cstate' new_msgs' in

  (*Return new contract state, messages and events *)
  pure (cstate'', new_msgs', new_events)
    
