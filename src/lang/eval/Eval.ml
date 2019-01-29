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
open ErrorUtils
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
  List.map ~f:(fun entry ->
      match entry with
      | LibVar (lname, _) -> get_id lname
      | LibTyp (tname, _) -> get_id tname)
    Recursion.recursion_principles

(* Printing result *)
let pp_result r exclude_names = 
  let enames = List.append exclude_names reserved_names in
  match r with
  | Error (s, _) -> sprint_scilla_error_list s
  | Ok ((e, env), _) ->
      let filter_prelude = fun (k, _) ->
        not (List.mem enames k ~equal:(fun s1 s2 -> s1 = s2))
      in
      sprintf "%s,\n%s" (Env.pp_value e) (Env.pp ~f:filter_prelude env)

(* Makes sure that the literal has no closures in it *)
(* TODO: Augment with deep checking *)
let rec is_pure_literal l = match l with
  | Clo _ -> false
  | TAbs _ -> false
  | Msg es -> List.for_all es ~f:(fun (_, l') -> is_pure_literal l')
  | ADTValue (_, _, es) -> List.for_all es ~f:(fun e -> is_pure_literal e)
  (* | Map (_, ht) ->
   *     let es = Caml.Hashtbl.to_alist ht in
   *     List.for_all es ~f:(fun (k, v) -> is_pure_literal k && is_pure_literal v) *)
  | _ -> true

(* Serializable literals *)
let rec is_serializable_literal l = match l with
  | Msg _ | Map _ | Clo _ | TAbs _ -> false
  | ADTValue (_, _, llist) ->
    List.for_all llist ~f:(fun l -> is_serializable_literal l)
  | _ -> true

(* Sanitize before storing into a message *)
let sanitize_literal l =
  if is_serializable_literal l
  then pure l
  else fail0 @@ sprintf "Cannot serialize literal %s" (pp_literal l)

(*******************************************************)
(* A monadic big-step evaluator for Scilla expressions *)
(*******************************************************)

(* [Evaluation in CPS]

   The following evaluator is implemented in a monadic style, with the
   monad, at the moment to be CPS, with the specialised return result
   type as described in [Specialising the Return Type of Closures].
 *)

let rec exp_eval erep env  =
  let (e, loc) = erep in
  match e with
  | Literal l ->
      pure (l, env)
  | Var i ->
      let%bind v = Env.lookup env i in
      pure @@ (v, env)
  | Let (i, _, lhs, rhs) ->
      let%bind (lval, _) = exp_eval_wrapper lhs env in
      let env' = Env.bind env (get_id i) lval in
      exp_eval_wrapper rhs env'
  | Message bs ->
      (* Resolve all message payload *)
      let resolve pld = match pld with
        | MTag s -> pure @@ (StringLit s)
        | MLit l -> sanitize_literal l
        | MVar i ->
            let%bind v = Env.lookup env i in
            sanitize_literal v
      in
      let%bind payload_resolved =
        (* Make sure we resolve all the payload *)
        mapM bs ~f:(fun (s, pld) -> liftPair2 s @@ resolve pld) in
      pure (Msg payload_resolved, env)
  | Fun (formal, _, body) ->
      (* Apply to an argument *)
      let runner arg = 
        let env1 = Env.bind env (get_id formal) arg in
        let%bind (v, _) = exp_eval_wrapper body env1 in
        pure v
      in      
      pure (Clo runner, env)
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
              try_apply_as_closure v arg) in
      pure (fully_applied, env)          
  | Constr (cname, ts, actuals) ->
      let open Datatypes.DataTypeDictionary in 
      let%bind (_, constr) = fromR @@ lookup_constructor cname in
      let alen = List.length actuals in
      if (constr.arity <> alen)
      then fail0 @@ sprintf
          "Constructor %s expects %d arguments, but got %d."
          cname constr.arity alen
      else
        (* Resolve the actuals *)
        let%bind args =
          mapM actuals ~f:(fun arg -> Env.lookup env arg) in
        (* Make sure we only pass "pure" literals, not closures *)
        let lit = ADTValue (cname, ts, args) in
        pure (lit, env)
  | MatchExpr (x, clauses) ->
      let%bind v = Env.lookup env x in
      (* Get the branch and the bindings *)
      let%bind ((_, e_branch), bnds) =
        tryM clauses
          ~msg:(fun () -> mk_error1 (sprintf "Match expression failed. No clause matched.") loc)
          ~f:(fun (p, _) -> fromR @@ match_with_pattern v p) in
      (* Update the environment for the branch *)
      let env' = List.fold_left bnds ~init:env
          ~f:(fun z (i, w) -> Env.bind z (get_id i) w) in
      exp_eval_wrapper e_branch env'      
  | Builtin (i, actuals) ->
      let%bind args = mapM actuals ~f:(fun arg -> Env.lookup env arg) in
      let%bind tps = fromR @@ MonadUtil.mapM args ~f:literal_type in
      let%bind res = builtin_executor i tps args in
      pure (res, env)
  | Fixpoint (g, _, body) ->
      let rec fix arg =
        let env1 = Env.bind env (get_id g) clo_fix in
        let%bind (fbody, _) = exp_eval_wrapper body env1 in
        match fbody with
        | Clo f -> f arg
        | _ -> fail0 "Cannot apply fxpoint argument to a value"
      and clo_fix = Clo fix          
      in pure (clo_fix, env)
  | TFun (tv, body) ->
      let typer arg_type =
        let body_subst = subst_type_in_expr tv arg_type body in
        let%bind (v, _) = exp_eval_wrapper body_subst env in
        pure v
      in      
      pure (TAbs typer, env)
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
  | Clo clo -> clo arg
  |  _ ->
      fail0 @@ sprintf "Not a functional value: %s." (Env.pp_value v)

and try_apply_as_type_closure v arg_type =
  match v with
  | TAbs tclo -> tclo arg_type
  | _ ->
      fail0 @@ sprintf "Not a type closure: %s." (Env.pp_value v)

(* Adding gas cost to the reduction *)
and exp_eval_wrapper expr env =
  let (_, eloc) = expr in
  let thunk () = exp_eval expr env in
  let%bind cost = fromR @@ EvalGas.expr_static_cost expr in
  let emsg = sprintf "Ran out of gas.\n" in
  (* Add end location too: https://github.com/Zilliqa/scilla/issues/134 *)
  checkwrap_op thunk (Uint64.of_int cost) (mk_error1 emsg eloc)

(* [Initial Gas-Passing Continuation]

   The following function is used as an initial continuation to
   "bootstrap" the gas-aware computation and then retrieve not just
   the result, but also the remaining gas.

*)
let init_gas_kont r gas' = (match r with 
    | Ok z -> Ok (z, gas')
    | Error msg -> Error (msg, gas'))

(* [Continuation for Expression Evaluation]

   The following function implements an impedance matcher. Even though
   it takes a continuation `k` from the callee, it starts evaluating
   an expression `expr` in a "basic" continaution `init_gas_kont` (cf.
   [Initial Gas-Passing Continuation]) with a _fixed_ result type (cf
   [Specialising the Return Type of Closures]). In short, it fully
   evaluates an expression with the fixed continuation, after which
   the result is passed further to the callee's continuation `k`.

*)
let exp_eval_wrapper_no_cps expr env k gas = 
  let eval_res = exp_eval_wrapper expr env init_gas_kont gas in
  let (res, remaining_gas) = (match eval_res with 
    | Ok (z, g) -> (Ok z, g)
    | Error (m, g) -> (Error m, g)) in
  k res remaining_gas

open EvalSyntax
(*******************************************************)
(* A monadic big-step evaluator for Scilla statemnts   *)
(*******************************************************)
let rec stmt_eval conf stmts =
  match stmts with
  | [] -> pure conf
  | (s, sloc) :: sts -> (match s with
      | Load (x, r) ->
          let%bind (l, scon) = Configuration.load conf r in
          let conf' = Configuration.bind conf (get_id x) l in
          let%bind _ = stmt_gas_wrap scon sloc in
          stmt_eval conf' sts
      | Store (x, r) ->
          let%bind v = Configuration.lookup conf r in
          let%bind (conf', scon) = Configuration.store conf x v in
          let%bind _ = stmt_gas_wrap scon sloc in
          stmt_eval conf' sts
      | Bind (x, e) ->
          let%bind (lval, _) = exp_eval_wrapper_no_cps e conf.env in
          let conf' = Configuration.bind conf (get_id x) lval in
          let%bind _ = stmt_gas_wrap G_Bind sloc in
          stmt_eval conf' sts
      | MapUpdate(m, klist, ropt) ->
          let%bind klist' = mapM ~f:(fun k -> Configuration.lookup conf k) klist in
          let%bind v = (match ropt with
            | Some r ->
                let%bind v = Configuration.lookup conf r in
                pure (Some v)
            | None -> pure None)
          in
          let%bind (conf', scon) = Configuration.map_update conf m klist' v in
          let%bind _ = stmt_gas_wrap scon sloc in
          stmt_eval conf' sts
      | MapGet(x, m, klist, fetchval) ->
          let%bind klist' = mapM ~f:(fun k -> Configuration.lookup conf k) klist in
          let%bind (l, scon) = Configuration.map_get conf m klist' fetchval in
          let conf' = Configuration.bind conf (get_id x) l in
          let%bind _ = stmt_gas_wrap scon sloc in
          stmt_eval conf' sts
      | ReadFromBC (x, bf) ->
          let%bind l = Configuration.bc_lookup conf bf in
          let conf' = Configuration.bind conf (get_id x) l in
          let%bind _ = stmt_gas_wrap G_ReadFromBC sloc in
          stmt_eval conf' sts                            
      | MatchStmt (x, clauses) ->
          let%bind v = Env.lookup conf.env x in 
          let%bind ((_, branch_stmts), bnds) =
            tryM clauses
              ~msg:(fun () -> mk_error0 (sprintf "Value %s\ndoes not match any clause of\n%s."
                      (Env.pp_value v) (pp_stmt s)))
              ~f:(fun (p, _) -> fromR @@ match_with_pattern v p) in 
          (* Update the environment for the branch *)
          let conf' = List.fold_left bnds ~init:conf
              ~f:(fun z (i, w) -> Configuration.bind z (get_id i) w) in
          let%bind conf'' = stmt_eval conf' branch_stmts in
          (* Restore initial immutable bindings *)
          let cont_conf = {conf'' with env = conf.env} in
          let%bind _ = stmt_gas_wrap (G_MatchStmt (List.length clauses)) sloc in
          stmt_eval cont_conf sts
      | AcceptPayment ->
          let%bind conf' = Configuration.accept_incoming conf in
          let%bind _ = stmt_gas_wrap G_AcceptPayment sloc in
          stmt_eval conf' sts
      (* Caution emitting messages does not change balance immediately! *)      
      | SendMsgs ms ->
          let%bind ms_resolved = Configuration.lookup conf ms in
          let%bind (conf', scon) = Configuration.send_messages conf ms_resolved in
          let%bind _ = stmt_gas_wrap scon sloc in
          stmt_eval conf' sts
      | CreateEvnt params ->
          let%bind eparams_resolved = Configuration.lookup conf params in
          let%bind (conf', scon) = Configuration.create_event conf eparams_resolved in
          let%bind _ = stmt_gas_wrap scon sloc in
          stmt_eval conf' sts
      | Throw _ -> fail1 (sprintf "Throw statements are not supported yet.") sloc
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
    fail0 @@sprintf "Mismatch in input blockchain variables:\nexpected:\n%s\nprovided:\n%s\n"
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
  let init_lib_entry env id e = (
    let%bind (v, _) = exp_eval_wrapper_no_cps e env in
    let env' = Env.bind env (get_id id) v in
    pure env') in

  let libs = Recursion.recursion_principles @ (combine_libs clibs elibs) in

  DebugMessage.plog ("Loading library types and functions.");
  List.fold_left libs ~init:(pure Env.empty)
    ~f:(fun eres lentry ->
        match lentry with
        | LibTyp (tname, ctr_defs) ->
            let open Datatypes.DataTypeDictionary in
            let (ctrs, tmaps) = List.fold_right ctr_defs ~init:([], [])
                ~f:(fun ctr_def (tmp_ctrs, tmp_tmaps) ->
                    let { cname ; c_arg_types } = ctr_def in
                    ( { Datatypes.cname = get_id cname ;
                        Datatypes.arity = List.length c_arg_types } :: tmp_ctrs,
                      ( get_id cname, c_arg_types ) :: tmp_tmaps )) in
            let adt = { Datatypes.tname = get_id tname ;
                        Datatypes.tparams = [] ;
                        Datatypes.tconstr = ctrs ;
                        Datatypes.tmap = tmaps } in
            let _ = add_adt adt (get_rep tname) in
            eres
        | LibVar (lname, lexp) ->
            let%bind env = eres in
            init_lib_entry env lname lexp)

(* Initialize fields in a constant environment *)
let init_fields env fs =
  (* Initialize a field in a constant environment *)
  let init_field fname _t fexp =
    let%bind (v, _) = exp_eval_wrapper_no_cps fexp env in
    match v with
    | l when is_pure_literal l -> pure (fname, l)
    | _ -> fail0 @@ sprintf "Closure cannot be stored in a field %s." fname
  in
  mapM fs ~f:(fun (i, t, e) -> init_field (get_id i) t e)

let init_contract clibs elibs cparams' cfields args init_bal  =
  (* All contracts take a few implicit parameters. *)
  let cparams = CU.append_implict_contract_params cparams' in
  (* Initialize libraries *)
  let%bind libenv = init_libraries clibs elibs in
  (* Is there an argument that is not a parameter? *)
  let%bind _ = forallM ~f:(fun a ->
    let%bind atyp = fromR @@ literal_type (snd a) in
    let emsg () = mk_error0 
        (sprintf "Parameter %s : %s is not specified in the contract.\n" (fst a) (pp_typ atyp)) in
    (* For each argument there should be a parameter *)
    let%bind (_, mp) = tryM ~f:(fun (ps, pt) -> 
        let%bind at = fromR @@ literal_type (snd a) in
        if ((get_id ps) = (fst a) && pt = at)
          then pure true else fail0 ""
    ) cparams ~msg:emsg in
    pure mp
  ) args in
  let%bind _ = forallM ~f:(fun (p, _) ->
    (* For each parameter there should be exactly one argument. *)
    if (List.count args ~f:(fun a -> (get_id p) = fst a)) <> 1
    then fail0 (sprintf "Parameter %s must occur exactly once in input.\n" (get_id p))
    else pure true
  ) cparams in
  (* Fold params into already initialized libraries, possibly shadowing *)
  let env = List.fold_left ~init:libenv args
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
  let%bind _ = forallM ~f:(fun (s, lc) ->
    let%bind t_lc = fromR @@ literal_type lc in
    let emsg () = mk_error0 
        (sprintf "Field %s : %s not defined in the contract\n" s (pp_typ t_lc)) in
    let%bind (_, ex) = tryM ~f:(fun (t, li) ->
        let%bind t1 = fromR @@ literal_type lc in
        let%bind t2 = fromR @@ literal_type li in
        if (s = t && (type_equiv t1 t2))
          then pure true else fail0 ""
    ) initcstate ~msg:emsg in
    pure ex
  ) curcstate in
  (* Each entry name is unique *)
  let%bind _ = forallM ~f:(fun (e, _) ->
    if (List.count curcstate ~f:(fun (e', _) -> e = e') > 1)
    then fail0 (sprintf "Field %s occurs more than once in input.\n" e)
    else pure true
  ) initcstate in
  (* Get only those fields from initcstate that are not in curcstate *)
  let filtered_init = List.filter initcstate 
    ~f:(fun (s, _) -> not (List.exists curcstate 
        ~f:(fun (s1, _) -> s = s1))) in
    (* Combine filtered list and curcstate *)
    pure (filtered_init @ curcstate)

(* Initialize a module with given arguments and initial balance *)
let init_module md initargs curargs init_bal bstate elibs =
  let {libs; contr; _} = md in
  let {cparams; cfields; _} = contr in
  let%bind initcstate =
    init_contract libs elibs cparams cfields initargs init_bal in
  let%bind curfields = create_cur_state_fields initcstate.fields curargs in
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
  | None -> fail0 @@ sprintf
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
  then fail0 @@ sprintf
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
  | _ -> fail0 @@ sprintf "Not a message literal: %s." (pp_literal m)

(* Subtract the amounts to be transferred *)
let post_process_msgs cstate outs =
  (* Evey outgoing message should carry an "_amount" tag *)
  let%bind amounts = mapM outs ~f:(fun l -> match l with
      | Msg es -> fromR @@ MessagePayload.get_amount es
      | _ -> fail0 @@ sprintf "Not a message literal: %s." (pp_literal l)) in
  let open Uint128 in
  let to_be_transferred = List.fold_left amounts ~init:zero
      ~f:(fun z a -> add z a) in
  let open ContractState in
  if (compare cstate.balance to_be_transferred) < 0
  then fail0 @@ sprintf
      "The balance is too low (%s) to transfer all the funds in the messages (%s)"
      (to_string cstate.balance) (to_string to_be_transferred)
  else
    let balance = sub cstate.balance to_be_transferred in
    pure {cstate with balance}

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
      ~f:(fun e (n, l) -> Env.bind e n l) in
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
  let new_events = conf'.events in
  (* Make sure that we aren't too generous and subract funds *)
  let%bind cstate'' = post_process_msgs cstate' new_msgs in

  (*Return new contract state, messages and events *)
  pure (cstate'', new_msgs, new_events, conf'.accepted)

