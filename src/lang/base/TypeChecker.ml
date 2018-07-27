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
open Utils
open Recursion
open ContractUtil

(* TODO: This dependency should be removed once types annotations are available in the AST *)
open PatternChecker

(* Instantiated the type environment *)
module SimpleTEnv = MakeTEnv(PlainTypes)
open SimpleTEnv

(**************************************************************)
(*             Auxiliary functions for typing                 *)
(**************************************************************)

(* Given a scrutinee type and a pattern,
   produce a list of ident -> type mappings for 
   all variables bound by the pattern *)
let assign_types_for_pattern sctyp pattern =
  let rec go atyp tlist p = match p with
    | Wildcard -> pure tlist
    | Binder x -> pure @@ tlist @ [(x, atyp)]
    | Constructor (cn, ps) ->
        let%bind arg_types = constr_pattern_arg_types atyp cn sctyp in
        let plen = List.length arg_types in
        let alen = List.length ps in
        let%bind _ = validate_param_length cn plen alen in
        let tps_pts = List.zip_exn arg_types ps in
        foldM ~init:tlist tps_pts
          ~f:(fun acc (t, pt) -> go t acc pt)
  in go sctyp [] pattern

(**************************************************************)
(*                   Typing expressions                       *)
(**************************************************************)

(* TODO: Check if the type is well-formed: support type variables *)
let rec type_expr tenv e = match e with
  | Literal l ->
      (* TODO: Check that literal is well-formed *)
      let%bind lt = literal_type l in
      pure @@ mk_qual_tp lt
  | Var i ->
      let%bind r = TEnv.resolveT tenv (get_id i) ~lopt:(Some (get_loc i)) in
      pure @@ (rr_typ r)
  |  Fun (arg, t, body) ->
      let%bind _ = TEnv.is_wf_type tenv t in
      let tenv' = TEnv.addT (TEnv.copy tenv) arg t in
      let%bind bt = type_expr tenv' body in
      pure @@ mk_qual_tp (FunType (t, bt.tp))
  | App (f, actuals) ->
      let%bind fres = wrap_err e @@ 
        TEnv.resolveT tenv (get_id f) ~lopt:(Some (get_loc f)) in
      let ftyp = (rr_typ fres).tp in
      wrap_err e @@ app_type tenv ftyp actuals
  | Builtin (i, actuals) ->
      wrap_err e @@ 
      let%bind tresults = mapM actuals
          ~f:(fun arg -> TEnv.resolveT tenv (get_id arg)
                 ~lopt:(Some (get_loc arg))) in
      let targs = List.map tresults ~f:(fun rr -> (rr_typ rr).tp) in
      let%bind (_, ret_typ, _) = BuiltInDictionary.find_builtin_op i targs in
      let%bind _ = TEnv.is_wf_type tenv ret_typ in
      pure @@ mk_qual_tp ret_typ
  | Let (i, t, lhs, rhs) ->
      (* Poor man's error reporting *)
      let%bind ityp = wrap_err e @@ type_expr tenv lhs in
      let tenv' = TEnv.addT (TEnv.copy tenv) i ityp.tp in
      type_expr tenv' rhs
  | Constr (cname, ts, actuals) ->
      let%bind _ = mapM ts ~f:(TEnv.is_wf_type tenv) in
      let open Datatypes.DataTypeDictionary in 
      let%bind (adt, constr) = lookup_constructor cname in
      let alen = List.length actuals in
      if (constr.arity <> alen)
      then fail @@ sprintf
          "Constructor %s expects %d arguments, but got %d."
          cname constr.arity alen
      else
        let%bind ftyp = elab_constr_type cname ts in
        (* Now type-check as a function application *)
        app_type tenv ftyp actuals
  | MatchExpr (x, clauses) ->
      if List.is_empty clauses
      then fail @@ sprintf
          "List of pattern matching clauses is empty:\n%s" (expr_str e)
      else
        let%bind sctyp = TEnv.resolveT tenv (get_id x)
            ~lopt:(Some (get_loc x)) in
        let sct = (rr_typ sctyp).tp in
        let msg = sprintf " of type %s" (pp_typ sct) in
        wrap_err e ~opt:msg (
          let%bind cl_types = mapM clauses ~f:(fun (ptrn, ex) ->
              type_check_match_branch tenv sct ptrn ex) in
          let%bind _ =
            assert_all_same_type (List.map ~f:(fun it -> it.tp) cl_types) in
          (* TODO: Move the PM checks to separate phase once type annotations are available in AST *)
          let%bind _ = pm_check sct clauses in
          (* Return the first type since all they are the same *)
          pure @@ List.hd_exn cl_types
        )
  | Fixpoint (f, t, body) ->
      wrap_err e @@ 
      let tenv' = TEnv.addT (TEnv.copy tenv) f t in
      let%bind bt = type_expr tenv' body in
      let%bind _ = assert_type_equiv t bt.tp in
      pure @@ mk_qual_tp t
  | TFun (tvar, body) ->
      let tenv' = TEnv.addV (TEnv.copy tenv) tvar in
      let%bind bt = type_expr tenv' body in
      pure @@ mk_qual_tp (PolyFun ((get_id tvar), bt.tp))
  | TApp (tf, arg_types) ->
      let%bind _ = mapM arg_types ~f:(TEnv.is_wf_type tenv) in
      let%bind tfres = TEnv.resolveT tenv (get_id tf)
          ~lopt:(Some (get_loc tf)) in
      let tftyp = (rr_typ tfres).tp in
      let%bind res_type = elab_tfun_with_args tftyp arg_types in
      let%bind _ = TEnv.is_wf_type tenv res_type in
      pure @@ mk_qual_tp res_type
  | Message bs ->
      let open PrimTypes in 
      let payload_type pld =
        match pld with
        | MTag s -> pure @@ mk_qual_tp string_typ
        | MLit l -> type_expr tenv (Literal l)
        | MVar i ->
            let%bind r = TEnv.resolveT tenv (get_id i)
                ~lopt:(Some (get_loc i)) in
            let rtp = (rr_typ r).tp in
            if is_sendable_type rtp
            then pure (rr_typ r)
            else fail @@ sprintf
              "Cannot send values of type %s." (pp_typ rtp)
      in
      let%bind typed_payloads =
        (* Make sure we resolve all the payload *)
        mapM bs ~f:(fun (s, pld) -> liftPair2 s @@ payload_type pld)
      in
      pure @@ mk_qual_tp @@ msg_typ

and app_type tenv ftyp actuals =
  (* Type-check function application *)  
  let%bind _ = TEnv.is_wf_type tenv ftyp in
  let%bind tresults = mapM actuals
      ~f:(fun arg -> TEnv.resolveT tenv (get_id arg) ~lopt:(Some (get_loc arg))) in
  let targs = List.map tresults ~f:(fun rr -> (rr_typ rr).tp) in
  let%bind res_type = fun_type_applies ftyp targs in
  let%bind _ = TEnv.is_wf_type tenv res_type in
  pure @@ mk_qual_tp res_type

and type_check_match_branch tenv styp ptrn e =
  let%bind new_typings = assign_types_for_pattern styp ptrn in
  let tenv' = TEnv.addTs (TEnv.copy tenv) new_typings in
  type_expr tenv' e

(**************************************************************)
(*                   Typing statements                        *)
(**************************************************************)

(* Auxiliaty structure for types of fields and BC components *)
type stmt_tenv = {
  pure   : TEnv.t;
  fields : TEnv.t;
  bc     : TEnv.t;
}

let rec type_stmts env stmts =
  let open PrimTypes in
  let open Datatypes.DataTypeDictionary in 
  match stmts with
  | [] -> pure env
  | s :: sts -> (match s with
      | Load (x, f) ->
          let%bind env' = wrap_serr s (
              let%bind fr = TEnv.resolveT env.fields (get_id f) in
              let pure' = TEnv.addT (TEnv.copy env.pure) x (rr_typ fr).tp in
              pure {env with pure = pure'}
            ) in
          type_stmts env' sts                            
      | Store (f, r) ->
          if List.mem ~equal:(fun s1 s2 -> s1 = s2)
              no_store_fields (get_id f) then
            wrap_serr s (
              fail @@ sprintf
                "Writing to the field `%s` is prohibited." (get_id f)) 
          else          
            let%bind _ = wrap_serr s (
                let%bind fr = TEnv.resolveT env.fields (get_id f) in
                let%bind r = TEnv.resolveT env.pure (get_id r) in
                assert_type_equiv (rr_typ fr).tp (rr_typ r).tp
              ) in
            type_stmts env sts            
      | Bind (x, e) ->
          let%bind ityp = wrap_serr s @@ type_expr env.pure e in
          let pure' = TEnv.addT (TEnv.copy env.pure) x ityp.tp in
          let env' = {env with pure = pure'} in
          type_stmts env' sts
      | ReadFromBC (x, bf) ->
          let%bind r = wrap_serr s @@ TEnv.resolveT env.bc bf in
          let bt = (rr_typ r).tp in
          let pure' = TEnv.addT (TEnv.copy env.pure) x bt in
          let env' = {env with pure = pure'} in
          type_stmts env' sts
      | MatchStmt (x, clauses) ->
          if List.is_empty clauses
          then wrap_serr s @@ fail @@ sprintf
              "List of pattern matching clauses is empty:\n%s" (stmt_str s)
          else
            let%bind sctyp = TEnv.resolveT env.pure (get_id x)
                ~lopt:(Some (get_loc x)) in
            let sct = (rr_typ sctyp).tp in
            let msg = sprintf " of type %s" (pp_typ sct) in
            let%bind _ = wrap_serr s ~opt:msg @@
              mapM clauses ~f:(fun (ptrn, ex) ->
                  type_match_stmt_branch env sct ptrn ex) in
            type_stmts env sts                      
      | AcceptPayment ->
          type_stmts env sts                      
      | SendMsgs i ->
          let%bind r = TEnv.resolveT env.pure (get_id i)
              ~lopt:(Some (get_loc i)) in
          let expected = list_typ msg_typ in
          let%bind _ = wrap_serr s @@
            assert_type_equiv expected (rr_typ r).tp in
          type_stmts env sts
      | _ ->
          fail @@ sprintf
            "Type-checking the statement %s is not supported yet."
            (stmt_str s)
    )
and type_match_stmt_branch env styp ptrn sts =
  let%bind new_typings = assign_types_for_pattern styp ptrn in
  let pure' = TEnv.addTs (TEnv.copy env.pure) new_typings in
  let env' = {env with pure = pure'} in
  type_stmts env' sts


(**************************************************************)
(*           Typing recursion principles and libraries        *)
(**************************************************************)

(* This is a really good sanity check: 
   it should always succeed! *)
let type_recursion_principles =
  let recs = List.map recursion_principles
      ~f:(fun ({lname = a; lexp = e}, c) -> (a, e, c)) in
  let env0 = TEnv.copy TEnv.mk in 
  mapM recs
    ~f:(fun (rn, body, expected) ->
        wrap_with_info
          (sprintf "Type error when checking recursion primitive %s:\n"
             (get_id rn)) @@
        let%bind ar = type_expr env0 body in
        let actual = ar.tp in
        let%bind _ = assert_type_equiv expected actual in
        pure (rn, actual))
        
(**************************************************************)
(*                    Typing a library                        *)
(**************************************************************)

let typed_rec_libs =
  let%bind _ = type_recursion_principles in      
  let recs = List.map recursion_principles
      ~f:(fun ({lname = a}, c) -> (a, c)) in
  let env = TEnv.mk in
  pure @@ (TEnv.addTs (TEnv.copy env) recs)


let type_library env0 {lentries = ents} =
  let%bind res =
    foldM ~init:env0 ents ~f:(fun env {lname=ln; lexp = le} ->
        let msg = sprintf
            "[%s] Type error in library %s:\n"
            (get_loc_str (get_loc ln)) (get_id ln) in
        let%bind tr = wrap_with_info msg (type_expr env le) in       
        pure @@ TEnv.addT (TEnv.copy env) ln tr.tp) in
  pure @@ TEnv.copy res

(*****************************************************************)
(*               Blockchain component typing                     *)
(*****************************************************************)

let bc_type_env =
  let open SimpleTEnv in
  let open PrimTypes in 
  let bc_elements =
    [(mk_ident TypeUtil.blocknum_name, bnum_typ)] in
  TEnv.addTs (TEnv.copy TEnv.mk) bc_elements
    

(*****************************************************************)
(*                 Typing entire contracts                       *)
(*****************************************************************)

let type_fields tenv flds =
  foldM flds ~init:TEnv.mk ~f:(fun fenv (fn, ft, fe) ->
      let msg = sprintf
          "[%s] Type error in field %s:\n"
          (get_loc_str (get_loc fn)) (get_id fn) in
      wrap_with_info msg @@
      let%bind ar = type_expr tenv fe in
      let actual = ar.tp in
      let%bind _ = assert_type_equiv ft actual in
      pure @@ TEnv.addT (TEnv.copy fenv) fn actual)

(* Type-check transition *)
let type_transition env0 tr =
  let {tname; tparams; tbody} = tr in
  let tenv0 = env0.pure in
  let tenv1 = TEnv.addTs tenv0 (append_implict_trans_params tparams) in
  let env = {env0 with pure = tenv1} in
  let msg = sprintf "[%s] Type error in transition %s:\n"
      (get_loc_str (get_loc tname)) (get_id tname) in
  wrap_with_info msg @@
  type_stmts env tbody

let type_module md elibs =
  let {cname; libs; contr} = md in
  let {cname; cparams; cfields; ctrans} = contr in
  let msg = sprintf "Type error in contract %s:\n" (get_id cname) in
  wrap_with_info msg @@
  
  (* Step 0: Type check recursion principles *)
  let%bind tenv0 = typed_rec_libs in

  (* Step 1: Type check external libraries *)
  (* TODO: Cache this information unless its version changed! *)
  let%bind tenv1 = foldM elibs ~init:tenv0
      ~f:(fun acc elib -> type_library acc elib) in

  (* Step 2: Type check internal libraries *)
  let%bind tenv2 = type_library tenv1 libs in

  (* Step 3: Adding typed contract parameters (incl. implicit ones) *)
  let params = append_implict_contract_params cparams in
  let tenv3 = TEnv.addTs tenv2 params in

  (* Step 4: Type-check fields and add balance *)
  let%bind fenv0 = type_fields tenv3 cfields in
  let (bn, bt) = balance_field in
  let fenv = TEnv.addT fenv0 bn bt in

  (* Step 5: Form a general environment for checking transitions *)
  let env = {pure= tenv3; fields= fenv; bc= bc_type_env} in

  (* Step 6: Type-checking all transitions in batch *)
  let%bind _ = mapM ctrans ~f:(fun tr -> type_transition env tr) in

  (* TODO: Improve the type checker, so it would report more than one
     type error. E.g., check independent libraries "in parallel".  *)

  (* Return pure environment *)  
  pure env

