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
open ParserUtil
open Core
open Result.Let_syntax
open MonadUtil
open TypeUtil
open Datatypes
open BuiltIns
open Recursion
open ContractUtil
open Utils
    
(* TODO: This dependency should be removed once types annotations are available in the AST *)
open PatternChecker

(* Instantiated the type environment *)
module SimpleTEnv = MakeTEnv(PlainTypes)(ParserRep)
open SimpleTEnv

(**************************************************************)
(*             Auxiliary functions for typing                 *)
(**************************************************************)

(* Lift 'rep ident to (inferred_type * 'rep) ident *)
let add_type_to_ident i typ =
  match i with
  | Ident (name, rep) -> Ident (name, (typ, rep))

(* Given a scrutinee type and a pattern,
   produce a list of ident -> type mappings for 
   all variables bound by the pattern *)
let assign_types_for_pattern sctyp pattern =
  let rec go atyp tlist p = match p with
    | Wildcard -> pure (Wildcard, tlist)
    | Binder x -> pure @@ (Binder (add_type_to_ident x (mk_qual_tp atyp)), (x, atyp) :: tlist)
    | Constructor (cn, ps) ->
        let%bind arg_types = constr_pattern_arg_types atyp cn in
        let plen = List.length arg_types in
        let alen = List.length ps in
        let%bind _ = validate_param_length cn plen alen in
        let tps_pts = List.zip_exn arg_types ps in
        let%bind (typed_ps, tps) =
          foldrM ~init:([], tlist) tps_pts
            ~f:(fun (ps, ts) (t, pt) ->
                let%bind (p, tss) = go t ts pt in
                pure @@ (p :: ps, tss)) in
        pure @@ (Constructor (cn, typed_ps), tps)
  in go sctyp [] pattern

(**************************************************************)
(*                   Typing expressions                       *)
(**************************************************************)

(* TODO: Check if the type is well-formed: support type variables *)
let rec type_expr tenv (erep : 'rep expr_annot) get_loc =
  let (e, rep) = erep in
  match e with
  | Literal l ->
      let%bind lt = literal_type l in
      pure @@ (Literal l, (mk_qual_tp lt, rep))
  | Var i ->
      let%bind r = TEnv.resolveT tenv (get_id i) ~lopt:(Some (get_rep i)) in
      let typ = rr_typ r in
      pure @@ (Var (add_type_to_ident i typ), (typ, rep))
  |  Fun (arg, t, body) ->
      wrap_err e get_rep @@ 
      let%bind _ = TEnv.is_wf_type tenv t in
      let tenv' = TEnv.addT (TEnv.copy tenv) arg t in
      let%bind (_, (bt, _)) as b = type_expr tenv' body get_loc in
      let typed_arg = add_type_to_ident arg (mk_qual_tp t) in
      pure @@ (Fun (typed_arg, t, b), (mk_qual_tp (FunType (t, bt.tp)), rep))
  | App (f, actuals) ->
      wrap_err e  get_rep @@ 
      let%bind fres = TEnv.resolveT tenv (get_id f) ~lopt:(Some (get_rep f)) in
      let%bind (typed_actuals, apptyp) = app_type tenv (rr_typ fres).tp actuals in
      let typed_f = add_type_to_ident f (rr_typ fres) in
      pure @@ (App (typed_f, typed_actuals), (apptyp, rep))
  | Builtin (i, actuals) ->
      wrap_err e  get_rep @@ 
      let%bind (targs, typed_actuals) = type_actuals tenv actuals in
      let%bind (_, ret_typ, _) = BuiltInDictionary.find_builtin_op i targs in
      let%bind _ = TEnv.is_wf_type tenv ret_typ in
      let q_ret_typ = mk_qual_tp ret_typ in
      pure @@ (Builtin (add_type_to_ident i q_ret_typ, typed_actuals), (q_ret_typ, rep))
  | Let (i, topt, lhs, rhs) ->
      (* Poor man's error reporting *)
      let%bind (_, (ityp, _)) as checked_lhs = wrap_err e  get_rep @@ type_expr tenv lhs get_loc in
      let tenv' = TEnv.addT (TEnv.copy tenv) i ityp.tp in
      let typed_i = add_type_to_ident i ityp in
      let%bind (_, (rhstyp, _)) as checked_rhs = type_expr tenv' rhs get_loc in
      pure @@ (Let (typed_i, topt, checked_lhs, checked_rhs), (rhstyp, rep))
  | Constr (cname, ts, actuals) ->
      let%bind _ = mapM ts ~f:(TEnv.is_wf_type tenv) in
      let open Datatypes.DataTypeDictionary in 
      let%bind (_, constr) = lookup_constructor cname in
      let alen = List.length actuals in
      if (constr.arity <> alen)
      then fail @@ sprintf
          "Constructor %s expects %d arguments, but got %d."
          cname constr.arity alen
      else
        let%bind ftyp = elab_constr_type cname ts in
        (* Now type-check as a function application *)
        let%bind (typed_actuals, apptyp) = app_type tenv ftyp actuals in
        pure @@ (Constr (cname, ts, typed_actuals), (apptyp, rep))
  | MatchExpr (x, clauses) ->
      if List.is_empty clauses
      then fail @@ sprintf
          "List of pattern matching clauses is empty:\n%s" (pp_expr e)
      else
        let%bind sctyp = TEnv.resolveT tenv (get_id x)
            ~lopt:(Some (get_rep x)) in
        let sct = (rr_typ sctyp).tp in
        let msg = sprintf " of type %s" (pp_typ sct) in
        wrap_err e  get_rep ~opt:msg (
          let%bind typed_clauses = mapM clauses ~f:(fun (ptrn, ex) ->
              type_check_match_branch tenv sct ptrn ex get_loc) in
          let cl_types = List.map typed_clauses ~f:(fun (_, (_, (t, _))) -> t) in
          let%bind _ =
            assert_all_same_type (List.map ~f:(fun it -> it.tp) cl_types) in
          (* TODO: Move the PM checks to separate phase once type annotations are available in AST *)
          let%bind _ = pm_check sct clauses in
          (* Return the first type since all they are the same *)
          pure @@ (MatchExpr (add_type_to_ident x (rr_typ sctyp), typed_clauses), (List.hd_exn cl_types, rep))
        )
  | Fixpoint (f, t, body) ->
      wrap_err e get_rep @@ 
      let tenv' = TEnv.addT (TEnv.copy tenv) f t in
      let%bind (_, (bt, _)) as typed_b = type_expr tenv' body get_loc in
      let%bind _ = assert_type_equiv t bt.tp in
      pure @@ (Fixpoint (add_type_to_ident f (mk_qual_tp t), t, typed_b), (mk_qual_tp t, rep))
  | TFun (tvar, body) ->
      let tenv' = TEnv.addV (TEnv.copy tenv) tvar in
      let%bind (_, (bt, _)) as typed_b = type_expr tenv' body get_loc in
      let typed_tvar = add_type_to_ident tvar bt in
      pure @@ (TFun (typed_tvar, typed_b), (mk_qual_tp (PolyFun ((get_id tvar), bt.tp)), rep))
  | TApp (tf, arg_types) ->
      let%bind _ = mapM arg_types ~f:(TEnv.is_wf_type tenv) in
      let%bind tfres = TEnv.resolveT tenv (get_id tf)
          ~lopt:(Some (get_rep tf)) in
      let tf_rr = rr_typ tfres in
      let tftyp = tf_rr.tp in
      let%bind res_type = elab_tfun_with_args tftyp arg_types in
      let%bind _ = TEnv.is_wf_type tenv res_type in
      pure @@ (TApp (add_type_to_ident tf tf_rr, arg_types), (mk_qual_tp res_type, rep))
  | Message bs ->
      let open PrimTypes in 
      let payload_type pld =
        (match pld with
         | MTag _ as m -> pure @@ m
         | MLit l as m ->
             let%bind _ = type_expr tenv (Literal l, rep) get_loc 
             in pure @@ m
         | MVar i ->
             let%bind r = TEnv.resolveT tenv (get_id i)
                 ~lopt:(Some (get_rep i)) in
             let t = rr_typ r in
             let rtp = t.tp in
             if is_storable_type rtp
             then pure @@ MVar (add_type_to_ident i t)
             else fail @@ sprintf
                 "Cannot send values of type %s." (pp_typ rtp))
      in
      let%bind typed_bs =
        (* Make sure we resolve all the payload *)
        mapM bs ~f:(fun (s, pld) -> liftPair2 s @@ payload_type pld)
      in
      pure @@ (Message typed_bs, (mk_qual_tp @@ msg_typ, rep))

and app_type tenv ftyp actuals =
  (* Type-check function application *)  
  let%bind _ = TEnv.is_wf_type tenv ftyp in
  let%bind (targs, typed_actuals) = type_actuals tenv actuals in
  let%bind res_type = fun_type_applies ftyp targs in
  let%bind _ = TEnv.is_wf_type tenv res_type in
  pure @@ (typed_actuals, mk_qual_tp res_type)

and type_check_match_branch tenv styp ptrn e get_loc =
  let%bind (new_p, new_typings) = assign_types_for_pattern styp ptrn in
  let tenv' = TEnv.addTs (TEnv.copy tenv) new_typings in
  let%bind typed_e = type_expr tenv' e get_loc in
  pure @@ (new_p, typed_e)

and type_actuals tenv actuals =
  let%bind tresults = mapM actuals
      ~f:(fun arg -> TEnv.resolveT tenv (get_id arg)
             ~lopt:(Some (get_rep arg))) in
  let tqargs = List.map tresults ~f:rr_typ in
  let targs = List.map tqargs ~f:(fun rr -> rr.tp) in
  let actuals_with_types =
    match List.zip actuals tqargs with
    | Some l -> l
    | None -> raise (InternalError "Different number of actuals and Types of actuals")  in
  let typed_actuals = List.map actuals_with_types ~f:(fun (a, t) -> add_type_to_ident a t) in
  pure @@ (targs, typed_actuals)

(**************************************************************)
(*                   Typing statements                        *)
(**************************************************************)

(* Auxiliaty structure for types of fields and BC components *)
type stmt_tenv = {
  pure   : TEnv.t;
  fields : TEnv.t;
  bc     : TEnv.t;
}

let add_stmt_to_stmts_env s repstmts =
  match repstmts with
  | (stmts, env) -> (s :: stmts, env)

let rec type_stmts env stmts get_loc =
  let open PrimTypes in
  let open Datatypes.DataTypeDictionary in 
  match stmts with
  | [] -> pure ([], env)
  | ((s, rep) as stmt) :: sts ->
      (match s with
       | Load (x, f) ->
           let%bind (next_env, ident_type) = wrap_serr s get_rep (
               let%bind fr = TEnv.resolveT env.fields (get_id f) in
               let pure' = TEnv.addT (TEnv.copy env.pure) x (rr_typ fr).tp in
               let next_env = {env with pure = pure'} in
               pure @@ (next_env, rr_typ fr)
             ) in
           let%bind checked_stmts = type_stmts next_env sts get_loc in
           let typed_x = add_type_to_ident x ident_type in
           let typed_f = add_type_to_ident f ident_type in
           pure @@ add_stmt_to_stmts_env (Load (typed_x, typed_f), rep) checked_stmts
       | Store (f, r) ->
           if List.mem ~equal:(fun s1 s2 -> s1 = s2)
               no_store_fields (get_id f) then
             wrap_serr s  get_rep (
               fail @@ sprintf
                 "Writing to the field `%s` is prohibited." (get_id f)) 
           else          
             let%bind (checked_stmts, f_type, r_type) = wrap_serr s get_rep (
                 let%bind fr = TEnv.resolveT env.fields (get_id f) in
                 let%bind r = TEnv.resolveT env.pure (get_id r) in
                 let%bind _ = assert_type_equiv (rr_typ fr).tp (rr_typ r).tp in
                 let%bind checked_stmts = type_stmts env sts get_loc in
                 pure @@ (checked_stmts, rr_typ fr, rr_typ r)
               ) in
             let typed_f = add_type_to_ident f f_type in
             let typed_r = add_type_to_ident r r_type in
             pure @@ add_stmt_to_stmts_env (Store (typed_f, typed_r), rep) checked_stmts
      | Bind (x, e) ->
          let%bind (_, (ityp, _)) as checked_e = wrap_serr s get_rep @@ type_expr env.pure e get_loc in
          let pure' = TEnv.addT (TEnv.copy env.pure) x ityp.tp in
          let env' = {env with pure = pure'} in
          let%bind checked_stmts = type_stmts env' sts get_loc in
          let typed_x = add_type_to_ident x ityp in
          pure @@ add_stmt_to_stmts_env (Bind (typed_x, checked_e), rep) checked_stmts
      | ReadFromBC (x, bf) ->
          let%bind r = wrap_serr s get_rep @@ TEnv.resolveT env.bc bf in
          let x_type = rr_typ r in
          let bt = x_type.tp in
          let pure' = TEnv.addT (TEnv.copy env.pure) x bt in
          let env' = {env with pure = pure'} in
          let%bind checked_stmts = type_stmts env' sts get_loc in
          let typed_x = add_type_to_ident x x_type in
          pure @@ add_stmt_to_stmts_env (ReadFromBC (typed_x, bf), rep) checked_stmts
      | MatchStmt (x, clauses) ->
          if List.is_empty clauses
          then wrap_serr s get_rep @@ fail @@ sprintf
              "List of pattern matching clauses is empty:\n%s" (stmt_str stmt)
          else
            let%bind sctyp = TEnv.resolveT env.pure (get_id x)
                ~lopt:(Some (get_rep x)) in
            let sctype = rr_typ sctyp in
            let sct = sctype.tp in
            let msg = sprintf " of type %s" (pp_typ sct) in
            let typed_x = add_type_to_ident x sctype in
            let%bind checked_clauses = wrap_serr s get_rep ~opt:msg @@
              mapM clauses ~f:(fun (ptrn, ex) ->
                  type_match_stmt_branch env sct ptrn ex get_loc ) in
            let%bind checked_stmts = type_stmts env sts get_loc in
            pure @@ add_stmt_to_stmts_env (MatchStmt (typed_x, checked_clauses), rep) checked_stmts
      | AcceptPayment ->
          let%bind checked_stmts = type_stmts env sts get_loc in
          pure @@ add_stmt_to_stmts_env (AcceptPayment, rep) checked_stmts
      | SendMsgs i ->
          let%bind r = TEnv.resolveT env.pure (get_id i)
              ~lopt:(Some (get_rep i)) in
          let i_type = rr_typ r in
          let expected = list_typ msg_typ in
          let%bind _ = wrap_serr s get_rep @@
            assert_type_equiv expected i_type.tp in
          let typed_i = add_type_to_ident i i_type in
          let%bind checked_stmts = type_stmts env sts get_loc in
          pure @@ add_stmt_to_stmts_env (SendMsgs typed_i, rep) checked_stmts
      | CreateEvnt (st, i) ->
          let%bind r = TEnv.resolveT env.pure (get_id i)
              ~lopt:(Some (get_loc (get_rep i))) in
          (* An event is a named message, hence msg_typ. *)
          let i_type = rr_typ r in
          let expected = msg_typ in
          let%bind _ = wrap_serr s get_rep @@
            assert_type_equiv expected i_type.tp in
          let typed_i = add_type_to_ident i i_type in
          let%bind checked_stmts = type_stmts env sts get_loc in
          pure @@ add_stmt_to_stmts_env (CreateEvnt (st, typed_i), rep) checked_stmts
      | _ ->
          fail @@ sprintf
            "Type-checking the statement %s is not supported yet."
            (stmt_str stmt)
    )
and type_match_stmt_branch env styp ptrn sts get_loc =
  let%bind (new_p, new_typings) = assign_types_for_pattern styp ptrn in
  let pure' = TEnv.addTs (TEnv.copy env.pure) new_typings in
  let env' = {env with pure = pure'} in
  let%bind (new_stmts, _) = type_stmts env' sts get_loc in
  pure @@ (new_p, new_stmts)


(**************************************************************)
(*           Typing recursion principles and libraries        *)
(**************************************************************)

(* This is a really good sanity check: 
   it should always succeed! *)
let type_recursion_principles get_loc =
  let recs = List.map recursion_principles
      ~f:(fun ({lname = a; lexp = e}, c) -> (a, e, c)) in
  let env0 = TEnv.copy TEnv.mk in 
  mapM recs
    ~f:(fun (rn, body, expected) ->
        wrap_with_info
          (sprintf "Type error when checking recursion primitive %s:\n"
             (get_id rn)) @@
        let%bind (_, (ar, _)) = type_expr env0 body get_loc in
        let actual = ar.tp in
        let%bind _ = assert_type_equiv expected actual in
        pure (rn, actual))


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

open TypeHelpers
    
module Typechecker_Contracts
    (* TODO: This needs to be parameterized rather than bound to ParserRep.
       Cannot be done until type_stmt has been generalized. *)
  (* (SR : Rep) *)
  (*     (ER : Rep) *) = struct

  module SR = ParserRep
  module ER = ParserRep
  module STR = SR
  module ETR = TypecheckerERep (ER)
  module UntypedContract = Contract (SR) (ER)
  module TypedContract = Contract (STR) (ETR)

  include TypedContract

  module TypeEnv = MakeTEnv(PlainTypes)(ER)
  open TypeEnv

  (* Auxiliaty structure for types of fields and BC components *)
(*  type stmt_tenv = {
    pure   : TEnv.t;
    fields : TEnv.t;
    bc     : TEnv.t;
    } *)
      
  open UntypedContract

  let add_type_to_id id t : ETR.rep ident =
    match id with
    | Ident (s, r) -> Ident (s, ETR.mk_rep r t)
  
  let type_transition (env0 : stmt_tenv) (tr : UntypedContract.transition) : (TypedContract.transition * stmt_tenv, string) result  =
    let {tname; tparams; tbody} = tr in
    let tenv0 = env0.pure in
    let lift_ident_e (id, t) = (add_type_to_id id (mk_qual_tp t), t) in
    let typed_tparams = List.map tparams ~f:lift_ident_e in
    let tenv1 = TEnv.addTs tenv0 (append_implict_trans_params tparams ER.mk_msg_payload_id_address ER.mk_msg_payload_id_uint128) in
    let env = {env0 with pure = tenv1} in
    let msg = sprintf "[%s] Type error in transition %s:\n"
        (get_loc_str (SR.get_loc (get_rep tname))) (get_id tname) in
    let%bind (typed_stmts, new_tenv) = wrap_with_info msg @@ type_stmts env tbody ER.get_loc in
    pure @@ ({ TypedContract.tname = tname ;
               TypedContract.tparams = typed_tparams;
               TypedContract.tbody = typed_stmts }, new_tenv)
        

  (*****************************************************************)
  (*                 Typing entire contracts                       *)
  (*****************************************************************)
  let type_fields tenv flds =
    let%bind (typed_flds, new_env) = foldM flds ~init:([], TEnv.mk)
        ~f:(fun (acc, fenv) (fn, ft, fe) ->
            let msg = sprintf
                "[%s] Type error in field %s:\n"
                (get_loc_str (ER.get_loc (get_rep fn))) (get_id fn) in
            wrap_with_info msg @@
            let%bind (_, (ar, _)) as typed_expr = type_expr tenv fe ER.get_loc in
            let actual = ar.tp in
            let%bind _ = assert_type_equiv ft actual in
            let typed_fs = add_type_to_id fn ar in
            if is_storable_type ft then
              pure @@ ((typed_fs, ft, typed_expr) :: acc,
                       TEnv.addT (TEnv.copy fenv) fn actual)
            else fail @@ sprintf "Values of the type \"%s\" cannot be stored." (pp_typ ft)) in
        pure @@ (List.rev typed_flds, new_env)

  (**************************************************************)
  (*                    Typing libraries                        *)
  (**************************************************************)
      
  let typed_rec_libs =
    let%bind _ = type_recursion_principles ER.get_loc in      
    let recs = List.map recursion_principles
        ~f:(fun ({lname = a; _}, c) -> (a, c)) in
    let env = TEnv.mk in
    pure @@ (TEnv.addTs (TEnv.copy env) recs)
            
  let type_library env0 { lname ; lentries = ents } =
    let%bind (typed_entries, new_tenv) =
      foldM ~init:([], env0) ents ~f:(fun (acc, env) {lname=ln; lexp = le} ->
          let msg = sprintf
              "[%s] Type error in library %s:\n"
              (get_loc_str (ER.get_loc (get_rep ln))) (get_id ln) in
          let%bind (_, (tr, _)) as typed_e = wrap_with_info msg (type_expr env le ER.get_loc ) in
          let typed_ln = add_type_to_id ln tr in
          pure @@ ({ TypedContract.lname = typed_ln;
                     TypedContract.lexp = typed_e }:: acc,
                   TEnv.addT (TEnv.copy env) ln tr.tp)) in
    pure @@ ( { TypedContract.lname = lname ;
                TypedContract.lentries = List.rev typed_entries }, TEnv.copy new_tenv)

  (* TODO, issue #179: Re-introduce this when library cache can store typed ASTs
  (* type library, handling cache as necessary. *)
  let type_library_cache (tenv : TEnv.t) (elib : UntypedContract.library)  =
    (* We are caching TypeEnv = MakeTEnv(PlainTypes)(ER) *)
    let module STC = TypeCache.StdlibTypeCacher(MakeTEnv)(PlainTypes) (STR) (ER) (UntypedContract) in
    let open STC in
    (* Check if we have the type info in cache. *)
    match get_lib_tenv_cache tenv elib with
    | Some tenv' ->
        (* Use cached entries. *)
        (tenv', "")
    | None ->
        (* Couldn't find in cache. Actually type the library. *)
        let res = type_library tenv elib in
        (match res with
         | Error msg -> (tenv, msg)
         | Ok (_, tenv') as lib_res -> 
             (* Since we don't have this in cache, cache it now. *)
             cache_lib_tenv tenv' elib;
             (lib_res, "")
        )
  *)
            
  let type_module (md : UntypedContract.cmodule) (elibs : UntypedContract.library list) : (TypedContract.cmodule * stmt_tenv, string) result =
    let {cname = mod_cname; libs; elibs = mod_elibs; contr} = md in
    let {cname = ctr_cname; cparams; cfields; ctrans} = contr in
    let msg = sprintf "Type error(s) in contract %s:\n" (get_id ctr_cname) in
    wrap_with_info msg @@
    
    (* Step 0: Type check recursion principles *)
    let%bind tenv0 = typed_rec_libs in
    
    (* Step 1: Type check external libraries *)
    (* Step 2: Type check contract library, if defined. *)
    let all_libs = match libs with
      | Some lib -> List.append elibs (lib::[])
      | None -> elibs
    in
    let ((libs, tenv), emsgs) = List.fold_left all_libs ~init:(([], tenv0), "")
        ~f:(fun ((lib_acc, tenv_acc), emsgs_acc) elib ->
            (* TODO, issue #179: Re-introduce this when library cache can store typed ASTs
               let (tenv', emsg) = type_library_cache tenv_acc elib in *)
            let ((typed_libraries, tenv'), emsg) =
              match type_library tenv_acc elib with
              | Ok (t_lib, t_env) -> ((t_lib :: lib_acc, t_env), emsgs_acc)
              | Error msg ->
                  let emsgs' = if msg = "" then emsgs_acc else (emsgs_acc ^ "\n\n" ^ msg) in
                  ((lib_acc, tenv_acc), emsgs')
            in
            (* Updated env and error messages are what we accummulate in the fold. *)
            ((typed_libraries, tenv'), emsg)
          )
    in
    
    (* Step 3: Adding typed contract parameters (incl. implicit ones) *)
    let params = append_implict_contract_params cparams in
    let tenv3 = TEnv.addTs tenv params in
    
    (* Step 4: Type-check fields and add balance *)
    let (typed_fields, fenv0), femsgs0 = 
      match type_fields tenv3 cfields with
      | Error msg -> (([], tenv3), emsgs ^ "\n\n" ^ msg)
      | Ok (typed_fields, tenv) -> ((typed_fields, tenv), emsgs)
    in
    let (bn, bt) = balance_field in
    let fenv = TEnv.addT fenv0 bn bt in
    
    (* Step 5: Form a general environment for checking transitions *)
    let env = {pure= tenv3; fields= fenv; bc= bc_type_env} in
    
    (* Step 6: Type-checking all transitions in batch *)
    let (t_trans, emsgs') = List.fold_left ~init:([], femsgs0) ctrans 
        ~f:(fun (trans_acc, emsgs) tr -> 
            match type_transition env tr with
            | Error msg -> (trans_acc, emsgs ^ "\n\n" ^ msg)
            | Ok (typed_trans, _) -> (typed_trans :: trans_acc, emsgs)
          ) in
    let typed_trans = List.rev t_trans in

    (* Step 7: Lift contract parameters to ETR.rep ident *)
    let typed_params = List.map params
        ~f:(fun (id, t) -> (add_type_to_id id (mk_qual_tp t), t)) in
    
    if emsgs' = ""
    (* Return pure environment *)  
    then pure ({TypedContract.cname = mod_cname;
                TypedContract.libs = List.hd libs;
                TypedContract.elibs = mod_elibs;
                TypedContract.contr =
                  {TypedContract.cname = ctr_cname;
                   TypedContract.cparams = typed_params;
                   TypedContract.cfields = typed_fields;
                   TypedContract.ctrans = typed_trans}}, env)
    (* Return error messages *)
    else fail @@ emsgs'

end
