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
open MonadUtil
open Result.Let_syntax
open TypeUtil
open Datatypes
open BuiltIns
open ContractUtil
open Utils
open PrimTypes
    
(*******************************************************)
(*                   Annotations                       *)
(*******************************************************)

module TypecheckerERep (R : Rep) = struct
  type rep = PlainTypes.t inferred_type * R.rep
  [@@deriving sexp]
 
  let get_loc r = match r with | (_, rr) -> R.get_loc rr

  let mk_id s t =
    match s with
    | Ident (n, r) -> Ident (n, (PlainTypes.mk_qualified_type t, r))

  let mk_id_address s = mk_id (R.mk_id_address s) (bystrx_typ address_length)
  let mk_id_uint128 s = mk_id (R.mk_id_uint128 s) uint128_typ
  let mk_id_uint32 s = mk_id (R.mk_id_uint128 s) uint32_typ
  let mk_id_bnum    s = mk_id (R.mk_id_bnum s) bnum_typ
  let mk_id_string  s = mk_id (R.mk_id_string s) string_typ
  
  let mk_rep (r : R.rep) (t : PlainTypes.t inferred_type) = (t, r)
  
  let parse_rep s = (PlainTypes.mk_qualified_type uint128_typ, R.parse_rep s)
  let get_rep_str r = match r with | (_, rr) -> R.get_rep_str rr

  let get_type (r : rep) = fst r
end

(*****************************************************************)
(*                 Typing entire contracts                       *)
(*****************************************************************)

module ScillaTypechecker
  (SR : Rep)
  (ER : Rep) = struct

  module STR = SR
  module ETR = TypecheckerERep (ER)
  module UntypedSyntax = ScillaSyntax (SR) (ER)
  module TypedSyntax = ScillaSyntax (STR) (ETR)
  include TypedSyntax
  include ETR
  
  module TU = TypeUtilities
  module TBuiltins = ScillaBuiltIns (SR) (ER)
  module TypeEnv = TU.MakeTEnv(PlainTypes)(ER)
  module CU = ScillaContractUtil (SR) (ER)

  open TU
  open TBuiltins
  open TypeEnv
  open UntypedSyntax
      
  let wrap_type_err e ?opt:(opt = "") = wrap_err e "typechecking" ~opt:opt
  let wrap_type_serr s ?opt:(opt = "") = wrap_serr s "typechecking" ~opt:opt
      
  (*****************************************************************)
  (*               Blockchain component typing                     *)
  (*****************************************************************)
      
  let bc_types =
    let open PrimTypes in 
    [(TypeUtil.blocknum_name, bnum_typ)]

  let lookup_bc_type x =
    match List.findi bc_types ~f:(fun _ (f, _) -> f = x) with
    | Some (_, (_, t)) -> pure @@ t
    | None -> fail0 @@ sprintf "Unknown blockchain field %s." x
  
  (**************************************************************)
  (*             Auxiliary functions for typing                 *)
  (**************************************************************)

  (* Lift 'rep ident to (inferred_type * 'rep) ident *)
  let add_type_to_ident i typ =
    match i with
    | Ident (name, rep) -> Ident (name, ETR.mk_rep rep typ)

  (* Given a scrutinee type and a pattern,
     produce a list of ident -> type mappings for 
     all variables bound by the pattern *)
  let assign_types_for_pattern sctyp pattern =
    let rec go atyp tlist p = match p with
      | Wildcard -> pure (TypedSyntax.Wildcard, tlist)
      | Binder x -> pure @@ (TypedSyntax.Binder (add_type_to_ident x (mk_qual_tp atyp)), (x, atyp) :: tlist)
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
          pure @@ (TypedSyntax.Constructor (cn, typed_ps), tps)
    in go sctyp [] pattern

  (**************************************************************)
  (*                   Typing expressions                       *)
  (**************************************************************)

  let rec type_expr tenv (erep : UntypedSyntax.expr_annot) =
    let (e, rep) = erep in
    match e with
    | Literal l ->
        let%bind lt = literal_type l in
        pure @@ (TypedSyntax.Literal l, (mk_qual_tp lt, rep))
    | Var i ->
        let%bind r = TEnv.resolveT tenv (get_id i) ~lopt:(Some (get_rep i)) in
        let typ = rr_typ r in
        pure @@ (TypedSyntax.Var (add_type_to_ident i typ), (typ, rep))
    |  Fun (arg, t, body) ->
        let%bind _ = TEnv.is_wf_type tenv t in
        let tenv' = TEnv.addT (TEnv.copy tenv) arg t in
        let%bind (_, (bt, _)) as b = type_expr tenv' body in
        let typed_arg = add_type_to_ident arg (mk_qual_tp t) in
        pure @@ (TypedSyntax.Fun (typed_arg, t, b), (mk_qual_tp (FunType (t, bt.tp)), rep))
    | App (f, actuals) ->
        wrap_type_err erep @@ 
        let%bind fres = TEnv.resolveT tenv (get_id f) ~lopt:(Some (get_rep f)) in
        let%bind (typed_actuals, apptyp) = app_type tenv (rr_typ fres).tp actuals in
        let typed_f = add_type_to_ident f (rr_typ fres) in
        pure @@ (TypedSyntax.App (typed_f, typed_actuals), (apptyp, rep))
    | Builtin (b, actuals) ->
        wrap_type_err erep @@ 
        let%bind (targs, typed_actuals) = type_actuals tenv actuals in
        let%bind (_, ret_typ, _) = BuiltInDictionary.find_builtin_op b targs in
        let%bind _ = TEnv.is_wf_type tenv ret_typ in
        let q_ret_typ = mk_qual_tp ret_typ in
        let q_ret_tag = ETR.mk_rep rep q_ret_typ in
        pure @@ (TypedSyntax.Builtin ((fst b, q_ret_tag), typed_actuals), (q_ret_typ, rep))
    | Let (i, topt, lhs, rhs) ->
        (* Poor man's error reporting *)
        let%bind (_, (ityp, _)) as checked_lhs = wrap_type_err erep @@ type_expr tenv lhs in
        let%bind _ = match topt with Some tannot -> assert_type_equiv tannot ityp.tp | None -> pure () in
        let tenv' = TEnv.addT (TEnv.copy tenv) i ityp.tp in
        let typed_i = add_type_to_ident i ityp in
        let%bind (_, (rhstyp, _)) as checked_rhs = type_expr tenv' rhs in
        pure @@ (TypedSyntax.Let (typed_i, topt, checked_lhs, checked_rhs), (rhstyp, rep))
    | Constr (cname, ts, actuals) ->
        let%bind _ = mapM ts ~f:(TEnv.is_wf_type tenv) in
        let open Datatypes.DataTypeDictionary in 
        let%bind (_, constr) = lookup_constructor cname in
        let alen = List.length actuals in
        if (constr.arity <> alen)
        then fail0 @@ (sprintf
            "Constructor %s expects %d arguments, but got %d."
            cname constr.arity alen)
        else
          let%bind ftyp = elab_constr_type cname ts in
          (* Now type-check as a function application *)
          let%bind (typed_actuals, apptyp) = app_type tenv ftyp actuals in
          pure @@ (TypedSyntax.Constr (cname, ts, typed_actuals), (apptyp, rep))
    | MatchExpr (x, clauses) ->
        if List.is_empty clauses
        then fail0 @@ sprintf
            "List of pattern matching clauses is empty:\n%s" (pp_expr e)
        else
          let%bind sctyp = TEnv.resolveT tenv (get_id x)
              ~lopt:(Some (get_rep x)) in
          let sct = (rr_typ sctyp).tp in
          let msg = sprintf " of type %s" (pp_typ sct) in
          wrap_type_err erep ~opt:msg (
            let%bind typed_clauses = mapM clauses ~f:(fun (ptrn, ex) ->
                type_check_match_branch tenv sct ptrn ex) in
            let cl_types = List.map typed_clauses ~f:(fun (_, (_, (t, _))) -> t) in
            let%bind _ =
              assert_all_same_type (List.map ~f:(fun it -> it.tp) cl_types) in
            (* Return the first type since all they are the same *)
            pure @@ (TypedSyntax.MatchExpr
                       (add_type_to_ident x (rr_typ sctyp),
                        typed_clauses),
                     (List.hd_exn cl_types, rep))
          )
    | Fixpoint (f, t, body) ->
        wrap_type_err erep @@ 
        let tenv' = TEnv.addT (TEnv.copy tenv) f t in
        let%bind (_, (bt, _)) as typed_b = type_expr tenv' body in
        let%bind _ = assert_type_equiv t bt.tp in
        pure @@ (TypedSyntax.Fixpoint (add_type_to_ident f (mk_qual_tp t), t, typed_b), (mk_qual_tp t, rep))
    | TFun (tvar, body) ->
        let tenv' = TEnv.addV (TEnv.copy tenv) tvar in
        let%bind (_, (bt, _)) as typed_b = type_expr tenv' body in
        let typed_tvar = add_type_to_ident tvar bt in
        pure @@ (TypedSyntax.TFun (typed_tvar, typed_b), (mk_qual_tp (PolyFun ((get_id tvar), bt.tp)), rep))
    | TApp (tf, arg_types) ->
        let%bind _ = mapM arg_types ~f:(TEnv.is_wf_type tenv) in
        let%bind tfres = TEnv.resolveT tenv (get_id tf)
            ~lopt:(Some (get_rep tf)) in
        let tf_rr = rr_typ tfres in
        let tftyp = tf_rr.tp in
        let%bind res_type = elab_tfun_with_args tftyp arg_types in
        let%bind _ = TEnv.is_wf_type tenv res_type in
        pure @@ (TypedSyntax.TApp (add_type_to_ident tf tf_rr, arg_types), (mk_qual_tp res_type, rep))
    | Message bs ->
        let%bind msg_typ = get_msgevnt_type bs in
        let payload_type fld pld =
          let check_field_type seen_type =
            match Caml.List.assoc_opt fld CU.msg_mandatory_field_types with
            | Some fld_t when fld_t <> seen_type ->
              fail1 (sprintf "Type mismatch for Message field %s. Expected %s but got %s"
                    fld (pp_typ fld_t) (pp_typ seen_type)) (ER.get_loc rep) 
            | _ -> pure ()
          in
          (match pld with
           | MLit l ->
               let%bind (_, (lt, _)) = type_expr tenv (Literal l, rep)  in
               let%bind _ = check_field_type lt.tp in
               pure @@ TypedSyntax.MLit l
           | MVar i ->
               let%bind r = TEnv.resolveT tenv (get_id i)
                   ~lopt:(Some (get_rep i)) in
               let t = rr_typ r in
               let rtp = t.tp in
               let%bind _ = check_field_type rtp in
               if is_serializable_type rtp
               then pure @@ TypedSyntax.MVar (add_type_to_ident i t)
               else fail1 (sprintf "Cannot serialize values of type %s." (pp_typ rtp))
                          (ER.get_loc (get_rep i)))
        in
        let%bind typed_bs =
          (* Make sure we resolve all the payload *)
          mapM bs ~f:(fun (s, pld) -> liftPair2 s @@ payload_type s pld)
        in
        pure @@ (TypedSyntax.Message typed_bs, (mk_qual_tp @@ msg_typ, rep))

  and app_type tenv ftyp actuals =
    (* Type-check function application *)  
    let%bind _ = TEnv.is_wf_type tenv ftyp in
    let%bind (targs, typed_actuals) = type_actuals tenv actuals in
    let%bind res_type = fun_type_applies ftyp targs in
    let%bind _ = TEnv.is_wf_type tenv res_type in
    pure @@ (typed_actuals, mk_qual_tp res_type)

  and type_check_match_branch tenv styp ptrn e =
    let%bind (new_p, new_typings) = assign_types_for_pattern styp ptrn in
    let tenv' = TEnv.addTs (TEnv.copy tenv) new_typings in
    let%bind typed_e = type_expr tenv' e in
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
      | None -> raise (mk_internal_error "Different number of actuals and Types of actuals")  in
    let typed_actuals = List.map actuals_with_types ~f:(fun (a, t) -> add_type_to_ident a t) in
    pure @@ (targs, typed_actuals)

  (**************************************************************)
  (*                   Typing statements                        *)
  (**************************************************************)

  (* Auxiliary structure for types of fields and BC components *)
  type stmt_tenv = {
    pure   : TEnv.t;
    fields : TEnv.t;
    procedures : (string * typ list) list;
  }

  (* Return typed map accesses and the accessed value's type. *)
  (* (m[k1][k2]... -> (typed_m, typed_k_list, type_of_accessed_value) *)
  let type_map_access env m' keys' =
    let%bind t' = TEnv.resolveT env.fields (get_id m') ~lopt:(Some (get_rep m'))  in
    let rec helper t keys =
      match t, keys with
      | MapType (kt, vt), k :: rest ->
        let%bind k_t = TEnv.resolveT env.pure (get_id k) ~lopt:(Some (get_rep k)) in
        let%bind _ = assert_type_equiv kt (rr_typ k_t).tp in
        let%bind (typed_keys, res) = helper vt rest in
        let typed_k = add_type_to_ident k (rr_typ k_t) in
        pure @@ (typed_k::typed_keys, res)
      (* If there are no more keys left, we have the result type. *)
      | _, [] -> pure @@ ([], t)
      | _ , k :: _ -> fail1 (sprintf "Type failure in map access. Cannot index into key %s" (get_id k))
                        (ER.get_loc (get_rep k))
    in
      let%bind (typed_keys, res) = helper (rr_typ t').tp keys' in
      let typed_m = add_type_to_ident m' (rr_typ t') in
      pure (typed_m, typed_keys, res)

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
             let%bind (next_env, ident_type) = wrap_type_serr stmt (
                 let%bind fr = TEnv.resolveT env.fields (get_id f) ~lopt:(Some (get_rep f)) in
                 let pure' = TEnv.addT (TEnv.copy env.pure) x (rr_typ fr).tp in
                 let next_env = {env with pure = pure'} in
                 pure @@ (next_env, rr_typ fr)
               ) in
             let%bind checked_stmts = type_stmts next_env sts get_loc in
             let typed_x = add_type_to_ident x ident_type in
             let typed_f = add_type_to_ident f ident_type in
             pure @@ add_stmt_to_stmts_env (TypedSyntax.Load (typed_x, typed_f), rep) checked_stmts
         | Store (f, r) ->
             if List.mem ~equal:(fun s1 s2 -> s1 = s2)
                 no_store_fields (get_id f) then
               wrap_type_serr stmt (
                 fail0 @@ sprintf
                   "Writing to the field `%s` is prohibited." (get_id f)) 
             else
               let%bind (checked_stmts, f_type, r_type) = wrap_type_serr stmt (
                   let%bind fr = TEnv.resolveT env.fields (get_id f) ~lopt:(Some (get_rep f)) in
                   let%bind r = TEnv.resolveT env.pure (get_id r) ~lopt:(Some (get_rep r)) in
                   let%bind _ = assert_type_equiv (rr_typ fr).tp (rr_typ r).tp in
                   let%bind checked_stmts = type_stmts env sts get_loc in
                   pure @@ (checked_stmts, rr_typ fr, rr_typ r)
                 ) in
               let typed_f = add_type_to_ident f f_type in
               let typed_r = add_type_to_ident r r_type in
               pure @@ add_stmt_to_stmts_env (TypedSyntax.Store (typed_f, typed_r), rep) checked_stmts
         | Bind (x, e) ->
             let%bind (_, (ityp, _)) as checked_e = wrap_type_serr stmt @@ type_expr env.pure e in
             let pure' = TEnv.addT (TEnv.copy env.pure) x ityp.tp in
             let env' = {env with pure = pure'} in
             let%bind checked_stmts = type_stmts env' sts get_loc in
             let typed_x = add_type_to_ident x ityp in
             pure @@ add_stmt_to_stmts_env (TypedSyntax.Bind (typed_x, checked_e), rep) checked_stmts
         | MapUpdate (m, klist, vopt) ->
             let%bind (typed_m, typed_klist, typed_v) = wrap_type_serr stmt (
                let%bind (typed_m, typed_klist, v_type) = type_map_access env m klist in
                let%bind typed_v = 
                  (match vopt with
                   | Some v -> (* This is adding/replacing the value for a key. *) 
                      let%bind v_resolv = TEnv.resolveT env.pure (get_id v) ~lopt:(Some (get_rep v)) in
                      let typed_v = rr_typ v_resolv in
                      let%bind _ = assert_type_equiv v_type typed_v.tp in
                      let typed_v' = add_type_to_ident v typed_v in
                      pure @@ (Some typed_v')
                   | None -> pure None (* This is deleting a key from the map. *)
                  )
                in
                pure @@ (typed_m, typed_klist, typed_v)
             ) in
             (* Check rest of the statements. *)
             let%bind checked_stmts = type_stmts env sts get_loc in
             (* Update annotations. *)
             pure @@ add_stmt_to_stmts_env (TypedSyntax.MapUpdate(typed_m, typed_klist, typed_v), rep) checked_stmts
         | MapGet (v, m, klist, valfetch) ->
             let%bind (typed_m, typed_klist, v_type) = wrap_type_serr stmt (
                let%bind (typed_m, typed_klist, v_type) = type_map_access env m klist in
                pure @@ (typed_m, typed_klist, v_type)
             ) in
             (* The return type of MapGet would be (Option v_type) or Bool. *)
             let v_type' = if valfetch then ADT("Option", [v_type]) else ADT("Bool", []) in
             (* Update environment. *)
             let pure' = TEnv.addT (TEnv.copy env.pure) v v_type' in
             let env' = {env with pure = pure'} in
             let typed_v = add_type_to_ident v (mk_qual_tp v_type') in
             (* Check rest of the statements. *)
             let%bind checked_stmts = type_stmts env' sts get_loc in
             (* Update annotations. *)
             pure @@ add_stmt_to_stmts_env (TypedSyntax.MapGet(typed_v, typed_m, typed_klist, valfetch), rep) checked_stmts
         | ReadFromBC (x, bf) ->
             let%bind bt = wrap_type_serr stmt @@ lookup_bc_type bf in
             let pure' = TEnv.addT (TEnv.copy env.pure) x bt in
             let env' = {env with pure = pure'} in
             let%bind checked_stmts = type_stmts env' sts get_loc in
             let typed_x = add_type_to_ident x (mk_qual_tp bt) in
             pure @@ add_stmt_to_stmts_env (TypedSyntax.ReadFromBC (typed_x, bf), rep) checked_stmts
         | MatchStmt (x, clauses) ->
             if List.is_empty clauses
             then wrap_type_serr stmt @@ fail0 @@ sprintf
                 "List of pattern matching clauses is empty:\n%s" (pp_stmt s)
             else
               let%bind sctyp = TEnv.resolveT env.pure (get_id x)
                   ~lopt:(Some (get_rep x)) in
               let sctype = rr_typ sctyp in
               let sct = sctype.tp in
               let msg = sprintf "Error in pattern matching \"%s\" of type %s" (get_id x) (pp_typ sct) in
               let sloc = ER.get_loc (get_rep x) in
               let typed_x = add_type_to_ident x sctype in
               let%bind checked_clauses = wrap_with_info (msg, sloc) @@
                 mapM clauses ~f:(fun (ptrn, ex) ->
                     type_match_stmt_branch env sct ptrn ex get_loc ) in
               let%bind checked_stmts = type_stmts env sts get_loc in
               pure @@ add_stmt_to_stmts_env (TypedSyntax.MatchStmt (typed_x, checked_clauses), rep) checked_stmts
         | AcceptPayment ->
             let%bind checked_stmts = type_stmts env sts get_loc in
             pure @@ add_stmt_to_stmts_env (TypedSyntax.AcceptPayment, rep) checked_stmts
         | SendMsgs i ->
             let%bind r = TEnv.resolveT env.pure (get_id i)
                 ~lopt:(Some (get_rep i)) in
             let i_type = rr_typ r in
             let expected = list_typ msg_typ in
             let%bind _ = wrap_type_serr stmt @@
               assert_type_equiv expected i_type.tp in
             let typed_i = add_type_to_ident i i_type in
             let%bind checked_stmts = type_stmts env sts get_loc in
             pure @@ add_stmt_to_stmts_env (TypedSyntax.SendMsgs typed_i, rep) checked_stmts
         | CreateEvnt i ->
            (* Same as SendMsgs except that this takes a single message instead of a list. *)
             let%bind r = TEnv.resolveT env.pure (get_id i)
                 ~lopt:(Some (get_rep i)) in
             let i_type = rr_typ r in
             let%bind _ = wrap_type_serr stmt @@
               assert_type_equiv event_typ i_type.tp in
             let typed_i = add_type_to_ident i i_type in
             let%bind checked_stmts = type_stmts env sts get_loc in
             pure @@ add_stmt_to_stmts_env (TypedSyntax.CreateEvnt typed_i, rep) checked_stmts
         | CallProc (p, args) ->
             let msg = sprintf "Type error(s) in procedure call %s:\n" (get_id p) in
             let%bind typed_args =
               wrap_with_info (msg, SR.get_loc (get_rep p)) @@
               let%bind (targs, typed_actuals) = type_actuals env.pure args in
               match List.Assoc.find env.procedures ~equal:(=) (get_id p) with
               | Some arg_typs ->
                   let%bind _ = proc_type_applies arg_typs targs in
                   pure @@ typed_actuals
               | None -> 
                   fail0 @@ sprintf
                     "Procedure %s not found." (get_id p) in
             let%bind checked_stmts = type_stmts env sts get_loc in
             pure @@ add_stmt_to_stmts_env (TypedSyntax.CallProc (p, typed_args), rep) checked_stmts
         | Throw iopt ->
             let%bind checked_stmts = type_stmts env sts get_loc in
            (match iopt with
            | Some i ->
              (* Same as CreateEvent. *)
              let%bind r = TEnv.resolveT env.pure (get_id i)
                  ~lopt:(Some (get_rep i)) in
              let i_type = rr_typ r in
              let%bind _ = wrap_type_serr stmt @@
                assert_type_equiv exception_typ i_type.tp in
              let typed_i = add_type_to_ident i i_type in
              pure @@ add_stmt_to_stmts_env (TypedSyntax.Throw (Some typed_i), rep) checked_stmts
            | None ->
              pure @@ add_stmt_to_stmts_env (TypedSyntax.Throw None, rep) checked_stmts
            )
        )
        
  and type_match_stmt_branch env styp ptrn sts get_loc =
    let%bind (new_p, new_typings) = assign_types_for_pattern styp ptrn in
    let pure' = TEnv.addTs (TEnv.copy env.pure) new_typings in
    let env' = {env with pure = pure'} in
    let%bind (new_stmts, _) = type_stmts env' sts get_loc in
    pure @@ (new_p, new_stmts)

  let type_component env0 tr : ((TypedSyntax.component * (string * typ list) list), scilla_error list) result  =
    let {comp_type; comp_name; comp_params; comp_body} = tr in
    let tenv0 = env0.pure in
    let procedures = env0.procedures in
    let component_type_string = component_type_to_string comp_type in
    let msg = sprintf "Type error(s) in %s %s:\n" component_type_string (get_id comp_name) in
    wrap_with_info (msg, SR.get_loc (get_rep comp_name)) @@
    let param_checker =
      match comp_type with
      | CompTrans -> is_serializable_type
      | CompProc -> is_non_map_ground_type in
    let%bind typed_cparams =
      mapM ~f:(fun (param, t) ->
          if param_checker t
          then pure (add_type_to_ident param (mk_qual_tp t), t)
          else fail1 (sprintf "Type %s cannot be used as %s parameter" (pp_typ t) component_type_string) (ER.get_loc (get_rep param))) comp_params in

    let append_params = CU.append_implict_comp_params comp_params in
    let tenv1 = TEnv.addTs tenv0 append_params in
    let env = {env0 with pure = tenv1} in
    let%bind (typed_stmts, _) = type_stmts env comp_body ER.get_loc in
    let new_proc_signatures =
      match comp_type with
      | CompTrans -> procedures
      | CompProc ->
          let proc_sig = List.map comp_params ~f:snd in
          List.Assoc.add procedures ~equal:(=) (get_id comp_name) proc_sig in
    pure @@ ({ TypedSyntax.comp_type = comp_type ;
               TypedSyntax.comp_name = comp_name ;
               TypedSyntax.comp_params = typed_cparams;
               TypedSyntax.comp_body = typed_stmts },
             new_proc_signatures)


  (*****************************************************************)
  (*                 Typing entire contracts                       *)
  (*****************************************************************)
  let type_fields tenv flds =
    let%bind (typed_flds, new_env) = foldM flds ~init:([], TEnv.mk)
        ~f:(fun (acc, fenv) (fn, ft, fe) ->
            let msg = sprintf
                "Type error in field %s:\n" (get_id fn) in
            wrap_with_info (msg, ER.get_loc (get_rep fn)) @@
            let%bind (_, (ar, _)) as typed_expr = type_expr tenv fe in
            let actual = ar.tp in
            let%bind _ = assert_type_equiv ft actual in
            let typed_fs = add_type_to_ident fn ar in
            if is_storable_type ft then
              pure @@ ((typed_fs, ft, typed_expr) :: acc,
                       TEnv.addT (TEnv.copy fenv) fn actual)
            else fail0 @@ sprintf "Values of the type \"%s\" cannot be stored." (pp_typ ft)) in
        pure @@ (List.rev typed_flds, new_env)

  (**************************************************************)
  (*                    Typing libraries                        *)
  (**************************************************************)
      
  let type_rec_libs rec_libs =
    let (lib_vars, lib_types) =
      List.partition_map rec_libs
        ~f:(fun le -> match le with
            | LibVar (n, t, e) -> `Fst (n, t, e)
            | LibTyp (n, ts) ->`Snd (n, ts)) in
    (* recursion primitives must not contain type declarations *)
    let%bind _ =
      match lib_types with
      | _ :: _ -> fail0 @@ "Type declarations not allowed in recursion primitives"
      | [] -> pure () in
    let env0 = TEnv.copy TEnv.mk in
    foldM lib_vars ~init:([], env0)
      ~f:(fun (entry_acc, env_acc) (rn, topt, body) ->
          wrap_with_info
            (sprintf "Type error when checking recursion primitive %s:\n"
               (get_id rn), dummy_loc) @@
          let%bind ((_, (ar, _)) as typed_body) = type_expr env0 body in
          let%bind _ = match topt with Some tannot -> assert_type_equiv tannot ar.tp | None -> pure () in
          let typed_rn = add_type_to_ident rn ar in
          let new_entries = (TypedSyntax.LibVar (typed_rn, topt, typed_body)) :: entry_acc in
          let new_env = TEnv.addT (TEnv.copy env_acc) rn ar.tp in
          pure @@ (new_entries, new_env))

  (* Check that ADT constructors are well-formed.
     Declared ADTs and constructors are added to stored datatypes 
     by ADTChecker.
     Checking for ADT types in scope and multiple usages of the 
     same constructor name takes place in ADTChecker. *)
  let type_lib_typ_ctrs env (ctr_defs : ctr_def list) =
    forallM
      ~f:(fun ctr_def ->
          forallM
            ~f:(fun c_arg_type ->
                TEnv.is_wf_type env c_arg_type)
            ctr_def.c_arg_types )
      ctr_defs

  let type_library env0 { lname ; lentries = ents } =
    let msg = sprintf
        "Type error in library %s:\n\n" (get_id lname) in
    wrap_with_info (msg, SR.get_loc (get_rep lname)) @@
    let%bind (typed_entries, new_tenv, errs, _) =
      foldM ~init:([], env0, [], []) ents
        ~f:(fun (acc, env, errs, blist) lib_entry ->
            match lib_entry with
            | LibTyp (tname, ctr_defs) ->
                let msg = sprintf
                    "Type error in library type %s:\n\n" (get_id tname) in
                wrap_with_info (msg, ER.get_loc (get_rep tname)) @@
                let%bind _ = type_lib_typ_ctrs env ctr_defs in
                pure @@ (acc, env, errs, blist)
            | LibVar (ln, ltopt, le) ->
                let msg = sprintf
                    "Type error in library variable %s:\n\n" (get_id ln) in
                let dep_on_blist = free_vars_dep_check le blist in
                (* If exp depends on a blacklisted exp, then let's ignore it. *)
                if dep_on_blist then pure @@ (acc, env, errs, ln :: blist) else
                  let res = wrap_with_info (msg, SR.get_loc (get_rep lname)) (type_expr env le) in
                  match res with
                  | Error e ->
                      (* A new original failure. Add to blocklist and move on. *)
                      pure @@ (acc, env, errs @ e, ln :: blist)
                  | Ok res' ->
                      (* This went good. *)
                      let (_, (tr, _)) as typed_e = res' in
                      let%bind _ = match ltopt with Some tannot -> assert_type_equiv tannot tr.tp | None -> pure () in
                      let typed_ln = add_type_to_ident ln tr in
                      pure @@ (TypedSyntax.LibVar (typed_ln, ltopt, typed_e) :: acc,
                               TEnv.addT (TEnv.copy env) ln tr.tp, errs, blist))
    in
    (* If there has been no errors at all, we're good to go. *)
    if errs = [] then
        pure @@ ( { TypedSyntax.lname = lname ;
                TypedSyntax.lentries = List.rev typed_entries }, TEnv.copy new_tenv)
    (* Else report all errors together. *)
    else fail @@ errs

  (* TODO, issue #179: Re-introduce this when library cache can store typed ASTs
  (* type library, handling cache as necessary. *)
  let type_library_cache (tenv : TEnv.t) (elib : UntypedSyntax.library)  =
    (* We are caching TypeEnv = MakeTEnv(PlainTypes)(ER) *)
    let module STC = TypeCache.StdlibTypeCacher(MakeTEnv)(PlainTypes) (STR) (ER) in
    let open STC in
    (* Check if we have the type info in cache. *)
    match get_lib_tenv_cache tenv elib with
    | Some tenv' ->
        (* Use cached entries. *)
    pure (tenv', "")
    | None ->
        (* Couldn't find in cache. Actually type the library. *)
        let res = type_library tenv elib in
        (match res with
    | Error (msg, es) -> Ok((tenv, msg), es)
    | Ok ((_, tenv'), es) as lib_res -> 
             (* Since we don't have this in cache, cache it now. *)
             cache_lib_tenv tenv' elib;
        Ok((lib_res, ""), es)
        )
  *)

  (* Type a list of libtrees, with tenv0 as the base environment. *)
  let type_libraries elibs tenv0 =
    let ((typed_elibs, elibs_env), emsgs) = 

      let rec recurser libl =

        (* Do a preliminary check to ensure no name conflicts b/w
         * libraries in elibs at just the root levels. *)
        let err_dups =
          (* check if any entry in "lib" is in "rest". *)
          let check_dup lib rest (err_acc : scilla_error list) =
            List.fold lib.lentries ~init:err_acc ~f:(fun err_acc entry ->
              (* Check if entry is in rest. *)
              let ename = match entry with | LibTyp (i, _) | LibVar (i, _, _) -> i in
              List.fold rest ~init:err_acc ~f:(fun err_acc lib' ->
                List.fold lib'.lentries ~init:err_acc ~f:(fun err_acc entry' ->
                  let ename' = match entry' with | LibTyp (i, _) | LibVar (i, _, _) -> i in
                  if get_id ename = get_id ename'
                  then
                    err_acc @ (mk_error1 (sprintf "Entry %s in library %s conflicts with entry in library %s" 
                      (get_id ename) (get_id lib'.lname) (get_id lib.lname)) (ER.get_loc (get_rep ename')))
                  else
                    err_acc
                )
              )
            )
          in
          let rec checker libs err_acc =
            match libs with
            | [] | [_] -> err_acc
            | lib :: rest ->
              let err_acc' = check_dup lib rest err_acc in
              checker rest err_acc'
          in
          let libl' = List.map libl ~f:(fun l -> l.libn) in
          checker libl' []
        in

        (* Do the actual typing. *)
        List.fold libl ~init:(([], tenv0), err_dups)
        ~f:(fun ((lib_acc, tenv_acc), emsgs_acc) elib ->
            (* TODO, issue #179: Re-introduce this when library cache can store typed ASTs
            let%bind (tenv', emsg) = type_library_cache tenv_acc elib in *)
            let ((dep_libs, dep_env), dep_emsgs) = recurser elib.deps in
            let ((typed_libraries, tenv'), emsg) =
              match type_library dep_env elib.libn with
              | Ok (t_lib, t_env) ->
                let (elib' : TypedSyntax.libtree) = { libn = t_lib; deps = dep_libs } in
                (* from t_env, retain only entries from t_lib and tenv0 *)
                let env' = TEnv.filterTs (TEnv.copy t_env) ~f:(fun name ->
                  List.exists t_lib.lentries ~f:(function | LibTyp _ -> false | LibVar (i, _, _) -> get_id i = name)
                  || TEnv.existsT tenv0 name
                ) in
                ((lib_acc @ [elib'], TEnv.append (TEnv.copy tenv_acc) env'), emsgs_acc @ dep_emsgs)
              | Error el ->
                ((lib_acc, tenv_acc), emsgs_acc @ dep_emsgs @ el)
            in
            (* Updated env and error messages are what we accummulate in the fold. *)
            ((typed_libraries, tenv'), emsg)
          )
      in
      recurser elibs
    in
    if emsgs <> [] then fail emsgs else pure (typed_elibs, elibs_env)

  let type_lmodule
    (md : UntypedSyntax.lmodule)
    (rec_libs : UntypedSyntax.lib_entry list)
    (elibs : UntypedSyntax.libtree list)
    : (TypedSyntax.lmodule * TypedSyntax.lib_entry list * TypedSyntax.libtree list, scilla_error list) result =

    let msg = sprintf "Type error(s) in contract %s:\n" (get_id md.libs.lname) in
    wrap_with_info (msg, SR.get_loc (get_rep md.libs.lname)) @@

    (* Step 0: Type check recursion principles *)
    let%bind (typed_rlib, tenv0) = type_rec_libs rec_libs in

    (* Step 1: Type check external libraries. *)
    let%bind (typed_elibs, elibs_env) = type_libraries elibs tenv0 in

    (* Type the library of this module. *)
    let%bind (typed_mlib, _) = type_library elibs_env md.libs in

    let typed_lmodule = { TypedSyntax.elibs = md.elibs; TypedSyntax.libs = typed_mlib } in
    pure (typed_lmodule, typed_rlib, typed_elibs)

  let type_module
      (md : UntypedSyntax.cmodule)
      (* TODO, issue #225 : rec_libs should be added to the libraries when we allow custom, inductive ADTs *)
      (rec_libs : UntypedSyntax.lib_entry list)
      (elibs : UntypedSyntax.libtree list)
    : (TypedSyntax.cmodule * stmt_tenv * TypedSyntax.libtree list * TypedSyntax.lib_entry list, scilla_error list) result =

    let {smver = mod_smver;cname = mod_cname; libs; elibs = mod_elibs; contr} = md in
    let {cname = ctr_cname; cparams; cfields; ccomps} = contr in
    let msg = sprintf "Type error(s) in contract %s:\n" (get_id ctr_cname) in
    wrap_with_info (msg, SR.get_loc (get_rep ctr_cname)) @@

    (* Step 0: Type check recursion principles *)
    let%bind (typed_rlib, tenv0) = type_rec_libs rec_libs in
    
    (* Step 1: Type check external libraries *)
    let ((typed_elibs, elibs_env), emsgs) = 
      match type_libraries elibs tenv0 with
      | Ok (_ as te) -> (te, [])
      | Error e -> (([], tenv0), e)
    in
    (* Step 2: Type check contract library, if defined. *)
    let ((typed_clibs, tenv), emsgs) = 
      match libs with
      | Some lib ->
        (match type_library elibs_env lib with
        | Ok (lib', env') -> ((Some lib', env'), emsgs)
        | Error e -> ((None, elibs_env), emsgs @ e))
      | None ->  ((None, elibs_env), emsgs)
    in

    (* Step 3: Adding typed contract parameters (incl. implicit ones) *)
    let params = CU.append_implict_contract_params cparams in
    let tenv3 = TEnv.addTs tenv params in

    (* Step 4: Type-check fields and add balance *)
    let%bind (typed_fields, fenv0), femsgs0 = 
      match type_fields tenv3 cfields with
      | Error el -> Ok (([], tenv3), emsgs @ el)
      | Ok (typed_fields, tenv) -> Ok ((typed_fields, tenv), emsgs)
    in
    let (bn, bt) = CU.balance_field in
    let fenv = TEnv.addT fenv0 bn bt in

    (* Step 5: Form a general environment for checking components *)
    let env = {pure= tenv3; fields= fenv; procedures = []} in

    (* Step 6: Type-checking all components in batch *)
    let%bind ((t_comps, _), emsgs') = foldM ~init:(([], []), femsgs0) ccomps
        ~f:(fun ((comp_acc, proc_acc), emsgs) tr -> 
            let toplevel_env = {pure = TEnv.copy env.pure; fields = TEnv.copy fenv; procedures = proc_acc} in
            match type_component toplevel_env tr with
            | Error el -> Ok ((comp_acc, proc_acc), emsgs @ el)
            | Ok (typed_comp, proc_sigs) -> Ok ((typed_comp :: comp_acc, proc_sigs), emsgs)
          ) in
    let typed_comps = List.rev t_comps in

    (* Step 7: Lift contract parameters to ETR.rep ident *)
    let typed_params = List.map cparams
        ~f:(fun (id, t) -> (add_type_to_ident id (mk_qual_tp t), t)) in

    if emsgs' = []
    (* Return pure environment *)  
    then pure ({TypedSyntax.smver = mod_smver;
                TypedSyntax.cname = mod_cname;
                TypedSyntax.libs = typed_clibs;
                TypedSyntax.elibs = mod_elibs;
                TypedSyntax.contr =
                  {TypedSyntax.cname = ctr_cname;
                   TypedSyntax.cparams = typed_params;
                   TypedSyntax.cfields = typed_fields;
                   TypedSyntax.ccomps = typed_comps}}, env, typed_elibs, typed_rlib)
    (* Return error messages *)
    else fail @@ emsgs'


  (**************************************************************)
  (*                    Staging API                             *)
  (**************************************************************)

  module OutputSyntax = TypedSyntax
  module OutputSRep = STR
  module OutputERep = ETR

end
