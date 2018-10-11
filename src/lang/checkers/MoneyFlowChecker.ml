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
*)
(*
open Syntax
open TypeUtil


module MoneyFlowRep (R : Rep) = struct
  type MoneyTag =
    | Plain
    | Money
    | Map of MoneyTag
    | Top
    (* TODO: Add this if possible *)
  (*    | ADT of t *)
  [@@deriving sexp]
      
  type rep = MoneyTag * R.rep

  let get_loc r = match r with | (_, rr) -> R.get_loc rr

  let mk_id s =
    match s with
    | Ident (n, r) -> Ident (n, (Plain, r))

  let mk_id_address s = mk_id (R.mk_id_address s)
  let mk_id_uint128 s = mk_id (R.mk_id_uint128 s)
  let mk_id_bnum    s = mk_id (R.mk_id_bnum s)
  let mk_id_string  s = mk_id (R.mk_id_string s)
  
  let parse_rep s = (Plain, R.parse_rep s)
  let get_rep_str r = match r with | (_, rr) -> R.get_rep_str rr

  let get_type (r : rep) = fst r
end


module ScillaMoneyFlowChecker
    (SR : Rep)
    (ER : sig
       include Rep
       val get_type : rep -> PlainTypes.t inferred_type
     end) = struct

  module SMFR = SR
  module EMFR = ER
  module TypedSyntax = ScillaSyntax (SR) (ER)
  module MFSyntax = ScillaSyntax (SMFR) (EMFR)
  (*   module TU = TypeUtilities (SR) (ER) *)
  (*   module SCU = ContractUtil.ScillaContractUtil (SR) (ER) *)

  open MFSyntax
  (*   open SCU *)

  let rec mf_tag_expr (erep : TypedSyntax.expr_annot) =
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
    | Builtin (i, actuals) ->
        wrap_type_err erep @@ 
        let%bind (targs, typed_actuals) = type_actuals tenv actuals in
        let%bind (_, ret_typ, _) = BuiltInDictionary.find_builtin_op i targs in
        let%bind _ = TEnv.is_wf_type tenv ret_typ in
        let q_ret_typ = mk_qual_tp ret_typ in
        pure @@ (TypedSyntax.Builtin (add_type_to_ident i q_ret_typ, typed_actuals), (q_ret_typ, rep))
    | Let (i, topt, lhs, rhs) ->
        (* Poor man's error reporting *)
        let%bind (_, (ityp, _)) as checked_lhs = wrap_type_err erep @@ type_expr tenv lhs in
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
        let open PrimTypes in 
        let payload_type pld =
          (match pld with
           | MTag m -> pure @@ TypedSyntax.MTag m
           | MLit l ->
               let%bind _ = type_expr tenv (Literal l, rep) 
               in pure @@ TypedSyntax.MLit l
           | MVar i ->
               let%bind r = TEnv.resolveT tenv (get_id i)
                   ~lopt:(Some (get_rep i)) in
               let t = rr_typ r in
               let rtp = t.tp in
               if is_storable_type rtp
               then pure @@ TypedSyntax.MVar (add_type_to_ident i t)
               else fail1 (sprintf "Cannot send values of type %s." (pp_typ rtp))
                          (ER.get_loc (get_rep i)))
        in
        let%bind typed_bs =
          (* Make sure we resolve all the payload *)
          mapM bs ~f:(fun (s, pld) -> liftPair2 s @@ payload_type pld)
        in
        pure @@ (TypedSyntax.Message typed_bs, (mk_qual_tp @@ msg_typ, rep))

  
end
*)
