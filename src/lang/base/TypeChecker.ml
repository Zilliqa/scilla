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
        let%bind arg_types = contr_pattern_arg_types atyp cn in
        let plen = List.length arg_types in
        let alen = List.length ps in
        let%bind _ = validate_param_length cn plen alen in
        let tps_pts = List.zip_exn arg_types ps in
        foldM ~init:tlist tps_pts
          ~f:(fun acc (t, pt) -> go t acc pt)
  in go sctyp [] pattern

(**************************************************************)

(* TODO: Check if the type is well-formed: support type variables *)
let rec get_type e tenv = match e with
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
      let%bind bt = get_type body tenv' in
      pure @@ mk_qual_tp (FunType (t, bt.tp))
  | App (f, actuals) ->
      let%bind fres = wrap_err e @@
        TEnv.resolveT tenv (get_id f) ~lopt:(Some (get_loc f)) in
      let ftyp = (rr_typ fres).tp in
      wrap_err e @@ app_type tenv ftyp actuals
  | Builtin (i, actuals) ->
      let%bind tresults = mapM actuals
          ~f:(fun arg -> TEnv.resolveT tenv (get_id arg)
                 ~lopt:(Some (get_loc arg))) in
      let targs = List.map tresults ~f:(fun rr -> (rr_typ rr).tp) in
      let%bind (_, ret_typ, _) = BuiltInDictionary.find_builtin_op i targs in
      let%bind _ = TEnv.is_wf_type tenv ret_typ in
      pure @@ mk_qual_tp ret_typ
  | Let (i, t, lhs, rhs) ->
      (* Poor man's error reporting *)
      let%bind ityp = wrap_err e @@ get_type lhs tenv in
      let tenv' = TEnv.addT (TEnv.copy tenv) i ityp.tp in
      get_type rhs tenv'
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
        let msg =
          sprintf "[%s]: error in typing pattern matching on `%s` of type %s (or one of its branches):\n"
        (get_loc_str (get_loc x)) (get_id x) (pp_typ sct) in
        wrap_with_info msg (
          let%bind cl_types = mapM clauses ~f:(fun (ptrn, ex) ->
              type_check_match_branch tenv sct ptrn ex) in
          let%bind _ =
            assert_all_same_type (List.map ~f:(fun it -> it.tp) cl_types) in
          (* Return the first type since all they are the same *)
          pure @@ List.hd_exn cl_types
        )
  | Fixpoint (f, t, body) ->
      pure @@ mk_qual_tp t
  | TFun (tvar, body) ->
      let tenv' = TEnv.addV (TEnv.copy tenv) tvar in
      let%bind bt = get_type body tenv' in
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
        | MLit l -> get_type (Literal l) tenv
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
  get_type e tenv'

