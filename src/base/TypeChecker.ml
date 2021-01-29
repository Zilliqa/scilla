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

open Core_kernel
open Literal
open Syntax
open ErrorUtils
open MonadUtil
open TypeUtil
open Datatypes
open BuiltIns
open ContractUtil
module TCLiteral = GlobalLiteral
module TCType = TCLiteral.LType
module TCIdentifier = TCType.TIdentifier
open TCIdentifier
open TCType

(*******************************************************)
(*                   Annotations                       *)
(*******************************************************)

module TypecheckerERep (R : Rep) = struct
  type rep = PlainTypes.t inferred_type * R.rep [@@deriving sexp]

  let dummy_rep = (PlainTypes.mk_qualified_type Unit, R.dummy_rep)

  let get_loc r = match r with _, rr -> R.get_loc rr

  let mk_rep (r : R.rep) (t : PlainTypes.t inferred_type) = (t, r)

  let address_rep =
    mk_rep R.address_rep
      (PlainTypes.mk_qualified_type (bystrx_typ Type.address_length))

  let uint128_rep =
    mk_rep R.uint128_rep (PlainTypes.mk_qualified_type uint128_typ)

  let uint32_rep =
    mk_rep R.uint128_rep (PlainTypes.mk_qualified_type uint32_typ)

  let bnum_rep = mk_rep R.bnum_rep (PlainTypes.mk_qualified_type bnum_typ)

  let string_rep = mk_rep R.string_rep (PlainTypes.mk_qualified_type string_typ)

  let parse_rep s = (PlainTypes.mk_qualified_type uint128_typ, R.parse_rep s)

  let get_rep_str r = match r with _, rr -> R.get_rep_str rr

  let get_type (r : rep) = fst r
end

(*****************************************************************)
(*                 Typing entire contracts                       *)
(*****************************************************************)

module ScillaTypechecker (SR : Rep) (ER : Rep) = struct
  module STR = SR
  module ETR = TypecheckerERep (ER)
  module TCLiteral = TCLiteral
  module TCType = TCLiteral.LType
  module TCIdentifier = TCType.TIdentifier
  module TCName = TCIdentifier.Name
  module UntypedSyntax = ScillaSyntax (SR) (ER) (TCLiteral)
  module TypedSyntax = ScillaSyntax (STR) (ETR) (TCLiteral)
  include TypedSyntax
  include ETR
  module TU = TypeUtilities
  module TBuiltins = ScillaBuiltIns (SR) (ER)
  module TypeEnv = TU.MakeTEnv (PlainTypes) (ER)
  module CU = ScillaContractUtil (SR) (ER)
  open TU
  open TBuiltins
  open TypeEnv
  open UntypedSyntax
  open EvalMonad
  open EvalMonad.Let_syntax

  let fromR errorType r =
    match r with Error s -> fail (errorType, s) | Core_kernel.Ok a -> pure a

  let fromR_TE r = fromR TypeError r

  let init_gas_kont r gas' =
    match r with Ok z -> Ok (z, gas') | Error msg -> Error (msg, gas')

  let strip_error_type res =
    match res with Ok (r, g) -> Ok (r, g) | Error ((_, e), g) -> Error (e, g)

  (*****************************************************************)
  (*               Blockchain component typing                     *)
  (*****************************************************************)

  let bc_types = [ (TypeUtil.blocknum_name, bnum_typ) ]

  let lookup_bc_type x =
    match List.Assoc.find bc_types x ~equal:String.( = ) with
    | Some t -> pure t
    | None -> fail (mk_type_error0 (sprintf "Unknown blockchain field %s." x))

  (**************************************************************)
  (*             Auxiliary functions for typing                 *)
  (**************************************************************)

  (* Lift 'rep ident to (inferred_type * 'rep) ident *)
  let add_type_to_ident i typ = mk_id (get_id i) (ETR.mk_rep (get_rep i) typ)

  (* Given a scrutinee type and a pattern,
     produce a list of ident -> type mappings for
     all variables bound by the pattern *)
  let assign_types_for_pattern sctyp pattern =
    let rec go atyp tlist p =
      match p with
      | Wildcard -> pure (TypedSyntax.Wildcard, tlist)
      | Binder x ->
          pure
          @@ ( TypedSyntax.Binder (add_type_to_ident x (mk_qual_tp atyp)),
               (x, atyp) :: tlist )
      | Constructor (cn, ps) ->
          let%bind arg_types =
            fromR_TE
            @@ constr_pattern_arg_types atyp (get_id cn)
                 ~lc:(SR.get_loc (get_rep cn))
          in
          let plen = List.length arg_types in
          let alen = List.length ps in
          let%bind () =
            fromR_TE
            @@ validate_param_length
                 ~lc:(SR.get_loc (get_rep cn))
                 (get_id cn) plen alen
          in
          let tps_pts = List.zip_exn arg_types ps in
          let%bind typed_ps, tps =
            foldrM ~init:([], tlist) tps_pts ~f:(fun (ps, ts) (t, pt) ->
                let%bind p, tss = go t ts pt in
                pure @@ (p :: ps, tss))
          in
          pure @@ (TypedSyntax.Constructor (cn, typed_ps), tps)
    in
    go sctyp [] pattern

  (* tm[tvar := tp]
     Parallel implementation to the one in Syntax.ml to allow gas accounting.
  *)
  let subst_type_in_type_with_gas tvar tp tm =
    (* Count the number of AST nodes in a type *)
    let rec type_size t =
      match t with
      | PrimType _ | Unit | TypeVar _ -> 1
      | PolyFun (_, t) -> 1 + type_size t
      | MapType (t1, t2) | FunType (t1, t2) -> 1 + type_size t1 + type_size t2
      | ADT (_, ts) ->
          List.fold_left ts ~init:1 ~f:(fun acc t -> acc + type_size t)
      | Address fts ->
          List.fold_left fts ~init:0 ~f:(fun acc (_, t) -> acc + type_size t)
    in

    let subst_type_cost tvar tm tp_size =
      let rec cost tm =
        match tm with
        | PrimType _ | Unit
        | MapType (_, _)
        | FunType (_, _)
        | ADT (_, _)
        | PolyFun (_, _) ->
            1
        | TypeVar n -> if String.(n = tvar) then tp_size else 1
        | Address fts ->
            max 1
              (List.fold_left fts ~init:0 ~f:(fun acc (_, t) -> acc + cost t))
      in
      cost tm
    in

    let tp_size = type_size tp in
    let rec recurser t =
      let gas_cost = Stdint.Uint64.of_int @@ subst_type_cost tvar t tp_size in
      let thunk () =
        match t with
        | PrimType _ | Unit -> pure t
        (* Make sure the map's type is still primitive! *)
        | MapType (kt, vt) ->
            let%bind kts = recurser kt in
            let%bind vts = recurser vt in
            pure (MapType (kts, vts))
        | FunType (at, rt) ->
            let%bind ats = recurser at in
            let%bind rts = recurser rt in
            pure (FunType (ats, rts))
        | TypeVar n ->
            let res = if String.(tvar = n) then tp else t in
            pure res
        | ADT (s, ts) ->
            let%bind ts_res = mapM ts ~f:recurser in
            pure (ADT (s, ts_res))
        | PolyFun (arg, t') ->
            if String.(tvar = arg) then pure t'
            else
              let%bind res = recurser t' in
              pure (PolyFun (arg, res))
        | Address fts ->
            let%bind fts_res =
              mapM fts ~f:(fun (x, t') ->
                  let%bind t'_res = recurser t' in
                  pure (x, t'_res))
            in
            pure (Address fts_res)
      in
      checkwrap_op thunk gas_cost (GasError, out_of_gas_err)
    in
    recurser tm

  let rec elab_tfun_with_args ~lc tf args =
    match (tf, args) with
    | (PolyFun _ as pf), a :: args' ->
        let afv = free_tvars a in
        let%bind n, tp =
          match refresh_tfun pf afv with
          | PolyFun (a, b) -> pure (a, b)
          | _ -> fail @@ mk_type_error1 "This can't happen!" lc
        in
        let%bind tp' = subst_type_in_type_with_gas n a tp in
        elab_tfun_with_args ~lc tp' args'
    | t, [] -> pure t
    | _ ->
        let msg =
          sprintf
            "Cannot elaborate expression of type\n\
             %s\n\
             applied, as a type function, to type arguments\n\
             %s."
            (pp_typ_error tf) (pp_typ_list_error args)
        in
        fail @@ mk_type_error1 msg lc

  (**************************************************************)
  (*               Typing explict gas charges                   *)
  (**************************************************************)

  (* No actual typechecking required - we just need to translate
     gas_charge constructors to the new syntax *)

  let rec type_gas_charge gc =
    let open UntypedSyntax.SGasCharge in
    match gc with
    | StaticCost i -> TypedSyntax.SGasCharge.StaticCost i
    | SizeOf v -> TypedSyntax.SGasCharge.SizeOf v
    | ValueOf v -> TypedSyntax.SGasCharge.ValueOf v
    | LengthOf v -> TypedSyntax.SGasCharge.LengthOf v
    | MapSortCost m -> TypedSyntax.SGasCharge.MapSortCost m
    | SumOf (g1, g2) ->
        TypedSyntax.SGasCharge.SumOf (type_gas_charge g1, type_gas_charge g2)
    | ProdOf (g1, g2) ->
        TypedSyntax.SGasCharge.ProdOf (type_gas_charge g1, type_gas_charge g2)
    | MinOf (g1, g2) ->
        TypedSyntax.SGasCharge.MinOf (type_gas_charge g1, type_gas_charge g2)
    | DivCeil (g1, g2) ->
        TypedSyntax.SGasCharge.DivCeil (type_gas_charge g1, type_gas_charge g2)
    | LogOf v -> TypedSyntax.SGasCharge.LogOf v

  (**************************************************************)
  (*                   Typing expressions                       *)
  (**************************************************************)

  (* 1. Gets the current TEnv from cur_env by calling get_tenv on it.
   * 2. Updates it with new_binds
   * 3. Calls typer with the updated env.
   * 4. Restores the environment 
   * 5. Returns typer's result. *)
  let with_extended_env env get_tenv new_tbinds new_vbinds typer =
    let cur_env = get_tenv env in
    let rl = TEnv.addTs cur_env new_tbinds in
    let rl' = TEnv.addVs cur_env new_vbinds in
    let%bind res = typer env in
    let rl'' = TEnv.combine_restores ~older:rl ~newer:rl' in
    let () = TEnv.apply_restore cur_env rl'' in
    pure res

  let rec type_expr (erep : UntypedSyntax.expr_annot) tenv =
    let e, rep = erep in
    match e with
    | Literal l ->
        let%bind lt = fromR_TE @@ literal_type l ~lc:(ER.get_loc rep) in
        pure @@ (TypedSyntax.Literal l, (mk_qual_tp lt, rep))
    | Var i ->
        let%bind r =
          fromR_TE @@ TEnv.resolveT tenv (get_id i) ~lopt:(Some (get_rep i))
        in
        let typ = rr_typ r in
        pure @@ (TypedSyntax.Var (add_type_to_ident i typ), (typ, rep))
    | Fun (arg, t, body) ->
        let%bind () = fromR_TE @@ TEnv.is_wf_type tenv t in
        let%bind ((_, (bt, _)) as b) =
          with_extended_env tenv Fn.id [ (arg, t) ] [] (type_expr body)
        in
        let typed_arg = add_type_to_ident arg (mk_qual_tp t) in
        pure
        @@ ( TypedSyntax.Fun (typed_arg, t, b),
             (mk_qual_tp (FunType (t, bt.tp)), rep) )
    | App (f, actuals) ->
        let%bind fres =
          fromR_TE @@ TEnv.resolveT tenv (get_id f) ~lopt:(Some (get_rep f))
        in
        let%bind typed_actuals, apptyp =
          app_type tenv (rr_typ fres).tp actuals ~lc:(ER.get_loc (get_rep f))
        in
        let typed_f = add_type_to_ident f (rr_typ fres) in
        pure @@ (TypedSyntax.App (typed_f, typed_actuals), (apptyp, rep))
    | Builtin (b, actuals) ->
        let%bind targs, typed_actuals = type_actuals tenv actuals in
        let%bind _, ret_typ, _ =
          fromR_TE @@ BuiltInDictionary.find_builtin_op b targs
        in
        let%bind () = fromR_TE @@ TEnv.is_wf_type tenv ret_typ in
        let q_ret_typ = mk_qual_tp ret_typ in
        let q_ret_tag = ETR.mk_rep rep q_ret_typ in
        pure
        @@ ( TypedSyntax.Builtin ((fst b, q_ret_tag), typed_actuals),
             (q_ret_typ, rep) )
    | Let (i, topt, lhs, rhs) ->
        (* Poor man's error reporting *)
        let%bind ((_, (ityp, _)) as checked_lhs) = type_expr lhs tenv in
        let%bind actual_typ =
          match topt with
          | Some tannot ->
              let%bind () =
                fromR_TE
                @@ assert_type_assignable ~lc:(ER.get_loc rep) ~expected:tannot
                     ~actual:ityp.tp
              in
              pure (mk_qual_tp tannot)
          | None -> pure ityp
        in
        let typed_i = add_type_to_ident i actual_typ in
        let%bind ((_, (rhstyp, _)) as checked_rhs) =
          with_extended_env tenv Fn.id [ (i, ityp.tp) ] [] (type_expr rhs)
        in
        pure
        @@ ( TypedSyntax.Let (typed_i, topt, checked_lhs, checked_rhs),
             (rhstyp, rep) )
    | Constr (cname, ts, actuals) ->
        let%bind _ = mapM ts ~f:(fun t -> fromR_TE @@ TEnv.is_wf_type tenv t) in
        let open Datatypes.DataTypeDictionary in
        let%bind _, constr =
          fromR_TE
          @@ lookup_constructor
               ~sloc:(SR.get_loc (get_rep cname))
               (get_id cname)
        in
        let alen = List.length actuals in
        if constr.arity <> alen then
          fail
            (mk_type_error1
               (sprintf "Constructor %s expects %d arguments, but got %d."
                  (as_string cname) constr.arity alen)
               (SR.get_loc (get_rep cname)))
        else
          let%bind ftyp =
            fromR_TE
            @@ elab_constr_type
                 ~lc:(SR.get_loc (get_rep cname))
                 (get_id cname) ts
          in
          (* Now type-check as a function application *)
          let%bind typed_actuals, apptyp =
            app_type tenv ftyp actuals ~lc:(SR.get_loc (get_rep cname))
          in
          pure @@ (TypedSyntax.Constr (cname, ts, typed_actuals), (apptyp, rep))
    | MatchExpr (x, clauses) ->
        if List.is_empty clauses then
          fail
            (mk_type_error1
               (sprintf "List of pattern matching clauses is empty:\n%s"
                  (pp_expr e))
               (ER.get_loc rep))
        else
          let%bind sctyp =
            fromR_TE @@ TEnv.resolveT tenv (get_id x) ~lopt:(Some (get_rep x))
          in
          let sct = (rr_typ sctyp).tp in
          let%bind typed_clauses_rev =
            foldM clauses ~init:[] ~f:(fun typed_clauses_acc (ptrn, ex) ->
                let%bind typed_clause =
                  type_check_match_branch tenv sct ptrn ex
                in
                pure (typed_clause :: typed_clauses_acc))
          in
          let typed_clauses = List.rev typed_clauses_rev in
          let cl_types =
            List.map typed_clauses ~f:(fun (_, (_, (t, _))) -> t)
          in
          let%bind () =
            fromR_TE
            @@ assert_all_same_type ~lc:(ER.get_loc rep)
                 (List.map ~f:(fun it -> it.tp) cl_types)
          in
          (* Return the first type since all they are the same *)
          pure
          @@ ( TypedSyntax.MatchExpr
                 (add_type_to_ident x (rr_typ sctyp), typed_clauses),
               (List.hd_exn cl_types, rep) )
    | Fixpoint (f, t, body) ->
        let%bind ((_, (bt, _)) as typed_b) =
          with_extended_env tenv Fn.id [ (f, t) ] [] (type_expr body)
        in
        let%bind () =
          fromR_TE
          @@ assert_type_assignable ~lc:(ER.get_loc rep) ~expected:t
               ~actual:bt.tp
        in
        pure
        @@ ( TypedSyntax.Fixpoint
               (add_type_to_ident f (mk_qual_tp t), t, typed_b),
             (mk_qual_tp t, rep) )
    | TFun (tvar, body) ->
        let id = get_id tvar in
        (* XXX this is a workaround for alpha-renaming *)
        (* Make it illegal to declare a new type variable inside the scope of another type variable with the same name *)
        if TEnv.existsV tenv id then
          fail
            (mk_type_error1
               (sprintf "Type variable %s is already in use\n"
                  (TCName.as_error_string id))
               (ER.get_loc (get_rep tvar)))
        else
          let%bind ((_, (bt, _)) as typed_b) =
            with_extended_env tenv Fn.id [] [ tvar ] (type_expr body)
          in
          let typed_tvar = add_type_to_ident tvar bt in
          pure
          @@ ( TypedSyntax.TFun (typed_tvar, typed_b),
               (mk_qual_tp (PolyFun (as_string tvar, bt.tp)), rep) )
    | TApp (tf, arg_types) ->
        let%bind _ =
          mapM arg_types ~f:(fun t -> fromR_TE @@ TEnv.is_wf_type tenv t)
        in
        let%bind tfres =
          fromR_TE @@ TEnv.resolveT tenv (get_id tf) ~lopt:(Some (get_rep tf))
        in
        let tf_rr = rr_typ tfres in
        let tftyp = tf_rr.tp in
        let%bind res_type =
          elab_tfun_with_args tftyp arg_types ~lc:(ER.get_loc rep)
        in
        let%bind () = fromR_TE @@ TEnv.is_wf_type tenv res_type in
        pure
        @@ ( TypedSyntax.TApp (add_type_to_ident tf tf_rr, arg_types),
             (mk_qual_tp res_type, rep) )
    | Message bs ->
        let%bind msg_typ = fromR_TE @@ get_msgevnt_type bs (ER.get_loc rep) in
        let payload_type fld pld =
          let check_field_type seen_type =
            match
              List.Assoc.find CU.msg_mandatory_field_types fld
                ~equal:String.( = )
            with
            | Some fld_t
              when not @@ type_assignable ~expected:fld_t ~actual:seen_type ->
                fail
                  (mk_type_error1
                     (sprintf
                        "Type mismatch for Message field %s. Expected %s but \
                         got %s"
                        fld (pp_typ_error fld_t) (pp_typ_error seen_type))
                     (ER.get_loc rep))
            | _ -> pure ()
          in
          match pld with
          | MLit l ->
              let%bind _, (lt, _) = type_expr (Literal l, rep) tenv in
              let%bind () = check_field_type lt.tp in
              pure @@ TypedSyntax.MLit l
          | MVar i ->
              let%bind r =
                fromR_TE
                @@ TEnv.resolveT tenv (get_id i) ~lopt:(Some (get_rep i))
              in
              let t = rr_typ r in
              let rtp = t.tp in
              let%bind () = check_field_type rtp in
              if is_legal_message_field_type rtp then
                pure @@ TypedSyntax.MVar (add_type_to_ident i t)
              else
                fail
                  (mk_type_error1
                     (sprintf "Cannot serialize values of type %s."
                        (pp_typ_error rtp))
                     (ER.get_loc (get_rep i)))
        in
        let%bind typed_bs_rev =
          (* Make sure we resolve all the payload *)
          foldM bs ~init:[] ~f:(fun typed_bs_acc (s, pld) ->
              let%bind typed_pld = payload_type s pld in
              let typed_bs = (s, typed_pld) in
              pure (typed_bs :: typed_bs_acc))
        in
        pure
        @@ ( TypedSyntax.Message (List.rev typed_bs_rev),
             (mk_qual_tp @@ msg_typ, rep) )
    | GasExpr (g, e) ->
        let%bind ((_, et) as e') = type_expr e tenv in
        pure (TypedSyntax.GasExpr (type_gas_charge g, e'), et)

  and app_type tenv ftyp actuals ~lc =
    (* Type-check function application *)
    let%bind () = fromR_TE @@ TEnv.is_wf_type tenv ftyp in
    let%bind targs, typed_actuals = type_actuals tenv actuals in
    let%bind res_type = fromR_TE @@ fun_type_applies ftyp targs ~lc in
    let%bind () = fromR_TE @@ TEnv.is_wf_type tenv res_type in
    pure @@ (typed_actuals, mk_qual_tp res_type)

  and type_check_match_branch tenv styp ptrn e =
    let%bind new_p, new_typings = assign_types_for_pattern styp ptrn in
    let%bind (_ as typed_e) =
      with_extended_env tenv Fn.id new_typings [] (type_expr e)
    in
    pure @@ (new_p, typed_e)

  and type_actuals tenv actuals =
    let%bind tresults =
      mapM actuals ~f:(fun arg ->
          fromR_TE @@ TEnv.resolveT tenv (get_id arg) ~lopt:(Some (get_rep arg)))
    in
    let tqargs = List.map tresults ~f:rr_typ in
    let targs = List.map tqargs ~f:(fun rr -> rr.tp) in
    let actuals_with_types =
      match List.zip actuals tqargs with
      | Ok l -> l
      | Unequal_lengths ->
          raise
            (mk_internal_error
               "Different number of actuals and Types of actuals")
    in
    let typed_actuals =
      List.map actuals_with_types ~f:(fun (a, t) -> add_type_to_ident a t)
    in
    pure @@ (targs, typed_actuals)

  (**************************************************************)
  (*                   Typing statements                        *)
  (**************************************************************)

  (* Auxiliary structure for types of fields and BC components *)
  type stmt_tenv = {
    pure : TEnv.t;
    fields : TEnv.t;
    procedures : (TCName.t * TCType.t list) list;
  }

  let lookup_proc env pname =
    List.Assoc.find env.procedures ~equal:[%equal: TCName.t] (get_id pname)

  let type_map_access_helper env maptype keys =
    let rec helper maptype keys =
      match (maptype, keys) with
      | MapType (kt, vt), k :: rest ->
          let%bind k_t =
            fromR_TE
            @@ TEnv.resolveT env.pure (get_id k) ~lopt:(Some (get_rep k))
          in
          let%bind () =
            fromR_TE
            @@ assert_type_assignable ~expected:kt ~actual:(rr_typ k_t).tp
                 ~lc:(ER.get_loc (get_rep k))
          in
          let%bind typed_keys, res = helper vt rest in
          let typed_k = add_type_to_ident k (rr_typ k_t) in
          pure @@ (typed_k :: typed_keys, res)
      (* If there are no more keys left, we have the result type. *)
      | _, [] -> pure @@ ([], maptype)
      | _, k :: _ ->
          fail
            (mk_type_error1
               (sprintf "Type failure in map access. Cannot index into key %s"
                  (as_error_string k))
               (ER.get_loc (get_rep k)))
    in
    helper maptype keys

  (* Return typed map accesses and the accessed value's type. *)
  (* (m[k1][k2]... -> (typed_m, typed_k_list, type_of_accessed_value) *)
  let type_map_access env m keys =
    let%bind m_type =
      fromR_TE @@ TEnv.resolveT env.fields (get_id m) ~lopt:(Some (get_rep m))
    in
    let%bind typed_keys, res =
      type_map_access_helper env (rr_typ m_type).tp keys
    in
    let typed_m = add_type_to_ident m (rr_typ m_type) in
    pure (typed_m, typed_keys, res)

  let type_remote_map_access env adr m keys =
    let%bind adr_type =
      fromR_TE @@ TEnv.resolveT env.pure (get_id adr) ~lopt:(Some (get_rep adr))
    in
    let%bind m_type = fromR_TE @@ address_field_type m (rr_typ adr_type).tp in
    let%bind typed_keys, res = type_map_access_helper env m_type keys in
    let typed_m = add_type_to_ident m (mk_qual_tp m_type) in
    let typed_adr = add_type_to_ident adr (rr_typ adr_type) in
    pure (typed_adr, typed_m, typed_keys, res)

  let add_stmt_to_stmts_env_gas s repstmts =
    match repstmts with stmts, env -> (s :: stmts, env)

  let get_tenv_fields env = env.fields

  let get_tenv_pure env = env.pure

  let rec type_stmts stmts get_loc env =
    let open Datatypes.DataTypeDictionary in
    match stmts with
    | [] -> pure ([], env)
    | (s, rep) :: sts -> (
        match s with
        | Load (x, f) ->
            let%bind pure', ident_type =
              let%bind fr =
                fromR_TE
                @@ TEnv.resolveT env.fields (get_id f) ~lopt:(Some (get_rep f))
              in
              pure @@ ((x, (rr_typ fr).tp), rr_typ fr)
            in
            let%bind checked_stmts =
              with_extended_env env get_tenv_pure [ pure' ] []
                (type_stmts sts get_loc)
            in
            let typed_x = add_type_to_ident x ident_type in
            let typed_f = add_type_to_ident f ident_type in
            pure
            @@ add_stmt_to_stmts_env_gas
                 (TypedSyntax.Load (typed_x, typed_f), rep)
                 checked_stmts
        | RemoteLoad (x, adr, f) ->
            let%bind pure', adr_type, ident_type =
              let%bind adr_typ =
                fromR_TE
                @@ TEnv.resolveT env.pure (get_id adr)
                     ~lopt:(Some (get_rep adr))
              in
              let%bind fr =
                fromR_TE @@ address_field_type f (rr_typ adr_typ).tp
              in
              pure @@ ((x, fr), rr_typ adr_typ, mk_qual_tp fr)
            in
            let%bind checked_stmts =
              with_extended_env env get_tenv_pure [ pure' ] []
                (type_stmts sts get_loc)
            in
            let typed_x = add_type_to_ident x ident_type in
            let typed_adr = add_type_to_ident adr adr_type in
            let typed_f = add_type_to_ident f ident_type in
            pure
            @@ add_stmt_to_stmts_env_gas
                 (TypedSyntax.RemoteLoad (typed_x, typed_adr, typed_f), rep)
                 checked_stmts
        | Store (f, r) ->
            if List.mem ~equal:[%equal: TCName.t] no_store_fields (get_id f)
            then
              fail
                (mk_type_error1
                   (sprintf "Writing to the field `%s` is prohibited."
                      (as_error_string f))
                   (ER.get_loc (get_rep f)))
            else
              let%bind checked_stmts, f_type, r_type =
                let%bind fr =
                  fromR_TE
                  @@ TEnv.resolveT env.fields (get_id f)
                       ~lopt:(Some (get_rep f))
                in
                let%bind rr =
                  fromR_TE
                  @@ TEnv.resolveT env.pure (get_id r) ~lopt:(Some (get_rep r))
                in
                let%bind () =
                  fromR_TE
                  @@ assert_type_assignable ~expected:(rr_typ fr).tp
                       ~actual:(rr_typ rr).tp
                       ~lc:(ER.get_loc (get_rep r))
                in
                let%bind checked_stmts = type_stmts sts get_loc env in
                pure @@ (checked_stmts, rr_typ fr, rr_typ rr)
              in
              let typed_f = add_type_to_ident f f_type in
              let typed_r = add_type_to_ident r r_type in
              pure
              @@ add_stmt_to_stmts_env_gas
                   (TypedSyntax.Store (typed_f, typed_r), rep)
                   checked_stmts
        | Bind (x, e) ->
            let%bind ((_, (ityp, _)) as checked_e) = type_expr e env.pure in
            let%bind checked_stmts =
              with_extended_env env get_tenv_pure [ (x, ityp.tp) ] []
                (type_stmts sts get_loc)
            in
            let typed_x = add_type_to_ident x ityp in
            pure
            @@ add_stmt_to_stmts_env_gas
                 (TypedSyntax.Bind (typed_x, checked_e), rep)
                 checked_stmts
        | MapUpdate (m, klist, vopt) ->
            let%bind typed_m, typed_klist, typed_v =
              let%bind typed_m, typed_klist, v_type =
                type_map_access env m klist
              in
              let%bind typed_v =
                match vopt with
                | Some v ->
                    (* This is adding/replacing the value for a key. *)
                    let%bind v_resolv =
                      fromR_TE
                      @@ TEnv.resolveT env.pure (get_id v)
                           ~lopt:(Some (get_rep v))
                    in
                    let typed_v = rr_typ v_resolv in
                    let%bind () =
                      fromR_TE
                      @@ assert_type_assignable ~expected:v_type
                           ~actual:typed_v.tp
                           ~lc:(ER.get_loc (get_rep v))
                    in
                    let typed_v' = add_type_to_ident v typed_v in
                    pure @@ Some typed_v'
                | None -> pure None
                (* This is deleting a key from the map. *)
              in
              pure @@ (typed_m, typed_klist, typed_v)
            in
            (* Check rest of the statements. *)
            let%bind checked_stmts = type_stmts sts get_loc env in
            (* Update annotations. *)
            pure
            @@ add_stmt_to_stmts_env_gas
                 (TypedSyntax.MapUpdate (typed_m, typed_klist, typed_v), rep)
                 checked_stmts
        | MapGet (v, m, klist, valfetch) ->
            let%bind typed_m, typed_klist, v_type =
              let%bind typed_m, typed_klist, v_type =
                type_map_access env m klist
              in
              pure @@ (typed_m, typed_klist, v_type)
            in
            (* The return type of MapGet would be (Option v_type) or Bool. *)
            let v_type' = if valfetch then option_typ v_type else bool_typ in
            (* Update environment. *)
            let typed_v = add_type_to_ident v (mk_qual_tp v_type') in
            (* Check rest of the statements. *)
            let%bind checked_stmts =
              with_extended_env env get_tenv_pure [ (v, v_type') ] []
                (type_stmts sts get_loc)
            in
            (* Update annotations. *)
            pure
            @@ add_stmt_to_stmts_env_gas
                 ( TypedSyntax.MapGet (typed_v, typed_m, typed_klist, valfetch),
                   rep )
                 checked_stmts
        | RemoteMapGet (v, adr, m, klist, valfetch) ->
            let%bind typed_adr, typed_m, typed_klist, v_type =
              let%bind typed_adr, typed_m, typed_klist, v_type =
                type_remote_map_access env adr m klist
              in
              pure @@ (typed_adr, typed_m, typed_klist, v_type)
            in
            (* The return type of MapGet would be (Option v_type) or Bool. *)
            let v_type' = if valfetch then option_typ v_type else bool_typ in
            (* Update environment. *)
            let typed_v = add_type_to_ident v (mk_qual_tp v_type') in
            (* Check rest of the statements. *)
            let%bind checked_stmts =
              with_extended_env env get_tenv_pure [ (v, v_type') ] []
                (type_stmts sts get_loc)
            in
            pure
            @@ add_stmt_to_stmts_env_gas
                 ( TypedSyntax.RemoteMapGet
                     (typed_v, typed_adr, typed_m, typed_klist, valfetch),
                   rep )
                 checked_stmts
        | ReadFromBC (x, bf) ->
            let%bind bt = lookup_bc_type bf in
            let%bind checked_stmts =
              with_extended_env env get_tenv_pure [ (x, bt) ] []
                (type_stmts sts get_loc)
            in
            let typed_x = add_type_to_ident x (mk_qual_tp bt) in
            pure
            @@ add_stmt_to_stmts_env_gas
                 (TypedSyntax.ReadFromBC (typed_x, bf), rep)
                 checked_stmts
        | MatchStmt (x, clauses) ->
            if List.is_empty clauses then
              fail
                (mk_type_error0
                   (sprintf "List of pattern matching clauses is empty:\n%s"
                      (pp_stmt s)))
            else
              let%bind sctyp =
                fromR_TE
                @@ TEnv.resolveT env.pure (get_id x) ~lopt:(Some (get_rep x))
              in
              let sctype = rr_typ sctyp in
              let sct = sctype.tp in
              let typed_x = add_type_to_ident x sctype in
              let%bind checked_clauses_rev =
                foldM clauses ~init:[] ~f:(fun checked_clauses_acc (ptrn, ex) ->
                    let%bind typed_clause =
                      type_match_stmt_branch env sct ptrn ex get_loc
                    in
                    pure @@ (typed_clause :: checked_clauses_acc))
              in
              let checked_clauses = List.rev checked_clauses_rev in
              let%bind checked_stmts = type_stmts sts get_loc env in
              pure
              @@ add_stmt_to_stmts_env_gas
                   (TypedSyntax.MatchStmt (typed_x, checked_clauses), rep)
                   checked_stmts
        | AcceptPayment ->
            let%bind checked_stmts = type_stmts sts get_loc env in
            pure
            @@ add_stmt_to_stmts_env_gas
                 (TypedSyntax.AcceptPayment, rep)
                 checked_stmts
        | SendMsgs i ->
            let%bind r =
              fromR_TE
              @@ TEnv.resolveT env.pure (get_id i) ~lopt:(Some (get_rep i))
            in
            let i_type = rr_typ r in
            let expected = list_typ msg_typ in
            let%bind () =
              fromR_TE
              @@ assert_type_assignable ~expected ~actual:i_type.tp
                   ~lc:(ER.get_loc (get_rep i))
            in
            let typed_i = add_type_to_ident i i_type in
            let%bind checked_stmts = type_stmts sts get_loc env in
            pure
            @@ add_stmt_to_stmts_env_gas
                 (TypedSyntax.SendMsgs typed_i, rep)
                 checked_stmts
        | CreateEvnt i ->
            (* Same as SendMsgs except that this takes a single message instead of a list. *)
            let%bind r =
              fromR_TE
              @@ TEnv.resolveT env.pure (get_id i) ~lopt:(Some (get_rep i))
            in
            let i_type = rr_typ r in
            let%bind () =
              fromR_TE
              @@ assert_type_assignable ~expected:event_typ ~actual:i_type.tp
                   ~lc:(ER.get_loc (get_rep i))
            in
            let typed_i = add_type_to_ident i i_type in
            let%bind checked_stmts = type_stmts sts get_loc env in
            pure
            @@ add_stmt_to_stmts_env_gas
                 (TypedSyntax.CreateEvnt typed_i, rep)
                 checked_stmts
        | CallProc (p, args) ->
            let%bind typed_args =
              let%bind targs, typed_actuals = type_actuals env.pure args in
              match lookup_proc env p with
              | Some arg_typs ->
                  let%bind _ =
                    fromR_TE
                    @@ proc_type_applies arg_typs targs ~lc:(SR.get_loc rep)
                  in
                  pure typed_actuals
              | None ->
                  fail
                    (mk_type_error1
                       (sprintf "Procedure %s not found." (as_error_string p))
                       (SR.get_loc (get_rep p)))
            in
            let%bind checked_stmts = type_stmts sts get_loc env in
            pure
            @@ add_stmt_to_stmts_env_gas
                 (TypedSyntax.CallProc (p, typed_args), rep)
                 checked_stmts
        | Iterate (l, p) -> (
            let%bind lt =
              fromR_TE
              @@ TEnv.resolveT env.pure (get_id l) ~lopt:(Some (get_rep l))
            in
            let l_type = rr_typ lt in
            match lookup_proc env p with
            | Some [ arg_typ ] ->
                let%bind () =
                  fromR_TE
                  (* The procedure accepts an element of l. *)
                  @@ assert_type_assignable ~expected:(list_typ arg_typ)
                       ~actual:l_type.tp
                       ~lc:(ER.get_loc (get_rep l))
                in
                let%bind checked_stmts = type_stmts sts get_loc env in
                pure
                @@ add_stmt_to_stmts_env_gas
                     (TypedSyntax.Iterate (add_type_to_ident l l_type, p), rep)
                     checked_stmts
            | _ ->
                fail
                  (mk_type_error1
                     (sprintf
                        "Procedure %s not found or has incorrect argument type."
                        (as_error_string p))
                     (SR.get_loc (get_rep p))) )
        | Throw iopt -> (
            let%bind checked_stmts = type_stmts sts get_loc env in
            match iopt with
            | Some i ->
                (* Same as CreateEvent. *)
                let%bind r =
                  fromR_TE
                  @@ TEnv.resolveT env.pure (get_id i) ~lopt:(Some (get_rep i))
                in
                let i_type = rr_typ r in
                let%bind () =
                  fromR_TE
                  @@ assert_type_assignable ~expected:exception_typ
                       ~actual:i_type.tp
                       ~lc:(ER.get_loc (get_rep i))
                in
                let typed_i = add_type_to_ident i i_type in
                pure
                @@ add_stmt_to_stmts_env_gas
                     (TypedSyntax.Throw (Some typed_i), rep)
                     checked_stmts
            | None ->
                pure
                @@ add_stmt_to_stmts_env_gas
                     (TypedSyntax.Throw None, rep)
                     checked_stmts )
        | GasStmt g ->
            let%bind checked_stmts = type_stmts sts get_loc env in
            pure
            @@ add_stmt_to_stmts_env_gas
                 (TypedSyntax.GasStmt (type_gas_charge g), rep)
                 checked_stmts )

  and type_match_stmt_branch env styp ptrn sts get_loc =
    let%bind new_p, new_typings = assign_types_for_pattern styp ptrn in
    let%bind new_stmts, _ =
      with_extended_env env get_tenv_pure new_typings []
        (type_stmts sts get_loc)
    in
    pure @@ (new_p, new_stmts)

  let type_component env0 tr =
    let { comp_type; comp_name; comp_params; comp_body } = tr in
    let procedures = env0.procedures in
    let component_type_string = component_type_to_string comp_type in
    let param_checker =
      match comp_type with
      | CompTrans -> is_legal_transition_parameter_type
      | CompProc -> is_legal_procedure_parameter_type
    in
    let%bind typed_cparams =
      mapM
        ~f:(fun (param, t) ->
          if param_checker t then
            pure (add_type_to_ident param (mk_qual_tp t), t)
          else
            fail
              (mk_type_error1
                 (sprintf "Type %s cannot be used as %s parameter"
                    (pp_typ_error t) component_type_string)
                 (ER.get_loc (get_rep param))))
        comp_params
    in
    let append_params = CU.append_implict_comp_params comp_params in
    let%bind typed_stmts, _ =
      with_extended_env env0 get_tenv_pure append_params []
        (type_stmts comp_body ER.get_loc)
    in
    let new_proc_signatures =
      match comp_type with
      | CompTrans -> procedures
      | CompProc ->
          let proc_sig = List.map comp_params ~f:snd in
          List.Assoc.add procedures ~equal:[%equal: TCName.t] (get_id comp_name)
            proc_sig
    in
    pure
    @@ ( {
           TypedSyntax.comp_type;
           TypedSyntax.comp_name;
           TypedSyntax.comp_params = typed_cparams;
           TypedSyntax.comp_body = typed_stmts;
         },
         new_proc_signatures )

  (*****************************************************************)
  (*                 Typing entire contracts                       *)
  (*****************************************************************)
  let type_fields tenv flds =
    let fields_env = TEnv.mk () in
    let%bind typed_flds =
      foldM flds ~init:[] ~f:(fun acc (fn, ft, fe) ->
          let%bind ((_, (ar, _)) as typed_expr) = type_expr fe tenv in
          let actual = ar.tp in
          let%bind () =
            fromR_TE
            @@
            match ft with
            | Address _ ->
                (* Address field.
                   Initialiser must be assignable to ByStr20.
                   Dynamic typecheck ensures that the byte string
                   refers to an address with the correct shape. *)
                assert_type_assignable
                  ~expected:(bystrx_typ Type.address_length)
                  ~actual
                  ~lc:(ER.get_loc (get_rep fn))
            | _ ->
                (* Non-address field.
                   Initialiser must be assignable to field type. *)
                assert_type_assignable ~expected:ft ~actual
                  ~lc:(ER.get_loc (get_rep fn))
          in
          let typed_fs = add_type_to_ident fn ar in
          if is_legal_field_type ft then
            let _ = TEnv.addT fields_env fn actual in
            pure @@ ((typed_fs, ft, typed_expr) :: acc)
          else
            fail
              (mk_type_error1
                 (sprintf "Values of the type \"%s\" cannot be stored."
                    (pp_typ_error ft))
                 (ER.get_loc (get_rep fn))))
    in
    pure @@ (List.rev typed_flds, fields_env)

  (**************************************************************)
  (*                    Typing libraries                        *)
  (**************************************************************)
  let type_rec_libs rec_libs =
    let lib_vars, lib_types =
      List.partition_map rec_libs ~f:(fun le ->
          match le with
          | LibVar (n, t, e) -> First (n, t, e)
          | LibTyp (n, ts) -> Second (n, ts))
    in
    (* recursion primitives must not contain type declarations *)
    let%bind () =
      match lib_types with
      | _ :: _ ->
          fail
            (mk_type_error0
               "Type declarations not allowed in recursion primitives")
      | [] -> pure ()
    in
    let env0 = TEnv.mk () in
    let%bind res =
      foldM lib_vars ~init:[] ~f:(fun entry_acc (rn, topt, body) ->
          let%bind ((_, (ar, _)) as typed_body) = type_expr body env0 in
          let%bind () =
            match topt with
            | Some tannot ->
                fromR_TE
                @@ assert_type_assignable ~expected:tannot ~actual:ar.tp
                     ~lc:(ER.get_loc (snd body))
            | None -> pure ()
          in
          let typed_rn = add_type_to_ident rn ar in
          let new_entries =
            TypedSyntax.LibVar (typed_rn, topt, typed_body) :: entry_acc
          in
          let _ = TEnv.addT env0 rn ar.tp in
          pure @@ new_entries)
    in
    pure (res, env0)

  open MonadUtil
  open Result.Let_syntax

  (* Check that ADT constructors are well-formed.
     Declared ADTs and constructors are added to stored datatypes
     by ADTChecker.
     Checking for ADT types in scope and multiple usages of the
     same constructor name takes place in ADTChecker. *)
  let type_lib_typ_ctrs env (ctr_defs : ctr_def list) =
    forallM
      ~f:(fun ctr_def ->
        forallM
          ~f:(fun c_arg_type -> TEnv.is_wf_type env c_arg_type)
          ctr_def.c_arg_types)
      ctr_defs

  (* In-place updates env0 with entries from this library. *)
  let type_library env0 { lname; lentries = ents } remaining_gas =
    let%bind (typed_entries, errs, _), remaining_gas =
      foldM ents
        ~init:(([], [], []), remaining_gas)
        ~f:(fun ((acc, errs, blist), remaining_gas) lib_entry ->
          match lib_entry with
          | LibTyp (tname, ctr_defs) -> (
              match type_lib_typ_ctrs env0 ctr_defs with
              | Ok () ->
                  (* Ascribing dummy [Unit] type to the new ADT type constructor...
                     This is needed because any identifier must have type ascription
                     after this pass, even if it does not make sense. *)
                  let dummy_type_annot = PlainTypes.mk_qualified_type Unit in
                  let tname = add_type_to_ident tname dummy_type_annot
                  (* Ascribing dummy [Unit] type to the new ADT constructors ... *)
                  and ctr_defs =
                    List.map ctr_defs ~f:(fun { cname; c_arg_types } ->
                        TypedSyntax.
                          {
                            cname = add_type_to_ident cname dummy_type_annot;
                            c_arg_types;
                          })
                  in
                  let libtyp = TypedSyntax.LibTyp (tname, ctr_defs) in
                  Ok ((libtyp :: acc, errs, blist), remaining_gas)
              | Error e -> Ok ((acc, errs @ e, blist), remaining_gas) )
          | LibVar (ln, ltopt, le) -> (
              let dep_on_blist = free_vars_dep_check le blist in
              (* If exp depends on a blacklisted exp, then let's ignore it. *)
              if dep_on_blist then Ok ((acc, errs, ln :: blist), remaining_gas)
              else
                let res = type_expr le env0 init_gas_kont remaining_gas in
                match res with
                | Ok (res', remaining_gas') -> (
                    (* This went good. *)
                    let ((_, (tr, _)) as typed_e) = res' in
                    let thunk () =
                      let _ = TEnv.addT env0 ln tr.tp in
                      let typed_ln = add_type_to_ident ln tr in
                      ( ( TypedSyntax.LibVar (typed_ln, ltopt, typed_e) :: acc,
                          errs,
                          blist ),
                        remaining_gas' )
                    in
                    match ltopt with
                    | Some tannot -> (
                        match
                          assert_type_assignable ~expected:tannot ~actual:tr.tp
                            ~lc:(ER.get_loc (get_rep ln))
                        with
                        | Ok () -> Ok (thunk ())
                        | Error e ->
                            Ok ((acc, errs @ e, ln :: blist), remaining_gas) )
                    | None -> Ok (thunk ()) )
                | Error ((TypeError, e), remaining_gas) ->
                    (* A new original type failure. Add to blocklist and move on. *)
                    Ok ((acc, errs @ e, ln :: blist), remaining_gas)
                | Error ((GasError, e), remaining_gas) ->
                    (* Out of gas. Bail out. *)
                    Error ((GasError, errs @ e), remaining_gas) ))
    in
    (* If there has been no errors at all, we're good to go. *)
    if List.is_empty errs then
      pure
      @@ ( { TypedSyntax.lname; TypedSyntax.lentries = List.rev typed_entries },
           remaining_gas ) (* Else report all errors together. *)
    else Error ((TypeError, errs), remaining_gas)

  (* Type a list of libtrees, with tenv0 as the base environment, updating
   * it in-place, to include entries in elibs (but not their deps). *)
  let type_libraries elibs tenv0 remaining_gas =
    let%bind typed_elibs, _, emsgs, remaining_gas =
      let rec recurser libl files_already_checked remaining_gas =
        foldM libl ~init:([], files_already_checked, [], remaining_gas)
          ~f:(fun (lib_acc, files_checked_acc, emsgs_acc, remaining_gas) elib ->
            let elib_fname = (SR.get_loc (get_rep elib.libn.lname)).fname in
            let%bind ( tc_lib_opt,
                       dep_libs,
                       all_checked_files,
                       all_emsgs,
                       remaining_gas ) =
              (* Only check each library once. Use file names rather than the library names because that's how we identify libraries.
                 TODO, issue #867: We ought to be able to rely on l.lname and ext_lib.libn.lname instead *)
              match
                List.find files_checked_acc ~f:(fun fname ->
                    String.(fname = elib_fname))
              with
              | Some _ ->
                  (* ext_lib already checked *)
                  pure (None, [], files_checked_acc, emsgs_acc, remaining_gas)
              | None -> (
                  (* ext_lib not checked yet *)
                  (* Check dependencies *)
                  let%bind tc_dep_libs, dep_files, dep_emsgs, dep_remaining_gas
                      =
                    recurser elib.deps files_checked_acc remaining_gas
                  in
                  let all_files =
                    (elib_fname :: dep_files) @ files_checked_acc
                  in
                  match type_library tenv0 elib.libn dep_remaining_gas with
                  | Ok (t_lib, remaining_gas) ->
                      pure
                        ( Some t_lib,
                          tc_dep_libs,
                          all_files,
                          emsgs_acc @ dep_emsgs,
                          remaining_gas )
                  | Error ((TypeError, el), remaining_gas) ->
                      (* Collect error, and continue typechecking. *)
                      pure
                        ( None,
                          tc_dep_libs,
                          all_files,
                          emsgs_acc @ dep_emsgs @ el,
                          remaining_gas )
                  | Error ((GasError, el), remaining_gas) ->
                      (* Gas error - bail out *)
                      Error ((GasError, el), remaining_gas) )
            in
            match tc_lib_opt with
            | Some t_lib ->
                let (elib' : TypedSyntax.libtree) =
                  { libn = t_lib; deps = dep_libs }
                in
                (* No reason to remove library entries. Their names are globally unique, and the disambiguator ensures that they are only accessed if they are in scope. *)
                pure
                  ( lib_acc @ [ elib' ],
                    all_checked_files,
                    all_emsgs,
                    remaining_gas )
            | None ->
                (* An error has occurred *)
                Error ((TypeError, all_emsgs), remaining_gas))
      in
      recurser elibs [] remaining_gas
    in
    if List.is_empty emsgs then pure (typed_elibs, remaining_gas)
    else Error ((TypeError, emsgs), remaining_gas)

  let type_lmodule (md : UntypedSyntax.lmodule)
      (rec_libs : UntypedSyntax.lib_entry list)
      (elibs : UntypedSyntax.libtree list) (gas : Stdint.uint64) :
      ( ( TypedSyntax.lmodule
        * TypedSyntax.lib_entry list
        * TypedSyntax.libtree list )
        * Stdint.uint64,
        (TU.typeCheckerErrorType * scilla_error list) * Stdint.uint64 )
      result =
    (* Step 0: Type check recursion principles *)
    let%bind (typed_rlib, tenv0), remaining_gas =
      type_rec_libs rec_libs init_gas_kont gas
    in

    (* Step 1: Type check external libraries. *)
    let%bind typed_elibs, remaining_gas =
      type_libraries elibs tenv0 remaining_gas
    in

    (* Type the library of this module. *)
    let%bind typed_mlib, remaining_gas =
      type_library tenv0 md.libs remaining_gas
    in

    let typed_lmodule =
      {
        TypedSyntax.smver = md.smver;
        TypedSyntax.elibs = md.elibs;
        TypedSyntax.libs = typed_mlib;
      }
    in
    pure ((typed_lmodule, typed_rlib, typed_elibs), remaining_gas)

  let type_module (md : UntypedSyntax.cmodule)
      (* TODO, issue #225 : rec_libs should be added to the libraries when we allow custom, inductive ADTs *)
        (rec_libs : UntypedSyntax.lib_entry list)
      (elibs : UntypedSyntax.libtree list) (gas : Stdint.uint64) :
      ( ( TypedSyntax.cmodule
        * stmt_tenv
        * TypedSyntax.libtree list
        * TypedSyntax.lib_entry list )
        * Stdint.uint64,
        scilla_error list * Stdint.uint64 )
      result =
    let { smver = mod_smver; libs; elibs = mod_elibs; contr } = md in
    let { cname = ctr_cname; cparams; cconstraint; cfields; ccomps } = contr in
    strip_error_type
    @@ (* Step 0: Type check recursion principles *)
       let%bind (typed_rlib, tenv0), remaining_gas =
         type_rec_libs rec_libs init_gas_kont gas
       in

       (* Step 1: Type check external libraries *)
       let%bind (typed_elibs, emsgs), remaining_gas =
         match type_libraries elibs tenv0 remaining_gas with
         | Ok (res, g) -> Ok ((res, []), g)
         | Error ((TypeError, e), g) -> Ok (([], e), g)
         | Error ((GasError, e), g) -> Error ((GasError, e), g)
       in
       (* Step 2: Type check contract library, if defined. *)
       let%bind (typed_clibs, emsgs), remaining_gas =
         match libs with
         | Some lib -> (
             match type_library tenv0 lib remaining_gas with
             | Ok (typed_lib, remaining_gas) ->
                 Ok ((Some typed_lib, emsgs), remaining_gas)
             | Error ((TypeError, e), gas_remaining) ->
                 Ok ((None, e), gas_remaining)
             | Error ((GasError, e), gas_remaining) ->
                 Error ((GasError, e), gas_remaining) )
         | None -> pure ((None, emsgs), remaining_gas)
       in

       (* Step 3: Adding typed contract parameters (incl. implicit ones) *)
       let params = CU.append_implict_contract_params cparams in
       let _ = TEnv.addTs tenv0 params in

       (* Step 4: Typecheck contract constraint. *)
       let%bind (typed_constraint, emsgs), remaining_gas =
         let res =
           let%bind ((_, (ityp, rep)) as checked_constraint), remaining_gas =
             type_expr cconstraint tenv0 init_gas_kont remaining_gas
           in
           match
             assert_type_assignable
               ~expected:Datatypes.DataTypeDictionary.bool_typ ~actual:ityp.tp
               ~lc:(ER.get_loc rep)
           with
           | Ok () -> pure (checked_constraint, remaining_gas)
           | Error e -> Error ((TypeError, e), remaining_gas)
         in
         match res with
         | Ok (checked_constraint, remaining_gas) ->
             Ok ((checked_constraint, emsgs), remaining_gas)
         | Error ((TypeError, e), g) ->
             Ok
               ( ( (TypedSyntax.Literal TCLiteral.false_lit, ETR.dummy_rep),
                   emsgs @ e ),
                 g )
         | Error ((GasError, e), g) -> Error ((GasError, emsgs @ e), g)
       in

       (* Step 5: Type-check fields and add balance *)
       let%bind ((typed_fields, fenv0), femsgs0), remaining_gas =
         match type_fields tenv0 cfields init_gas_kont remaining_gas with
         | Ok ((typed_fields, fenv), g) -> Ok (((typed_fields, fenv), emsgs), g)
         | Error ((TypeError, el), g) -> Ok ((([], tenv0), emsgs @ el), g)
         | Error ((GasError, el), g) -> Error ((GasError, el), g)
       in
       let bn, bt = CU.balance_field in
       let _ = TEnv.addT fenv0 bn bt in

       (* Step 6: Form a general environment for checking components *)
       let env = { pure = tenv0; fields = fenv0; procedures = [] } in

       (* Step 7: Type-checking all components in batch *)
       let%bind ((t_comps, _), emsgs'), remaining_gas =
         foldM
           ~init:((([], []), femsgs0), remaining_gas)
           ccomps
           ~f:(fun (((comp_acc, proc_acc), emsgs), remaining_gas') tr ->
             let toplevel_env =
               { pure = env.pure; fields = env.fields; procedures = proc_acc }
             in
             match
               type_component toplevel_env tr init_gas_kont remaining_gas'
             with
             | Ok ((typed_comp, proc_sigs), g) ->
                 Ok (((typed_comp :: comp_acc, proc_sigs), emsgs), g)
             | Error ((TypeError, el), g) ->
                 Ok (((comp_acc, proc_acc), emsgs @ el), g)
             | Error ((GasError, el), g) -> Error ((GasError, el), g))
       in
       let typed_comps = List.rev t_comps in

       (* Step 8: Lift contract parameters to ETR.rep ident *)
       let typed_params =
         List.map cparams ~f:(fun (id, t) ->
             (add_type_to_ident id (mk_qual_tp t), t))
       in

       if List.is_empty emsgs' (* Return pure environment *) then
         pure
           ( ( {
                 TypedSyntax.smver = mod_smver;
                 TypedSyntax.libs = typed_clibs;
                 TypedSyntax.elibs = mod_elibs;
                 TypedSyntax.contr =
                   {
                     TypedSyntax.cname = ctr_cname;
                     TypedSyntax.cparams = typed_params;
                     TypedSyntax.cconstraint = typed_constraint;
                     TypedSyntax.cfields = typed_fields;
                     TypedSyntax.ccomps = typed_comps;
                   };
               },
               env,
               typed_elibs,
               typed_rlib ),
             remaining_gas ) (* Return error messages *)
       else Error ((TypeError, emsgs'), remaining_gas)

  (**************************************************************)
  (*                    Staging API                             *)
  (**************************************************************)

  module OutputSyntax = TypedSyntax
  module OutputSRep = STR
  module OutputERep = ETR
end
