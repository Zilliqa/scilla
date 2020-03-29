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
open! Int.Replace_polymorphic_compare
open ErrorUtils
open Sexplib.Std
open Syntax
open MonadUtil
open Result.Let_syntax
open Datatypes
open PrettyPrinters

(****************************************************************)
(*                Inferred types and qualifiers                 *)
(****************************************************************)

type 'rep inferred_type = { tp : typ; qual : 'rep } [@@deriving sexp]

module type QualifiedTypes = sig
  type t

  val t_of_sexp : Sexp.t -> t

  val sexp_of_t : t -> Sexp.t

  val mk_qualified_type : typ -> t inferred_type
end

module type MakeTEnvFunctor = functor (Q : QualifiedTypes) (R : Rep) -> sig
  (* Resolving results *)
  type resolve_result

  val rr_loc : resolve_result -> loc

  val rr_rep : resolve_result -> R.rep

  val rr_typ : resolve_result -> Q.t inferred_type

  val rr_pp : resolve_result -> string

  val mk_qual_tp : typ -> Q.t inferred_type

  module TEnv : sig
    type t

    (* Make new type environment *)
    val mk : t

    (* Add to type environment *)
    val addT : t -> R.rep ident -> typ -> t

    (* Add to many type bindings *)
    val addTs : t -> (R.rep ident * typ) list -> t

    (* Add type variable to the environment *)
    val addV : t -> R.rep ident -> t

    (* Append env' to env in place. *)
    val append : t -> t -> t

    (* Retain only those keys for which (fb k v) is true. *)
    val filterTs : t -> f:(string -> resolve_result -> bool) -> t

    (* Check type for well-formedness in the type environment *)
    val is_wf_type : t -> typ -> (unit, scilla_error list) result

    (* Resolve the identifier *)
    val resolveT :
      ?lopt:R.rep option ->
      t ->
      string ->
      (resolve_result, scilla_error list) result

    (* Is bound in environment? *)
    val existsT : t -> string -> bool

    (* Is bound in tvars? *)
    val existsV : t -> string -> bool

    (* Copy the environment *)
    val copy : t -> t

    (* Convert to list *)
    val to_list : t -> (string * resolve_result) list

    (* Get type variables *)
    val tvars : t -> (string * R.rep) list

    (* Print the type environment *)
    val pp : ?f:(string * resolve_result -> bool) -> t -> string
  end
end

(****************************************************************)
(*                   Typing environments                        *)
(****************************************************************)

(* Typing environment, parameterised by a qualifier *)
module MakeTEnv : MakeTEnvFunctor =
functor
  (Q : QualifiedTypes)
  (R : Rep)
  ->
  struct
    type resolve_result = { qt : Q.t inferred_type; rep : R.rep }

    let rr_loc rr = R.get_loc rr.rep

    let rr_rep rr = rr.rep

    let rr_typ rr = rr.qt

    let rr_pp rr =
      let t = (rr_typ rr).tp |> pp_typ in
      let r = Sexp.to_string @@ R.sexp_of_rep (rr_rep rr) in
      "(" ^ t ^ ", " ^ r ^ ")"

    let mk_qual_tp tp = Q.mk_qualified_type tp

    module TEnv = struct
      type t = {
        (* Typed identifiers *)
        tenv : (string, resolve_result) Hashtbl.t;
        (* Context for type variables and their rep *)
        tvars : (string, R.rep) Hashtbl.t;
      }

      let addT env id tp =
        let _ =
          Hashtbl.add env.tenv (get_id id)
            { qt = Q.mk_qualified_type tp; rep = get_rep id }
        in
        env

      let addTs env kvs =
        List.fold_left ~init:env ~f:(fun z (k, v) -> addT z k v) kvs

      let addV env id =
        let _ = Hashtbl.add env.tvars (get_id id) (get_rep id) in
        env

      (* Append env' to env in place. *)
      let append env env' =
        let _ = Hashtbl.iter (fun k v -> Hashtbl.add env.tenv k v) env'.tenv in
        env

      (* Retain only those keys for which (fb k v) is true. *)
      let filterTs env ~f =
        let _ =
          Hashtbl.filter_map_inplace
            (fun k v -> if f k v then Some v else None)
            env.tenv
        in
        env

      let tvars env =
        Hashtbl.fold (fun key data z -> (key, data) :: z) env.tvars []

      let to_list env =
        Hashtbl.fold (fun key data z -> (key, data) :: z) env.tenv []

      (* Check type for well-formedness in the type environment *)
      let is_wf_type tenv t =
        let rec is_wf_typ' t' tb =
          match t' with
          | MapType (kt, vt) ->
              let%bind _ = is_wf_typ' kt tb in
              is_wf_typ' vt tb
          | FunType (at, rt) ->
              let%bind _ = is_wf_typ' at tb in
              is_wf_typ' rt tb
          | ADT (n, ts) ->
              let open Datatypes.DataTypeDictionary in
              let%bind adt = lookup_name ~sloc:(get_rep n) (get_id n) in
              if List.length ts <> List.length adt.tparams then
                fail1
                  (sprintf "ADT type %s expects %d arguments but got %d.\n"
                     (get_id n) (List.length adt.tparams) (List.length ts))
                  (get_rep n)
              else foldM ~f:(fun _ ts' -> is_wf_typ' ts' tb) ~init:() ts
          | PrimType _ | Unit -> pure ()
          | TypeVar a ->
              (* Check if bound locally. *)
              if List.mem tb a ~equal:String.( = ) then pure ()
                (* Check if bound in environment. *)
              else if Caml.Hashtbl.mem tenv.tvars a then pure ()
              else
                fail0
                @@ sprintf "Unbound type variable %s in type %s" a (pp_typ t)
          | PolyFun (arg, bt) -> is_wf_typ' bt (arg :: tb)
        in
        is_wf_typ' t []

      let pp ?(f = fun _ -> true) env =
        let lst = List.filter (to_list env) ~f in
        let ps =
          List.map lst ~f:(fun (k, v) -> " [" ^ k ^ " : " ^ rr_pp v ^ "]")
        in
        let cs = String.concat ~sep:",\n " ps in
        "{" ^ cs ^ " }"

      let resolveT ?(lopt = None) env id =
        match Hashtbl.find_opt env.tenv id with
        | Some r -> pure r
        | None ->
            let sloc =
              match lopt with Some l -> R.get_loc l | None -> dummy_loc
            in
            fail1 (sprintf "Couldn't resolve the identifier \"%s\".\n" id) sloc

      let existsT env id = Hashtbl.mem env.tenv id

      let existsV env id = Hashtbl.mem env.tvars id

      let copy e = { tenv = Hashtbl.copy e.tenv; tvars = Hashtbl.copy e.tvars }

      let mk =
        let t1 = Hashtbl.create 50 in
        let t2 = Hashtbl.create 10 in
        let env = { tenv = t1; tvars = t2 } in
        copy env
    end
  end

(***************************************************************)
(*               Specific instantiations                        *)
(****************************************************************)

module PlainTypes : QualifiedTypes = struct
  type t = unit [@@deriving sexp]

  let mk_qualified_type t = { tp = t; qual = () }
end

(****************************************************************)
(*             Utility function for matching types              *)
(****************************************************************)

module TypeUtilities = struct
  module MakeTEnv = MakeTEnv

  type typeCheckerErrorType = TypeError | GasError

  let mk_type_error0 msg remaining_gas =
    (TypeError, mk_error0 msg, remaining_gas)

  let mk_type_error1 msg loc remaining_gas =
    (TypeError, mk_error1 msg loc, remaining_gas)

  let wrap_error_with_errortype_and_gas errorType gas res =
    match res with Ok r -> Ok r | Error e -> Error (errorType, e, gas)

  let mark_error_as_type_error gas res =
    wrap_error_with_errortype_and_gas TypeError gas res

  (* Some useful data type constructors *)
  let fun_typ t s = FunType (t, s)

  let tvar i = TypeVar i

  let tfun_typ i t = PolyFun (i, t)

  let map_typ k v = MapType (k, v)

  let unit_typ = Unit

  (* Return True if corresponding elements are `type_equiv`,
     False otherwise, or if unequal lengths. *)
  let type_equiv_list tlist1 tlist2 =
    List.length tlist1 = List.length tlist2
    && not
         (List.exists2_exn tlist1 tlist2 ~f:(fun t1 t2 ->
              not ([%equal: typ] t1 t2)))

  let assert_type_equiv expected given =
    if [%equal: typ] expected given then pure ()
    else
      fail0
      @@ sprintf "Type mismatch: %s expected, but %s provided."
           (pp_typ expected) (pp_typ given)

  (* TODO: make this charge gas *)
  let assert_type_equiv_with_gas expected given remaining_gas =
    if [%equal: typ] expected given then pure remaining_gas
    else
      Error
        ( TypeError,
          mk_error0
            (sprintf "Type mismatch: %s expected, but %s provided."
               (pp_typ expected) (pp_typ given)),
          remaining_gas )

  let rec is_ground_type t =
    match t with
    | FunType (a, r) -> is_ground_type a && is_ground_type r
    | MapType (k, v) -> is_ground_type k && is_ground_type v
    | ADT (_, ts) -> List.for_all ~f:(fun t -> is_ground_type t) ts
    | PolyFun _ | TypeVar _ -> false
    | _ -> true

  let rec is_non_map_ground_type t =
    match t with
    | FunType (a, r) -> is_non_map_ground_type a && is_non_map_ground_type r
    | MapType (_, _) -> false
    | ADT (_, ts) -> List.for_all ~f:(fun t -> is_non_map_ground_type t) ts
    | PolyFun _ | TypeVar _ -> false
    | _ -> true

  let rec is_serializable_storable_helper accept_maps t seen_adts =
    match t with
    | FunType _ | PolyFun _ | Unit -> false
    | MapType (kt, vt) ->
        accept_maps
        && is_serializable_storable_helper accept_maps kt seen_adts
        && is_serializable_storable_helper accept_maps vt seen_adts
    | TypeVar _ ->
        (* If we are inside an ADT, then type variable
           instantiations are handled outside *)
        not @@ List.is_empty seen_adts
    | PrimType _ ->
        (* Messages and Events are not serialisable in terms of contract parameters *)
        PrimTypes.(
          (not @@ [%equal: typ] t msg_typ) || [%equal: typ] t event_typ)
    | ADT (tname, ts) -> (
        if List.mem seen_adts tname ~equal:equal_id then true
          (* Inductive ADT - ignore this branch *)
        else
          (* Check that ADT is serializable *)
          match
            DataTypeDictionary.lookup_name ~sloc:(get_rep tname) (get_id tname)
          with
          | Error _ -> false (* Handle errors outside *)
          | Ok adt ->
              let adt_serializable =
                List.for_all adt.tmap ~f:(fun (_, carg_list) ->
                    List.for_all carg_list ~f:(fun carg ->
                        is_serializable_storable_helper accept_maps carg
                          (tname :: seen_adts)))
              in
              adt_serializable
              && List.for_all ts ~f:(fun t ->
                     is_serializable_storable_helper accept_maps t seen_adts) )

  let is_serializable_type t = is_serializable_storable_helper false t []

  let is_storable_type t = is_serializable_storable_helper true t []

  let get_msgevnt_type m =
    let open ContractUtil.MessagePayload in
    if List.Assoc.mem m tag_label ~equal:String.( = ) then
      pure PrimTypes.msg_typ
    else if List.Assoc.mem m eventname_label ~equal:String.( = ) then
      pure PrimTypes.event_typ
    else if List.Assoc.mem m exception_label ~equal:String.( = ) then
      pure PrimTypes.exception_typ
    else fail0 "Invalid message construct. Not any of send, event or exception."

  (* Given a map type and a list of key types, what is the type of the accessed value? *)
  let rec map_access_type mt nindices =
    match (mt, nindices) with
    | _, 0 -> pure mt
    | MapType (_, vt'), 1 -> pure vt'
    | MapType (_, vt'), nkeys' when nkeys' > 1 ->
        map_access_type vt' (nindices - 1)
    | _, _ -> fail0 "Cannot index into map: Too many index keys."

  (* The depth of a nested map. *)
  let rec map_depth mt =
    match mt with MapType (_, vt) -> 1 + map_depth vt | _ -> 0

  let pp_typ_list ts =
    let tss = List.map ~f:(fun t -> pp_typ t) ts in
    sprintf "[%s]" (String.concat ~sep:"; " tss)

  (*
     Check that function type applies for a given arity n
     to a list of argument types.
     Returns the resul type of application or failure
  *)
  let rec fun_type_applies ft argtypes =
    match (ft, argtypes) with
    | FunType (argt, rest), a :: ats ->
        let%bind _ = assert_type_equiv argt a in
        fun_type_applies rest ats
    | FunType (Unit, rest), [] -> pure rest
    | t, [] -> pure t
    | _ ->
        fail0
        @@ sprintf
             "The type\n\
              %s\n\
              doesn't apply, as a function, to the arguments of types\n\
              %s."
             (pp_typ ft) (pp_typ_list argtypes)

  let proc_type_applies formals actuals =
    map2M formals actuals ~f:assert_type_equiv ~msg:(fun () ->
        mk_error0 "Incorrect number of arguments to procedure")

  let rec elab_tfun_with_args_no_gas tf args =
    match (tf, args) with
    | (PolyFun _ as pf), a :: args' ->
        let afv = free_tvars a in
        let%bind n, tp =
          match refresh_tfun pf afv with
          | PolyFun (a, b) -> pure (a, b)
          | _ -> Error (mk_error0 "This can't happen!")
        in
        (* This needs to account for gas *)
        let tp' = subst_type_in_type n a tp in
        elab_tfun_with_args_no_gas tp' args'
    | t, [] -> pure t
    | _ ->
        let msg =
          sprintf
            "Cannot elaborate expression of type\n\
             %s\n\
             applied, as a type function, to type arguments\n\
             %s."
            (pp_typ tf) (pp_typ_list args)
        in
        Error (mk_error0 msg)

  let subst_type_cost tvar tm tp_size =
    match tm with
    | PrimType _ | Unit
    | MapType (_, _)
    | FunType (_, _)
    | ADT (_, _)
    | PolyFun (_, _) ->
        1
    | TypeVar n -> if String.(n = tvar) then tp_size else 1

  (* Count the number of AST nodes in a type *)
  let rec type_size t =
    match t with
    | PrimType _ | Unit | TypeVar _ -> 1
    | PolyFun (_, t) -> 1 + type_size t
    | MapType (t1, t2) | FunType (t1, t2) -> 1 + type_size t1 + type_size t2
    | ADT (_, ts) ->
        List.fold_left ts ~init:1 ~f:(fun acc t -> acc + type_size t)

  (* tm[tvar := tp]
     Parallel implementation to the one in Syntax.ml to allow gas accounting.
  *)
  let subst_type_in_type_with_gas tvar tp tm gas =
    let tp_size = type_size tp in
    let rec recurser t remaining_gas =
      let gas_cost = Stdint.Uint64.of_int @@ subst_type_cost tvar t tp_size in
      if Stdint.Uint64.compare remaining_gas gas_cost >= 0 then
        let remaining_gas' = Stdint.Uint64.sub remaining_gas gas_cost in
        (*        let _ = printf "recursing over %s\n" (pp_typ t) in *)
        match t with
        | PrimType _ | Unit -> pure (t, remaining_gas')
        (* Make sure the map's type is still primitive! *)
        | MapType (kt, vt) ->
            let%bind kts, remaining_gas = recurser kt remaining_gas' in
            let%bind vts, remaining_gas = recurser vt remaining_gas in
            pure (MapType (kts, vts), remaining_gas)
        | FunType (at, rt) ->
            let%bind ats, remaining_gas = recurser at remaining_gas' in
            let%bind rts, remaining_gas = recurser rt remaining_gas in
            pure (FunType (ats, rts), remaining_gas)
        | TypeVar n ->
            let res = if String.(tvar = n) then tp else t in
            pure (res, remaining_gas')
        | ADT (s, ts) ->
            let%bind ts'_rev, remaining_gas =
              foldM ts ~init:([], remaining_gas')
                ~f:(fun (ts'_rev_acc, remaining_gas) t' ->
                  let%bind res, remaining_gas = recurser t' remaining_gas in
                  pure (res :: ts'_rev_acc, remaining_gas))
            in
            pure (ADT (s, List.rev ts'_rev), remaining_gas)
        | PolyFun (arg, t') ->
            if String.(tvar = arg) then pure (t, remaining_gas')
            else
              let%bind res, remaining_gas = recurser t' remaining_gas' in
              pure (PolyFun (arg, res), remaining_gas)
      else Error (GasError, EvalMonad.out_of_gas_err, remaining_gas)
    in
    recurser tm gas

  let rec elab_tfun_with_args tf args gas =
    match (tf, args) with
    | (PolyFun _ as pf), a :: args' ->
        let afv = free_tvars a in
        let%bind n, tp =
          match refresh_tfun pf afv with
          | PolyFun (a, b) -> pure (a, b)
          | _ -> Error (TypeError, mk_error0 "This can't happen!", gas)
        in
        let%bind tp', remaining_gas = subst_type_in_type_with_gas n a tp gas in
        elab_tfun_with_args tp' args' remaining_gas
    | t, [] -> pure (t, gas)
    | _ ->
        let msg =
          sprintf
            "Cannot elaborate expression of type\n\
             %s\n\
             applied, as a type function, to type arguments\n\
             %s."
            (pp_typ tf) (pp_typ_list args)
        in
        Error (TypeError, mk_error0 msg, gas)

  (****************************************************************)
  (*                        Working with ADTs                     *)
  (****************************************************************)

  let apply_type_subst tmap tp =
    List.fold_left tmap ~init:tp ~f:(fun acc_tp (tv, tp) ->
        subst_type_in_type tv tp acc_tp)

  let validate_param_length cn plen alen =
    if plen <> alen then
      fail0
      @@ sprintf "Constructor %s expects %d type arguments, but got %d." cn plen
           alen
    else pure ()

  (* Avoid variable clashes *)
  let refresh_adt adt taken =
    let { tparams; tmap; _ } = adt in
    let tkn = tparams @ taken in
    let subst = List.map tparams ~f:(fun tp -> (tp, mk_fresh_var tkn tp)) in
    let tparams' = List.unzip subst |> snd in
    let subst =
      List.zip_exn tparams @@ List.map tparams' ~f:(fun s -> TypeVar s)
    in
    let tmap' =
      List.map tmap ~f:(fun (cn, tls) ->
          let tls' = List.map tls ~f:(subst_types_in_type subst) in
          (cn, tls'))
    in
    { adt with tparams = tparams'; tmap = tmap' }

  (*  Get elaborated constructor type *)
  let elab_constr_type cn targs =
    let open Datatypes.DataTypeDictionary in
    let%bind adt', _ = lookup_constructor cn in
    let seq a b = if String.(a = b) then 0 else 1 in
    let taken =
      List.concat_map targs ~f:free_tvars |> List.dedup_and_sort ~compare:seq
    in
    let adt = refresh_adt adt' taken in
    let plen = List.length adt.tparams in
    let alen = List.length targs in
    let%bind _ = validate_param_length cn plen alen in
    let res_typ = ADT (asId adt.tname, targs) in
    match List.Assoc.find adt.tmap cn ~equal:String.( = ) with
    | None -> pure res_typ
    | Some ctparams ->
        let tmap = List.zip_exn adt.tparams targs in
        let ctparams_elab = List.map ctparams ~f:(apply_type_subst tmap) in
        let ctyp =
          List.fold_right ctparams_elab ~init:res_typ ~f:(fun ctp acc ->
              fun_typ ctp acc)
        in
        pure ctyp

  let extract_targs cn (adt : Datatypes.adt) atyp =
    match atyp with
    | ADT (name, targs) ->
        if String.(adt.tname = get_id name) then
          let plen = List.length adt.tparams in
          let alen = List.length targs in
          let%bind _ = validate_param_length cn plen alen in
          pure targs
        else
          fail0
          @@ sprintf
               "Types don't match: pattern uses a constructor of type %s, but \
                value of type %s is given."
               adt.tname (get_id name)
    | _ -> fail0 @@ sprintf "Not an algebraic data type: %s" (pp_typ atyp)

  let constr_pattern_arg_types atyp cn =
    let open Datatypes.DataTypeDictionary in
    let%bind adt', _ = lookup_constructor cn in
    let taken = free_tvars atyp in
    let adt = refresh_adt adt' taken in
    let%bind targs = extract_targs cn adt atyp in
    match constr_tmap adt cn with
    | None -> pure []
    | Some tms ->
        let subst = List.zip_exn adt.tparams targs in
        pure @@ List.map ~f:(apply_type_subst subst) tms

  let assert_all_same_type ts =
    match ts with
    | [] -> fail0 "Checking an empty type list."
    | t :: ts' -> (
        match List.find ts' ~f:(fun t' -> not ([%equal: typ] t t')) with
        | None -> pure ()
        | Some _ ->
            fail0
            @@ sprintf "Not all types of the branches %s are equivalent."
                 (pp_typ_list ts) )

  (****************************************************************)
  (*                     Typing literals                          *)
  (****************************************************************)

  let literal_type l =
    let open PrimTypes in
    match l with
    | IntLit (Int32L _) -> pure int32_typ
    | IntLit (Int64L _) -> pure int64_typ
    | IntLit (Int128L _) -> pure int128_typ
    | IntLit (Int256L _) -> pure int256_typ
    | UintLit (Uint32L _) -> pure uint32_typ
    | UintLit (Uint64L _) -> pure uint64_typ
    | UintLit (Uint128L _) -> pure uint128_typ
    | UintLit (Uint256L _) -> pure uint256_typ
    | StringLit _ -> pure string_typ
    | BNum _ -> pure bnum_typ
    | ByStr _ -> pure bystr_typ
    | ByStrX bs -> pure (bystrx_typ (Bystrx.width bs))
    (* Check that messages and events have storable parameters. *)
    | Msg bs -> get_msgevnt_type bs
    | Map ((kt, vt), _) -> pure (MapType (kt, vt))
    | ADTValue (cname, ts, _) ->
        let%bind adt, _ = DataTypeDictionary.lookup_constructor cname in
        pure @@ ADT (asId adt.tname, ts)
    | Clo _ -> fail0 @@ "Cannot type runtime closure."
    | TAbs _ -> fail0 @@ "Cannot type runtime type function."

  (* Verifies a literal to be wellformed and returns it's type. *)
  let rec is_wellformed_lit l =
    let open PrimTypes in
    match l with
    | IntLit (Int32L _) -> pure int32_typ
    | IntLit (Int64L _) -> pure int64_typ
    | IntLit (Int128L _) -> pure int128_typ
    | IntLit (Int256L _) -> pure int256_typ
    | UintLit (Uint32L _) -> pure uint32_typ
    | UintLit (Uint64L _) -> pure uint64_typ
    | UintLit (Uint128L _) -> pure uint128_typ
    | UintLit (Uint256L _) -> pure uint256_typ
    | StringLit _ -> pure string_typ
    | BNum _ -> pure bnum_typ
    | ByStr _ -> pure bystr_typ
    | ByStrX bsx -> pure (bystrx_typ (Bystrx.width bsx))
    (* Check that messages and events have storable parameters. *)
    | Msg m ->
        let%bind msg_typ = get_msgevnt_type m in
        let%bind all_storable =
          foldM
            ~f:(fun acc (_, l) ->
              let%bind t = is_wellformed_lit l in
              if acc then pure (is_storable_type t) else pure false)
            ~init:true m
        in
        if not all_storable then
          fail0 @@ sprintf "Message/Event has invalid / non-storable parameters"
        else pure msg_typ
    | Map ((kt, vt), kv) ->
        if PrimTypes.is_prim_type kt then
          (* Verify that all key/vals conform to kt,vt, recursively. *)
          let%bind valid =
            Caml.Hashtbl.fold
              (fun k v res' ->
                let%bind res = res' in
                if not res then pure @@ res
                else
                  let%bind kt' = is_wellformed_lit k in
                  let%bind vt' = is_wellformed_lit v in
                  pure @@ ([%equal: typ] kt kt' && [%equal: typ] vt vt'))
              kv (pure true)
          in
          if not valid then
            fail0 @@ sprintf "Malformed literal %s" (pp_literal l)
            (* We have a valid Map literal. *)
          else pure (MapType (kt, vt))
        else fail0 @@ sprintf "Not a primitive map key type: %s." (pp_typ kt)
    | ADTValue (cname, ts, args) ->
        let%bind adt, constr = DataTypeDictionary.lookup_constructor cname in
        let tparams = adt.tparams in
        let tname = adt.tname in
        if not (List.length tparams = List.length ts) then
          fail0
          @@ sprintf
               "Wrong number of type parameters for ADT %s (%i) in constructor \
                %s."
               tname (List.length ts) cname
        else if not (List.length args = constr.arity) then
          fail0
          @@ sprintf
               "Wrong number of arguments to ADT %s (%i) in constructor %s."
               tname (List.length args) cname
          (* Verify that the types of args match that declared. *)
        else
          let res = ADT (asId tname, ts) in
          let%bind tmap = constr_pattern_arg_types res cname in
          let%bind arg_typs = mapM ~f:(fun l -> is_wellformed_lit l) args in
          let args_valid = List.for_all2_exn tmap arg_typs ~f:[%equal: typ] in
          if not args_valid then
            fail0
            @@ sprintf "Malformed ADT %s. Arguments do not match expected types"
                 (pp_literal l)
          else pure @@ res
    | Clo _ -> fail0 @@ "Cannot type-check runtime closure."
    | TAbs _ -> fail0 @@ "Cannot type-check runtime type function."
end

(*****************************************************************)
(*               Blockchain component typing                     *)
(*****************************************************************)

let blocknum_name = "BLOCKNUMBER"
