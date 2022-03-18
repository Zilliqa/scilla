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
open ErrorUtils
open Sexplib.Std
open Literal
open Syntax
open MonadUtil
open Result.Let_syntax
open Datatypes
open PrettyPrinters
module TULiteral = GlobalLiteral
module TUType = TULiteral.LType
module TUIdentifier = TUType.TIdentifier
module TUName = TUIdentifier.Name
open TUIdentifier
open TUType

(****************************************************************)
(*                Inferred types and qualifiers                 *)
(****************************************************************)

type 'rep inferred_type = { tp : TUType.t; qual : 'rep } [@@deriving sexp]

module type QualifiedTypes = sig
  type t

  val t_of_sexp : Sexp.t -> t

  val sexp_of_t : t -> Sexp.t

  val mk_qualified_type : TUType.t -> t inferred_type
end

module type MakeTEnvFunctor = functor (Q : QualifiedTypes) (R : Rep) -> sig
  (* Resolving results *)
  type resolve_result

  val rr_loc : resolve_result -> loc

  val rr_rep : resolve_result -> R.rep

  val rr_typ : resolve_result -> Q.t inferred_type

  val rr_pp : resolve_result -> string

  val mk_qual_tp : TUType.t -> Q.t inferred_type

  module TEnv : sig
    type t

    type restore

    (* Make new type environment *)
    val mk : unit -> t

    (* Add to type environment *)
    val addT : t -> R.rep TUIdentifier.t -> TUType.t -> restore

    (* Add to many type bindings *)
    val addTs : t -> (R.rep TUIdentifier.t * TUType.t) list -> restore

    (* Add type variable to the environment *)
    val addV : t -> R.rep TUIdentifier.t -> restore

    (* Add many type variables to the environment. *)
    val addVs : t -> R.rep TIdentifier.t list -> restore

    (* Remove the latest binding for the argument. *)
    val remT : t -> R.rep TIdentifier.t -> restore

    (* Remove the latest bindings for the arguments. *)
    val remTs : t -> R.rep TUIdentifier.t list -> restore

    (* Restore the environment by applying the restore object. *)
    val apply_restore : t -> restore -> unit

    (* Combine a new list of restores with an older list. *)
    val combine_restores : older:restore -> newer:restore -> restore

    (* Check type for well-formedness in the type environment *)
    val is_wf_type : t -> TUType.t -> (unit, scilla_error list) result

    (* Resolve the identifier *)
    val resolveT :
      ?lopt:R.rep option ->
      t ->
      TUName.t ->
      (resolve_result, scilla_error list) result

    (* Is bound in environment? *)
    val existsT : t -> TUName.t -> bool

    (* Is bound in tvars? *)
    val existsV : t -> TUName.t -> bool

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

      type restore_op =
        (* This relies on Caml.Hashtbl allowing adding
         * bindings on top of existing ones and removing them
         * to restore the older binding. Restoring working in
         * FIFO order, i.e., most recent change restored first. *)
        | AddT of string * resolve_result
        | RemT of string
        (* | AddV of string * R.rep *)
        | RemV of string

      type restore = restore_op list

      let addT env id tp =
        let _ =
          Hashtbl.add env.tenv (as_string id)
            { qt = Q.mk_qualified_type tp; rep = get_rep id }
        in
        [ RemT (as_string id) ]

      let addTs env kvs =
        List.fold_left ~init:[] ~f:(fun rl (k, v) -> addT env k v @ rl) kvs

      let addV env id =
        let _ = Hashtbl.add env.tvars (as_string id) (get_rep id) in
        [ RemV (as_string id) ]

      let addVs env ids =
        List.fold_left ~init:[] ~f:(fun rl id -> addV env id @ rl) ids

      (* Remove the latest binding for the argument. *)
      let remT env id =
        match Hashtbl.find_opt env.tenv (as_string id) with
        | Some v ->
            Hashtbl.remove env.tenv (as_string id);
            [ AddT (as_string id, v) ]
        | None -> []

      (* Remove the latest bindings for the arguments. *)
      let remTs env ks =
        List.fold_left ~init:[] ~f:(fun rl k -> remT env k @ rl) ks

      (* Restore env based on the provided restore list. *)
      let apply_restore env rl =
        List.iter rl ~f:(function
          | AddT (s, rr) -> Hashtbl.add env.tenv s rr
          | RemT s -> Hashtbl.remove env.tenv s
          (* | AddV (s, r) -> Hashtbl.add env.tvars s r *)
          | RemV s -> Hashtbl.remove env.tvars s)

      (* Combine a new list of restores with an older list. *)
      let combine_restores ~older ~newer = newer @ older

      let tvars env =
        Hashtbl.fold (fun key data z -> (key, data) :: z) env.tvars []

      let to_list env =
        Hashtbl.fold (fun key data z -> (key, data) :: z) env.tenv []

      (* Check type for well-formedness in the type environment *)
      let is_wf_type tenv t =
        let rec is_wf_typ' t' tb =
          match t' with
          | MapType (kt, vt) ->
              let%bind () = is_wf_typ' kt tb in
              is_wf_typ' vt tb
          | FunType (at, rt) ->
              let%bind () = is_wf_typ' at tb in
              is_wf_typ' rt tb
          | ADT (n, ts) ->
              let open Datatypes.DataTypeDictionary in
              let%bind adt = lookup_name ~sloc:(get_rep n) (get_id n) in
              if List.length ts <> List.length adt.tparams then
                fail1 ~kind:"ADT type constructor arity mismatch"
                  ~inst:
                    (sprintf "ADT type %s expects %d arguments but got %d"
                       (as_error_string n) (List.length adt.tparams)
                       (List.length ts))
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
                  ~kind:
                    (sprintf "Unbound type variable in type %s" (pp_typ_error t))
                  ~inst:a
          | PolyFun (arg, bt) -> is_wf_typ' bt (arg :: tb)
          | Address AnyAddr | Address CodeAddr | Address LibAddr -> pure ()
          | Address (ContrAddr fts) ->
              foldM (IdLoc_Comp.Map.to_alist fts) ~init:() ~f:(fun _ (_, t) ->
                  is_wf_typ' t tb)
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
        match Hashtbl.find_opt env.tenv (TUName.as_string id) with
        | Some r -> pure r
        | None ->
            let sloc =
              match lopt with Some l -> R.get_loc l | None -> dummy_loc
            in
            fail1 ~kind:"Couldn't resolve identifier"
              ~inst:(TUName.as_error_string id)
              sloc

      let existsT env id = Hashtbl.mem env.tenv (TUName.as_string id)

      let existsV env id = Hashtbl.mem env.tvars (TUName.as_string id)

      let mk () =
        let t1 = Hashtbl.create 50 in
        let t2 = Hashtbl.create 10 in
        let env = { tenv = t1; tvars = t2 } in
        env
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

  let[@warning "-16"] mk_type_error0 ~kind ?inst =
    (TypeError, mk_error0 ~kind ?inst)

  let mk_type_error1 ~kind ?inst loc = (TypeError, mk_error1 ~kind ?inst loc)

  let wrap_error_with_errortype errorType res =
    match res with Ok r -> Ok r | Error e -> Error (errorType, e)

  let mark_error_as_type_error res = wrap_error_with_errortype TypeError res

  (* Some useful data type constructors *)
  let fun_typ t s = FunType (t, s)

  let tvar i = TypeVar i

  let tfun_typ i t = PolyFun (i, t)

  let map_typ k v = MapType (k, v)

  let unit_typ = Unit

  (* Return True if corresponding elements are `type_equiv`,
     False otherwise, or if unequal lengths. *)
  let type_equiv_list ~to_list ~from_list =
    List.length to_list = List.length from_list
    && not
         (List.exists2_exn to_list from_list ~f:(fun t1 t2 ->
              not (type_equivalent t1 t2)))

  let type_assignable_list ~to_list ~from_list =
    List.length to_list = List.length from_list
    && not
         (List.exists2_exn to_list from_list ~f:(fun expected actual ->
              not (type_assignable ~expected ~actual)))

  let[@warning "-16"] assert_type_assignable ?(lc = dummy_loc) ~expected ~actual
      =
    if type_assignable ~expected ~actual then pure ()
    else
      fail1 ~kind:"Type unassignable"
        ~inst:
          (sprintf "%s expected, but %s provided" (pp_typ expected)
             (pp_typ actual))
        lc

  let rec is_ground_type t =
    match t with
    | FunType (a, r) -> is_ground_type a && is_ground_type r
    | MapType (k, v) -> is_ground_type k && is_ground_type v
    | ADT (_, ts) -> List.for_all ~f:(fun t -> is_ground_type t) ts
    | PolyFun _ | TypeVar _ -> false
    | Address _ ->
        (* Addresses are represented as ByStr20 *)
        true
    | _ -> true

  let rec is_legal_type_helper ~allow_maps ~allow_messages_events
      ~allow_closures ~allow_polymorphism ~allow_unit ~check_addresses t
      seen_adts seen_tvars =
    let rec recurser t seen_adts seen_tvars =
      match t with
      | FunType (a, r) ->
          allow_closures
          && recurser a seen_adts seen_tvars
          && recurser r seen_adts seen_tvars
      | PolyFun (tvar, t) ->
          allow_polymorphism && recurser t seen_adts (tvar :: seen_tvars)
      | Unit -> allow_unit
      | MapType (kt, vt) ->
          allow_maps
          && recurser kt seen_adts seen_tvars
          && recurser vt seen_adts seen_tvars
      | TypeVar tvar ->
          (* Type variables can occur legally inside PolyFuns (if polymorphism is allowed) or inside ADTs *)
          List.mem seen_tvars tvar ~equal:String.( = )
      | PrimType _ ->
          (* Messages and Events are considered primitive types *)
          allow_messages_events
          || TUType.(
               (not @@ [%equal: TUType.t] t msg_typ)
               || [%equal: TUType.t] t event_typ)
      | ADT (tname, ts) -> (
          let open DataTypeDictionary in
          if List.mem seen_adts tname ~equal:TUIdentifier.equal then true
            (* Inductive ADT - ignore this branch *)
          else
            (* Check that ADT is serializable *)
            match lookup_name ~sloc:(get_rep tname) (get_id tname) with
            | Error _ -> false (* Handle errors outside *)
            | Ok adt ->
                let adt_serializable =
                  List.for_all adt.tmap ~f:(fun (_, carg_list) ->
                      List.for_all carg_list ~f:(fun carg ->
                          recurser carg (tname :: seen_adts)
                            (adt.tparams @ seen_tvars)))
                in
                adt_serializable
                && List.for_all ts ~f:(fun t -> recurser t seen_adts seen_tvars)
          )
      | Address (ContrAddr fts) when check_addresses ->
          (* If check_addresses is true, then all field types in the address type should be legal field types.
             No need to check for serialisability or storability, since addresses are stored and passed as ByStr20. *)
          IdLoc_Comp.Map.for_all fts ~f:(fun t ->
              is_legal_field_type_helper t seen_adts seen_tvars)
      | Address _ -> true
    in
    recurser t seen_adts seen_tvars

  and is_legal_field_type_helper t seen_adts seen_tvars =
    (* Maps are allowed. Address values should be checked for storable field types. *)
    is_legal_type_helper ~allow_maps:true ~allow_messages_events:false
      ~allow_closures:false ~allow_polymorphism:false ~allow_unit:false
      ~check_addresses:true t seen_adts seen_tvars

  let is_legal_message_field_type t =
    (* Maps are not allowed. Address values are considered ByStr20 when used as message field value. *)
    is_legal_type_helper ~allow_maps:false ~allow_messages_events:false
      ~allow_closures:false ~allow_polymorphism:false ~allow_unit:false
      ~check_addresses:false t [] []

  let is_legal_transition_parameter_type t =
    (* Maps are not allowed. Address values should be checked for storable field types. *)
    is_legal_type_helper ~allow_maps:false ~allow_messages_events:false
      ~allow_closures:false ~allow_polymorphism:false ~allow_unit:false
      ~check_addresses:true t [] []

  let is_legal_procedure_parameter_type t =
    (* Like transition parametes, except messages, events and closures are allowed,
       since parameters do not need to be serializable. *)
    is_legal_type_helper ~allow_maps:false ~allow_messages_events:true
      ~allow_closures:false ~allow_polymorphism:false ~allow_unit:false
      ~check_addresses:true t [] []

  let is_legal_contract_parameter_type t =
    (* Like transitions parameters, except maps are allowed (due to an exploited bug). Address values should be checked for storable field types. *)
    is_legal_type_helper ~allow_maps:true ~allow_messages_events:false
      ~allow_closures:false ~allow_polymorphism:false ~allow_unit:false
      ~check_addresses:true t [] []

  let is_legal_field_type t = is_legal_field_type_helper t [] []

  let is_legal_hash_argument_type t =
    (* Only closures and type closures are disallowed. Addresses behave like ByStr20. *)
    is_legal_type_helper ~allow_maps:true ~allow_messages_events:true
      ~allow_closures:false ~allow_polymorphism:false ~allow_unit:false
      ~check_addresses:false t [] []

  let is_legal_map_key_type t =
    (* Only primitive (non-message and non-event) types are allowed. Addresses behave like ByStr20, and are thus allowed. *)
    is_legal_type_helper ~allow_maps:false ~allow_messages_events:false
      ~allow_closures:false ~allow_polymorphism:false ~allow_unit:false
      ~check_addresses:false t [] []

  let get_msgevnt_type m lc =
    let open ContractUtil.MessagePayload in
    if List.exists m ~f:(fun (x, _, _) -> String.(tag_label = x)) then
      pure TUType.msg_typ
    else if List.exists m ~f:(fun (x, _, _) -> String.(eventname_label = x))
    then pure TUType.event_typ
    else if List.exists m ~f:(fun (x, _, _) -> String.(exception_label = x))
    then pure TUType.exception_typ
    else if
      List.exists m ~f:(fun (x, _, _) -> String.(replicate_contr_label = x))
    then pure TUType.replicate_contr_typ
    else fail1 ~kind:"Invalid message construct." ?inst:None lc

  (* Given a map type and a list of key types, what is the type of the accessed value? *)
  let rec map_access_type mt nindices =
    match (mt, nindices) with
    | _, 0 -> pure mt
    | MapType (_, vt'), 1 -> pure vt'
    | MapType (_, vt'), nkeys' when nkeys' > 1 ->
        map_access_type vt' (nindices - 1)
    | _, _ ->
        fail0 ~kind:"Cannot index into map: Too many index keys." ?inst:None

  (* The depth of a nested map. *)
  let rec map_depth mt =
    match mt with MapType (_, vt) -> 1 + map_depth vt | _ -> 0

  let address_field_type loc f t =
    let preknown_field_type =
      if [%equal: TUName.t] (get_id f) ContractUtil.balance_label then
        Some ContractUtil.balance_type
      else if [%equal: TUName.t] (get_id f) ContractUtil.codehash_label then
        Some ContractUtil.codehash_type
      else None
    in
    let not_declared () =
      fail1 ~kind:"Field is not declared in address type"
        ~inst:
          (sprintf "%s is not declared in %s" (as_error_string f) (pp_typ t))
        loc
    in
    match t with
    | Address AnyAddr
      when [%equal: TUName.t] (get_id f) ContractUtil.balance_label ->
        pure @@ ContractUtil.balance_type
    | (Address LibAddr | Address CodeAddr | Address (ContrAddr _))
      when Option.is_some preknown_field_type ->
        pure @@ Option.value_exn preknown_field_type
    | Address (ContrAddr fts) -> (
        let loc_removed =
          List.map (IdLoc_Comp.Map.to_alist fts) ~f:(fun (f, t) ->
              (get_id f, t))
        in
        match
          List.Assoc.find loc_removed (get_id f) ~equal:[%equal: TUName.t]
        with
        | Some ft -> pure ft
        | None -> not_declared ())
    | _ ->
        fail1 ~kind:"Invalid field read"
          ~inst:(TUIdentifier.as_string f ^ " from " ^ pp_typ t)
          loc

  let pp_typ_list_error ts =
    let tss = List.map ~f:(fun t -> pp_typ_error t) ts in
    sprintf "[%s]" (String.concat ~sep:"; " tss)

  (*
     Check that function type applies for a given arity n
     to a list of argument types.
     Returns the resul type of application or failure
  *)
  let rec fun_type_applies ?(lc = dummy_loc) ft argtypes =
    match (ft, argtypes) with
    | FunType (argt, rest), a :: ats ->
        let%bind () = assert_type_assignable ~expected:argt ~actual:a ~lc in
        fun_type_applies ~lc rest ats
    | FunType (Unit, rest), [] -> pure rest
    | t, [] -> pure t
    | _ ->
        fail1 ~kind:"Ill-typed function application"
          ~inst:
            (sprintf
               "The type\n\
                %s\n\
                doesn't apply, as a function, to the arguments of types\n\
                %s."
               (pp_typ_error ft)
               (pp_typ_list_error argtypes))
          lc

  let proc_type_applies ~lc formals actuals =
    map2M formals actuals
      ~f:(fun expected actual -> assert_type_assignable ~expected ~actual ~lc)
      ~msg:(fun () ->
        mk_error1 ~kind:"Incorrect number of arguments to procedure" ?inst:None
          lc)

  let rec elab_tfun_with_args_no_gas tf args =
    match (tf, args) with
    | (PolyFun _ as pf), a :: args' ->
        let afv = free_tvars a in
        let%bind n, tp =
          match refresh_tfun pf afv with
          | PolyFun (a, b) -> pure (a, b)
          | _ -> Error (mk_error0 ~kind:"This can't happen!" ?inst:None)
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
            (pp_typ_error tf) (pp_typ_list_error args)
        in
        Error (mk_error0 ~kind:msg ?inst:None)

  (****************************************************************)
  (*                        Working with ADTs                     *)
  (****************************************************************)

  let apply_type_subst tmap tp =
    List.fold_left tmap ~init:tp ~f:(fun acc_tp (tv, tp) ->
        subst_type_in_type tv tp acc_tp)

  let validate_param_length ~lc cn plen alen =
    if plen <> alen then
      fail1 ~kind:"Constructor type arguments arity mismatch"
        ~inst:
          (sprintf "%s expects %d type arguments, but got %d."
             (TUName.as_error_string cn)
             plen alen)
        lc
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
  let elab_constr_type ~lc cn targs =
    let open Datatypes.DataTypeDictionary in
    let%bind adt', _ = lookup_constructor cn in
    let seq a b = if String.(a = b) then 0 else 1 in
    let taken =
      List.concat_map targs ~f:free_tvars |> List.dedup_and_sort ~compare:seq
    in
    let adt = refresh_adt adt' taken in
    let plen = List.length adt.tparams in
    let alen = List.length targs in
    let%bind () = validate_param_length ~lc cn plen alen in
    let res_typ = ADT (mk_loc_id adt.tname, targs) in
    match List.Assoc.find adt.tmap cn ~equal:[%equal: TUName.t] with
    | None -> pure res_typ
    | Some ctparams ->
        let tmap = List.zip_exn adt.tparams targs in
        let ctparams_elab = List.map ctparams ~f:(apply_type_subst tmap) in
        let ctyp =
          List.fold_right ctparams_elab ~init:res_typ ~f:(fun ctp acc ->
              fun_typ ctp acc)
        in
        pure ctyp

  let extract_targs ?(lc = dummy_loc) cn (adt : Datatypes.adt) atyp =
    match atyp with
    | ADT (name, targs) ->
        if [%equal: TUName.t] adt.tname (get_id name) then
          let plen = List.length adt.tparams in
          let alen = List.length targs in
          let%bind () = validate_param_length ~lc cn plen alen in
          pure targs
        else
          fail1 ~kind:"Wrong constructor in pattern matching"
            ~inst:
              (sprintf
                 "Expected a constructor of type %s, but a constructor of type \
                  %s is given"
                 (as_string name)
                 (TUName.as_string adt.tname))
            (get_rep name)
    | _ -> fail1 ~kind:"Not an algebraic data type" ~inst:(pp_typ_error atyp) lc

  let constr_pattern_arg_types ?(lc = dummy_loc) atyp cn =
    let open Datatypes.DataTypeDictionary in
    let%bind adt', _ = lookup_constructor cn in
    let taken = free_tvars atyp in
    let adt = refresh_adt adt' taken in
    let%bind targs = extract_targs ~lc cn adt atyp in
    match constr_tmap adt cn with
    | None -> pure []
    | Some tms ->
        let subst = List.zip_exn adt.tparams targs in
        pure @@ List.map ~f:(apply_type_subst subst) tms

  let assert_all_same_type ~lc ts =
    match ts with
    | [] -> fail1 ~kind:"Checking an empty type list" ?inst:None lc
    | t :: ts' -> (
        match List.find ts' ~f:(fun t' -> not ([%equal: TUType.t] t t')) with
        | None -> pure ()
        | Some _ ->
            fail1
              ~kind:"Not all types of pattern matching branches are equivalent"
              ~inst:(pp_typ_list_error ts) lc)

  (****************************************************************)
  (*                     Typing literals                          *)
  (****************************************************************)

  (* Use when the type of l can be determined without dynamic typechecks or additional information.
     For example, the value of an IPC fetch of a field can be trusted, since that the value was constructed in a typesafe way.
     Conversely, the value in a message or an init file cannot be trusted, since that value may have been constructed from outside of Scilla. *)
  let literal_type ?(lc = dummy_loc) l =
    let open TULiteral in
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
    | Msg bs -> get_msgevnt_type bs lc
    | Map ((kt, vt), _) -> pure (MapType (kt, vt))
    | ADTValue (cname, ts, _) ->
        let%bind adt, _ = DataTypeDictionary.lookup_constructor cname in
        pure @@ ADT (mk_loc_id adt.tname, ts)
    | Clo _ -> fail0 ~kind:"Cannot type runtime closure" ?inst:None
    | TAbs _ -> fail0 ~kind:"Cannot type runtime type function" ?inst:None

  (* Use when the type of l must be verified using dynamic typechecks or otherwise. *)
  let assert_literal_type ?(lc = dummy_loc) ~expected l =
    let open TULiteral in
    let rec fun_typ_recurser fun_typ args dyn_checks_acc =
      match (fun_typ, args) with
      | FunType (t, res_t), arg :: rest ->
          let%bind dyn_checks_acc' = recurser t arg dyn_checks_acc in
          fun_typ_recurser res_t rest dyn_checks_acc'
      | ADT (_, _), [] -> pure @@ dyn_checks_acc
      | _, _ -> fail1 ~kind:"Malformed ADT literal" ~inst:(pp_literal l) lc
    and recurser expected l dyn_check_acc =
      match (expected, l) with
      | (Address _ as res_t), ByStrX bs
        when Bystrx.width bs = Type.address_length ->
          (* ByStr20 literal found, address expected. Must be typechecked dynamically. *)
          pure @@ (res_t, bs) :: dyn_check_acc
      | ADT (tname, targs), ADTValue (cname, _, cargs) ->
          let%bind adt, _ = DataTypeDictionary.lookup_constructor cname in
          (* Constructor must belong to ADT *)
          if not @@ [%equal: TUName.t] (get_id tname) adt.tname then
            fail0 ~kind:"Literal constructor does not belong to expected type"
              ~inst:
                (sprintf "%s does not belong to %s"
                   (TUName.as_error_string cname)
                   (TUIdentifier.as_error_string tname))
          else
            (* Elaborate constructor using expected type arguments *)
            let%bind c_fun_typ = elab_constr_type ~lc cname targs in
            (* Traverse constructor function type and check assignability of value arguments *)
            fun_typ_recurser c_fun_typ cargs dyn_check_acc
      | MapType (kt, vt), Map (_, tbl) ->
          (* key/value pairs must be assignable to the expected types *)
          let ks, vs =
            Caml.Hashtbl.fold
              (fun k v (k_acc, v_acc) -> (k :: k_acc, v :: v_acc))
              tbl ([], [])
          in
          let%bind dyn_check_acc' =
            foldM ks ~init:dyn_check_acc ~f:(fun acc k -> recurser kt k acc)
          in
          foldM vs ~init:dyn_check_acc' ~f:(fun acc v -> recurser vt v acc)
      | t, l ->
          (* Simple case - type literal, and check assignability *)
          let%bind lit_t = literal_type ~lc l in
          let%bind () = assert_type_assignable ~expected:t ~actual:lit_t ~lc in
          pure dyn_check_acc
    in
    recurser expected l []

  (* Verifies a literal to be wellformed and returns it's type. *)
  let rec is_wellformed_lit ?(lc = dummy_loc) l =
    let open TULiteral in
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
    | ByStrX bsx ->
        (* ByStr20 literals may represent addresses, but only statically.
           Dynamically, ByStr20 are considered ByStr20, and are checked for
           correct address contents only at certain points. *)
        pure (bystrx_typ (Bystrx.width bsx))
    (* Check that messages and events have legal parameters. *)
    | Msg m ->
        let%bind msg_typ = get_msgevnt_type m lc in
        let%bind all_legal =
          foldM
            ~f:(fun acc (n, t, l) ->
              let%bind t' = is_wellformed_lit l in
              if not @@ [%equal: TUType.t] t t' then
                fail0
                  ~kind:
                    "Message/Event has inconsistent values and types at field"
                  ~inst:n
              else if acc then pure (is_legal_message_field_type t)
              else pure false)
            ~init:true m
        in
        if not all_legal then
          fail0 ~kind:"Message/Event has invalid / non-storable parameters"
            ?inst:None
        else pure msg_typ
    | Map ((kt, vt), kv) ->
        if is_legal_map_key_type kt then
          (* Verify that all key/vals conform to kt,vt, recursively. *)
          let%bind valid =
            Caml.Hashtbl.fold
              (fun k v res' ->
                let%bind res = res' in
                if not res then pure @@ res
                else
                  let%bind kt' = is_wellformed_lit k in
                  let%bind vt' = is_wellformed_lit v in
                  pure
                  @@ (type_assignable ~expected:kt ~actual:kt'
                     && type_assignable ~expected:vt ~actual:vt'))
              kv (pure true)
          in
          if not valid then fail0 ~kind:"Malformed literal" ~inst:(pp_literal l)
            (* We have a valid Map literal. *)
          else pure (MapType (kt, vt))
        else fail0 ~kind:"Not a primitive map key type" ~inst:(pp_typ_error kt)
    | ADTValue (cname, ts, args) ->
        let%bind adt, constr = DataTypeDictionary.lookup_constructor cname in
        let tparams = adt.tparams in
        let tname = adt.tname in
        if not (List.length tparams = List.length ts) then
          fail0 ~kind:"Type parameters arity mismatch for ADT constructor"
            ~inst:
              (sprintf
                 "Wrong number of type parameters for ADT %s (%i) in \
                  constructor %s."
                 (TUName.as_error_string tname)
                 (List.length ts)
                 (TUName.as_error_string cname))
        else if not (List.length args = constr.arity) then
          fail0 ~kind:"Arity mismatch for ADT constructor"
            ~inst:
              (sprintf
                 "Wrong number of arguments to ADT %s (%i) in constructor %s."
                 (TUName.as_error_string tname)
                 (List.length args)
                 (TUName.as_error_string cname))
          (* Verify that the types of args match that declared. *)
        else
          let res = ADT (mk_loc_id tname, ts) in
          let%bind tmap = constr_pattern_arg_types res cname in
          let%bind arg_typs = mapM ~f:(fun l -> is_wellformed_lit l) args in
          let args_valid =
            List.for_all2_exn tmap arg_typs ~f:(fun expected actual ->
                type_assignable ~expected ~actual)
          in
          if not args_valid then
            fail0 ~kind:"Malformed ADT. Arguments do not match expected types"
              ~inst:(pp_literal l)
          else pure @@ res
    | Clo _ -> fail0 ~kind:"Cannot type-check runtime closure" ?inst:None
    | TAbs _ -> fail0 ~kind:"Cannot type-check runtime type function" ?inst:None
end
