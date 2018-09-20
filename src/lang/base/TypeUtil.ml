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

open Core
open Sexplib.Std
open Syntax
open MonadUtil
open Result.Let_syntax
open Datatypes
open PrettyPrinters

(****************************************************************)
(*                Inferred types and qualifiers                 *)
(****************************************************************)

type 'rep inferred_type = {
  tp   : typ;
  qual : 'rep
} [@@deriving sexp]

module type QualifiedTypes = sig
  type t
  val t_of_sexp : Sexp.t -> t
  val sexp_of_t : t -> Sexp.t
  val mk_qualified_type : typ -> t inferred_type      
end

module type MakeTEnvFunctor = functor
  (Q : QualifiedTypes)
  (R : Rep)
  -> sig

    (* Resolving results *)
    type resolve_result
    val rr_loc : resolve_result -> loc
    val rr_rep : resolve_result -> R.rep
    val rr_typ : resolve_result -> Q.t inferred_type
    val rr_pp  : resolve_result -> string
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
      (* Check type for well-formedness in the type environment *)
      val is_wf_type : t -> typ -> (unit, string) result
      (* Resolve the identifier *)
      val resolveT : 
        ?lopt:(R.rep option) -> t -> string -> (resolve_result, string) result
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
module MakeTEnv: MakeTEnvFunctor = functor
  (Q : QualifiedTypes)
  (R : Rep)
  -> struct
    type resolve_result = {qt : Q.t inferred_type; rep : R.rep }
    let rr_loc rr = R.get_loc (rr.rep)
    let rr_rep rr = rr.rep
    let rr_typ rr = rr.qt
    (*  TODO: Also print rep *)
    let rr_pp  rr = (rr_typ rr).tp |> pp_typ
    let mk_qual_tp tp =  Q.mk_qualified_type tp

    module TEnv = struct
      type t = {
        (* Typed identifiers *)
        tenv  : (string, resolve_result) Hashtbl.t;
        (* Context for type variables and their rep *)
        tvars : (string, R.rep) Hashtbl.t
      } 

      let addT env id tp =
        let _ = Hashtbl.add env.tenv (get_id id)
            {qt = Q.mk_qualified_type tp; rep = get_rep id} in
        env

      let addTs env kvs = 
        List.fold_left ~init:env ~f:(fun z (k, v) -> addT z k v) kvs

      let addV env id = 
        let _ = Hashtbl.add env.tvars (get_id id) (get_rep id) in env

      let tvars env =
        Hashtbl.fold (fun key data z -> (key, data) :: z) env.tvars []

      let to_list env =
        Hashtbl.fold (fun key data z -> (key, data) :: z) env.tenv []

      (* Check type for well-formedness in the type environment *)
      let is_wf_type tenv t =
        let rec is_wf_typ' t' tb = match t' with
          | MapType (kt, vt) ->
              let%bind _ = is_wf_typ' kt tb in
              is_wf_typ' vt tb
          | FunType (at, rt) ->
              let%bind _ = is_wf_typ' at tb in
              is_wf_typ' rt tb
          | ADT (n, ts) ->
              let open Datatypes.DataTypeDictionary in
              let%bind adt = lookup_name n in
              if List.length ts <> List.length adt.tparams then
                fail @@ sprintf "ADT type %s expects %d arguments but got %d.\n" 
                  n (List.length adt.tparams) (List.length ts) 
              else
                foldM ~f:(fun _ ts' -> is_wf_typ' ts' tb) ~init:(()) ts
          | PrimType _  | Unit -> pure ()
          | TypeVar a ->
              (* Check if bound locally. *)
              if List.mem tb a ~equal:(fun a b -> a = b) then pure ()
              (* Check if bound in environment. *)
              else
                (match List.findi (tvars tenv) ~f:(fun _ (x, _) -> x = a) with
                 | Some _ -> pure()
                 | None -> fail @@ sprintf "Unbound type variable %s in type %s" a (pp_typ t))
          | PolyFun (arg, bt) -> is_wf_typ' bt (arg::tb)
        in
        is_wf_typ' t []

      (* TODO: Add support for tvars *)    
      let pp ?f:(f = fun _ -> true) env  =
        let lst = List.filter (to_list env) ~f:f in
        let ps = List.map lst
            ~f:(fun (k, v) -> " [" ^ k ^ " : " ^ (rr_pp v) ^ "]") in
        let cs = String.concat ~sep:",\n " ps in
        "{" ^ cs ^ " }"

      let resolveT ?lopt:(lopt = None) env id =
        match Hashtbl.find_opt env.tenv id with
        | Some r -> pure r
        | None ->
            let loc_str = (match lopt with
                | Some l -> sprintf "[%s]: " (get_loc_str (R.get_loc l))
                | None -> "") in
            fail @@ sprintf
              "%sCouldn't resolve the identifier \"%s\".\n"
              loc_str id

      let copy e = {
        tenv = Hashtbl.copy e.tenv;
        tvars = Hashtbl.copy e.tvars
      }

      let mk =
        let t1 = Hashtbl.create 50 in
        let t2 = Hashtbl.create 10 in
        let env = {tenv = t1; tvars = t2} in
        copy env

    end
  end

(***************************************************************)
(*               Specific instantiations                        *)
(****************************************************************)

module PlainTypes : QualifiedTypes = struct
  type t = unit
  [@@deriving sexp]

  let mk_qualified_type t = {tp = t; qual = ()}
end

(****************************************************************)
(*             Utility function for matching types              *)
(****************************************************************)

module TypeUtilities
    (SR : Rep)
    (ER : Rep) = struct

  module MakeTEnv = MakeTEnv

  module TSSyntax = ScillaSyntax (SR) (ER)
  open TSSyntax
     
  (* Some useful data type constructors *)
  let fun_typ t s = FunType (t, s)
  let tvar i = TypeVar i
  let tfun_typ i t = PolyFun (i, t)
  let map_typ k v = MapType (k, v)
  let unit_typ = Unit

  (* Type equivalence *)
  let type_equiv t1 t2 =
    let t1' = canonicalize_tfun t1 in
    let t2' = canonicalize_tfun t2 in
    t1' = t2'

  (* Return True if corresponding elements are `type_equiv`,
     False otherwise, or if unequal lengths. *)
  let type_equiv_list tlist1 tlist2 =
    List.length tlist1 = List.length tlist2 &&
    not (List.exists2_exn tlist1 tlist2 ~f:(fun t1 t2 -> not (type_equiv t1 t2)))

  let assert_type_equiv expected given =
    if type_equiv expected given
    then pure ()
    else fail @@ sprintf
        "Type mismatch: %s expected, but %s provided."
        (pp_typ expected) (pp_typ given)

  let rec is_ground_type t = match t with 
    | FunType (a, r) -> is_ground_type a && is_ground_type r
    | MapType (k, v) -> is_ground_type k && is_ground_type v
    | ADT (_, ts) -> List.for_all ~f:(fun t -> is_ground_type t) ts
    | PolyFun _ -> false
    | _ -> true

  let rec is_storable_type t = match t with 
    | FunType _ -> false
    | MapType (kt, vt) -> is_storable_type kt && is_storable_type vt
    | TypeVar _ -> false
    | ADT (_, ts) -> List.for_all ~f:(fun t -> is_storable_type t) ts
    | PolyFun _ -> false
    | Unit -> false
    | _ -> true


  let pp_typ_list ts =
    let tss = List.map ~f:(fun t -> pp_typ t) ts in
    sprintf "[%s]" (String.concat ~sep:"; " tss)

  (* 
   Check that function type applies for a given arity n 
   to a list of argument types. 
   Returns the resul type of application or failure 
*)
  let rec fun_type_applies ft argtypes = match ft, argtypes with
    | FunType (argt, rest), a :: ats ->
        let%bind _ = assert_type_equiv argt a in
        fun_type_applies rest ats
    | FunType (argt, rest), [] when argt = Unit -> pure rest
    | t, []  -> pure t
    | _ -> fail @@ sprintf
          "The type\n%s\ndoesn't apply, as a function, to the arguments of types\n%s." (pp_typ ft)
          (pp_typ_list argtypes)

  let rec elab_tfun_with_args tf args = match tf, args with
    | PolyFun _ as pf, a :: args' ->
        let afv = free_tvars a in
        let%bind (n, tp) = (match refresh_tfun pf afv with
            | PolyFun (a, b) -> pure (a, b)
            | _ -> fail "This can't happen!") in
        let tp' = subst_type_in_type n a tp in
        elab_tfun_with_args tp' args'
    | t, [] -> pure t
    | _ ->
        let msg = sprintf
            "Cannot elaborate expression of type\n%s\napplied, as a type function, to type arguments\n%s." (pp_typ tf)
            (pp_typ_list args) in
        fail msg

  (****************************************************************)
  (*                        Working with ADTs                     *)
  (****************************************************************)

  let apply_type_subst tmap tp =
    List.fold_left tmap ~init:tp
      ~f:(fun acc_tp (tv, tp) -> subst_type_in_type tv tp acc_tp)

  let validate_param_length cn plen alen =
    if plen <> alen
    then fail @@ sprintf
        "Constructor %s expects %d type arguments, but got %d." cn plen alen
    else pure ()

  (* Avoid variable clashes *)
  let refresh_adt adt taken =
    let {tparams; tmap; _} = adt in
    let tkn = tparams @ taken in
    let subst = List.map tparams ~f:(fun tp ->
        (tp, mk_fresh_var tkn tp)) in
    let tparams' = List.unzip subst |> snd  in
    let subst =
      List.zip_exn tparams @@
      List.map tparams' ~f:(fun s -> TypeVar s) in 
    let tmap' = List.map tmap ~f:(fun (cn, tls) ->
        let tls' = List.map tls ~f:(fun t -> subst_types_in_type subst t)
        in (cn, tls'))
    in {adt with tparams = tparams'; tmap = tmap'}

  (*  Get elaborated constructor type *)    
  let elab_constr_type cn targs =
    let open Datatypes.DataTypeDictionary in
    let%bind (adt', _) = lookup_constructor cn in
    let seq a b = if a = b then 0 else 1 in
    let taken = List.map targs ~f:free_tvars |>
                List.concat |>
                List.dedup_and_sort ~compare:seq in
    let adt = refresh_adt adt' taken in
    let plen = List.length adt.tparams in
    let alen = List.length targs in
    let%bind _ = validate_param_length cn plen alen in
    let res_typ = ADT (adt.tname, targs) in
    match List.find adt.tmap ~f:(fun (n, _) -> n = cn) with
    | None -> pure res_typ
    | Some (_, ctparams) ->
        let tmap = List.zip_exn adt.tparams targs in
        let ctparams_elab = List.map ctparams ~f:(apply_type_subst tmap) in
        let ctyp = List.fold_right ctparams_elab ~init:res_typ
            ~f:(fun ctp acc -> fun_typ ctp acc) in
        pure ctyp

  let extract_targs cn (adt : Datatypes.adt) atyp = match atyp with
    | ADT (name, targs) ->
        if adt.tname = name
        then
          let plen = List.length adt.tparams in
          let alen = List.length targs in        
          let%bind _ = validate_param_length cn plen alen in
          pure targs
        else fail @@ sprintf
            "Types don't match: pattern uses a constructor of type %s, but value of type %s is given."
            adt.tname name
    | _ -> fail @@ sprintf
          "Not an algebraic data type: %s" (pp_typ atyp)

  let constr_pattern_arg_types atyp cn =
    let open Datatypes.DataTypeDictionary in
    let%bind (adt', _) = lookup_constructor cn in
    let taken = free_tvars atyp in
    let adt = refresh_adt adt' taken in
    let%bind targs = extract_targs cn adt atyp in
    match constr_tmap adt cn with
    | None -> pure []
    | Some tms ->
        let subst = List.zip_exn adt.tparams targs in
        pure @@ List.map ~f:(apply_type_subst subst) tms 

  let assert_all_same_type ts = match ts with
    | [] -> fail "Checking an empty type list."
    | t :: ts' ->
        match List.find ts' ~f:(fun t' -> not (type_equiv t t')) with
        | None -> pure ()
        | Some _ -> fail @@ sprintf
              "Not all types of the branches %s are equivalent." (pp_typ_list ts)

  (****************************************************************)
  (*                     Typing literals                          *)
  (****************************************************************)

  let rec literal_type l =
    let open PrimTypes in 
    match l with
    | IntLit (32, _) -> pure int32_typ
    | IntLit (64, _) -> pure int64_typ
    | IntLit (128, _) -> pure int128_typ
    | IntLit (256, _) -> pure int256_typ
    | UintLit (32, _) -> pure uint32_typ
    | UintLit (64, _) -> pure uint64_typ
    | UintLit (128, _) -> pure uint128_typ
    | UintLit (256, _) -> pure uint256_typ
    | IntLit(w, _) ->
        fail @@ (sprintf "Wrong bit depth for integer: %i." w)
    | UintLit(w, _) ->
        fail @@ (sprintf "Wrong bit depth for unsigned integer: %i." w)
    | StringLit _ -> pure string_typ
    | BNum _ -> pure bnum_typ
    | ByStr _ -> 
      if validate_bystr_literal l
      then pure bystr_typ
      else fail @@ (sprintf "Malformed byte string " ^ (pp_literal l))
    | ByStrX (b, _) ->
        if validate_bystrx_literal l
        then pure (bystrx_typ b)
        else fail @@ (sprintf "Malformed byte string " ^ (pp_literal l))
    (* Check that messages and events have storable parameters. *)
    | Msg m -> 
        let%bind all_storable = foldM ~f:(fun acc (_, l) ->
            let%bind t = literal_type l in
            if acc then pure (is_storable_type t) else pure false)
            ~init:true m
        in
        if not all_storable then
          fail @@ sprintf "Message/Event has invalid / non-storable parameters"
        else pure msg_typ
    | Map ((kt, vt), kv) ->
        if PrimTypes.is_prim_type kt
        then 
          (* Verify that all key/vals conform to kt,vt, recursively. *)
          let%bind valid = foldM ~f:(fun res (k, v) ->
              if not res then pure @@ res else
                let%bind kt' = literal_type k in
                let%bind vt' = literal_type v in
                pure @@
                ((type_equiv kt kt') && (type_equiv vt vt'))
            ) ~init:true kv in
          if not valid then fail @@ (sprintf "Malformed literal %s" (pp_literal l))
          (* We have a valid Map literal. *)
          else pure (MapType (kt, vt))
        else if kv = [] && (match kt with | TypeVar _ -> true | _ -> false) then
          (* we make an exception for Emp as it may be parameterized. *)
          pure (MapType (kt, vt))
        else
          fail @@
          (sprintf "Not a primitive map key type: %s." (pp_typ kt))
    | ADTValue (cname, ts, args) ->
        let%bind (adt, constr) = DataTypeDictionary.lookup_constructor cname in
        let tparams = adt.tparams in
        let tname = adt.tname in
        if not (List.length tparams = List.length ts)
        then fail @@
          sprintf "Wrong number of type parameters for ADT %s (%i) in constructor %s."
            tname (List.length ts) cname
        else if not (List.length args = constr.arity)
        then fail @@
          sprintf "Wrong number of arguments to ADT %s (%i) in constructor %s."
            tname (List.length args) cname
            (* Verify that the types of args match that declared. *)
        else
          let res = ADT(tname, ts) in
          let%bind tmap = constr_pattern_arg_types res cname in
          let%bind arg_typs = mapM ~f:(fun l -> literal_type l) args in
          let args_valid = List.for_all2_exn tmap arg_typs 
              ~f:(fun t1 t2 -> type_equiv t1 t2) in
          if not args_valid
          then fail @@ sprintf "Malformed ADT %s. Arguments do not match expected types" (pp_literal l)
          else pure @@ res
end

(*****************************************************************)
(*               Blockchain component typing                     *)
(*****************************************************************)

let blocknum_name = "BLOCKNUMBER"

