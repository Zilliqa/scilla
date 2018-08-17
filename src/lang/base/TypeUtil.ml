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
open Result.Let_syntax
open MonadUtil
open Datatypes

(****************************************************************)
(*                  Type substitutions                          *)
(****************************************************************)

(* Return free tvars in tp *)
let free_tvars tp =
  let add vs tv = tv :: List.filter ~f:(fun v -> v = tv) vs in
  let rem vs tv = List.filter ~f:(fun v -> v <> tv) vs in
  let rec go t acc = (match t with
  | PrimType _ -> acc
  | MapType (kt, vt) -> go kt acc |> go vt
  | FunType (at, rt) -> go at acc |> go rt
  | TypeVar n -> add acc n
  | ADT (_, ts) ->
      List.fold_left ts ~init:acc ~f:(fun z tt -> go tt z)
  | PolyFun (arg, bt) ->
      let acc' = go bt acc in
      rem acc' arg) in
  go tp []

let mk_fresh_var taken init =
  let tmp = ref init in
  let counter = ref 1 in
  while List.mem taken !tmp ~equal:(fun a b -> a = b) do
    let cnt = !counter in
    tmp := init ^ (Int.to_string cnt);
    counter := cnt + 1;
  done;
  !tmp


let rec subst_type_in_type tvar tp tm = match tm with
  | PrimType _ as p -> p
  (* Make sure the map's type is still primitive! *)
  | MapType (kt, vt) -> 
      let kts = subst_type_in_type tvar tp kt in
      let vts = subst_type_in_type tvar tp vt in
      MapType (kts, vts)
  | FunType (at, rt) -> 
      let ats = subst_type_in_type tvar tp at in
      let rts = subst_type_in_type tvar tp rt in
      FunType (ats, rts)
  | TypeVar n as tv ->
      if tvar = n then tp else tv
  | ADT (s, ts) ->
      let ts' = List.map ts ~f:(fun t -> subst_type_in_type tvar tp t) in
      ADT (s, ts')
  | PolyFun (arg, t) as pf -> 
      if tvar = arg then pf
      else PolyFun (arg, subst_type_in_type tvar tp t)

let subst_types_in_type sbst tm =
  List.fold_left sbst ~init:tm
    ~f:(fun acc (tvar, tp) -> subst_type_in_type tvar tp acc)

let rec refresh_tfun t taken = match t with
  | MapType (kt, vt) -> MapType (kt, refresh_tfun vt taken)
  | FunType (at, rt) ->
      FunType (refresh_tfun at taken, refresh_tfun rt taken)
  | ADT (n, ts) ->
      let ts' = List.map ts ~f:(fun w -> refresh_tfun w taken) in
      ADT (n, ts')
  | PrimType _ | TypeVar _ -> t
  | PolyFun (arg, bt) ->
      let arg' = mk_fresh_var taken arg in
      let tv_new = TypeVar arg' in
      let bt1 = subst_type_in_type arg tv_new bt in
      let taken' = arg' :: taken in
      let bt2 = refresh_tfun bt1 taken' in
      PolyFun (arg', bt2)

(* Alpha renaming to canonical (pre-determined) names. *)
let canonicalize_tfun t =
  let taken = free_tvars t in
  let get_new_name counter = "'_A" ^ Int.to_string counter in
  let rec refresh t taken counter = match t with
    | MapType (kt, vt) -> MapType (kt, refresh vt taken counter)
    | FunType (at, rt) ->
        FunType (refresh at taken counter, refresh rt taken counter)
    | ADT (n, ts) ->
        let ts' = List.map ts ~f:(fun w -> refresh w taken counter) in
        ADT (n, ts')
    | PrimType _ | TypeVar _ -> t
    | PolyFun (arg, bt) ->
        let arg' = get_new_name counter in
        let tv_new = TypeVar arg' in
        let bt1 = subst_type_in_type arg tv_new bt in
        let taken' = arg' :: taken in
        let bt2 = refresh bt1 taken' (counter+1) in
        PolyFun (arg', bt2)
  in
    refresh t taken 1

(* The same as above, but for a variable with locations *)
let subst_type_in_type' tv = subst_type_in_type (get_id tv)

let rec subst_type_in_literal tvar tp l = match l with
  | Map ((kt, vt), ls) -> 
      let kts = subst_type_in_type' tvar tp kt in
      let vts = subst_type_in_type' tvar tp vt in
      let ls' = List.map ls ~f:(fun (k, v) ->
        let k' = subst_type_in_literal tvar tp k in
        let v' = subst_type_in_literal tvar tp v in 
        (k', v')) in
      Map ((kts, vts), ls')
  | ADTValue (n, ts, ls) ->
      let ts' = List.map ts ~f:(fun t -> subst_type_in_type' tvar tp t) in
      let ls' = List.map ls ~f:(fun l -> subst_type_in_literal tvar tp l) in
      ADTValue (n, ts', ls')
  | _ -> l


(* Substitute type for a type variable *)
let rec subst_type_in_expr tvar tp e = match e with
  | Literal l -> Literal (subst_type_in_literal tvar tp l)
  | Var _ as v -> v
  | Fun (f, t, body) ->
      let t_subst = subst_type_in_type' tvar tp t in 
      let body_subst = subst_type_in_expr tvar tp body in
      Fun (f, t_subst, body_subst)
  | TFun (tv, body) as tf ->
      if get_id tv = get_id tvar
      then tf
      else 
        let body_subst = subst_type_in_expr tvar tp body in
        TFun (tv, body_subst)
  | Constr (n, ts, es) ->
      let ts' = List.map ts ~f:(fun t -> subst_type_in_type' tvar tp t) in
      Constr (n, ts', es)
  | App _ as app -> app
  | Builtin _ as bi -> bi
  | Let (i, tann, lhs, rhs) ->
      let tann' = Option.map tann ~f:(fun t -> subst_type_in_type' tvar tp t) in
      let lhs' = subst_type_in_expr tvar tp lhs in
      let rhs' = subst_type_in_expr tvar tp rhs in
      Let (i, tann', lhs', rhs')
  | Message _ as m -> m
  | MatchExpr (e, cs) ->
      let cs' = List.map cs ~f:(fun (p, b) -> (p, subst_type_in_expr tvar tp b)) in
      MatchExpr(e, cs')
  | TApp (tf, tl) -> 
      let tl' = List.map tl ~f:(fun t -> subst_type_in_type' tvar tp t) in
      TApp (tf, tl')
  | Fixpoint (f, t, body) ->
      let t' = subst_type_in_type' tvar tp t in
      let body' = subst_type_in_expr tvar tp body in
      Fixpoint (f, t', body')

(****************************************************************)
(*                Inferred types and qualifiers                 *)
(****************************************************************)

type 'rep inferred_type = {
  tp   : typ;
  qual : 'rep
} [@@deriving sexp]

module type QualifiedTypes = sig
  type t
  val mk_qualified_type : typ -> t inferred_type      
end

module type MakeTEnvFunctor = functor (Q: QualifiedTypes) -> sig
  (* Resolving results *)
  type resolve_result
  val rr_loc : resolve_result -> loc
  val rr_typ : resolve_result -> Q.t inferred_type
  val rr_pp  : resolve_result -> string
  val mk_qual_tp : typ -> Q.t inferred_type
  
  module TEnv : sig
    type t
    (* Make new type environment *)
    val mk : t
    (* Add to type environment *)
    val addT : t -> loc ident -> typ -> t
    (* Add to many type bindings *)
    val addTs : t -> (loc ident * typ) list -> t      
    (* Add type variable to the environment *)
    val addV : t -> loc ident -> t
    (* Check type for well-formedness in the type environment *)
    val is_wf_type : t -> typ -> (unit, string) result
    (* Resolve the identifier *)
    val resolveT : 
      ?lopt:(loc option) -> t -> string -> (resolve_result, string) result
    (* Copy the environment *)
    val copy : t -> t
    (* Convert to list *)
    val to_list : t -> (string * resolve_result) list
    (* Get type variables *)
    val tvars : t -> (string * loc) list
    (* Print the type environment *)
    val pp : ?f:(string * resolve_result -> bool) -> t -> string        
  end
end

(****************************************************************)
(*                   Typing environments                        *)
(****************************************************************)

(* Typing environment, parameterised by a qualifier *)
module MakeTEnv: MakeTEnvFunctor = functor (Q: QualifiedTypes) -> struct
  type resolve_result = {qt : Q.t inferred_type; loc : loc }
  let rr_loc rr = rr.loc
  let rr_typ rr = rr.qt
  (*  TODO: Also print location *)
  let rr_pp  rr = (rr_typ rr).tp |> pp_typ
  let mk_qual_tp tp =  Q.mk_qualified_type tp
      
  module TEnv = struct
    type t = {
      (* Typed identifiers *)
      tenv  : (string, resolve_result) Hashtbl.t;
      (* Context for type variables and their locations *)
      tvars : (string, loc) Hashtbl.t
    } 
    
    let addT env id tp =
      let _ = Hashtbl.add env.tenv (get_id id)
          {qt = Q.mk_qualified_type tp; loc = get_loc id} in
      env
        
    let addTs env kvs = 
      List.fold_left ~init:env ~f:(fun z (k, v) -> addT z k v) kvs
        
    let addV env id = 
      let _ = Hashtbl.add env.tvars (get_id id) (get_loc id) in env
        
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
      | PrimType _  -> pure ()
      | TypeVar a ->
        (* Check if bound locally. *)
        if List.mem tb a ~equal:(fun a b -> a = b) then pure ()
        (* Check if bound in environment. *)
        else if List.mem (tvars tenv) (a, dummy_loc) ~equal:(fun x y -> fst x = fst y) then pure()
        else fail @@ sprintf "Unbound type variable %s in type %s" a (pp_typ t)
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
              | Some l -> sprintf "[%s]: " (get_loc_str l)
              | None -> "") in
          fail @@ sprintf
            "%sCouldn't resolve the identifier \"%s\" in the type environment\n%s"
            loc_str id (pp env)

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


(****************************************************************)
(*               Specific instantiations                        *)
(****************************************************************)

module PlainTypes : QualifiedTypes = struct
  type t = unit
  let mk_qualified_type t = {tp = t; qual = ()}
end

(* Some useful data type constructors *)
let fun_typ t s = FunType (t, s)
let tvar i = TypeVar i
let tfun_typ i t = PolyFun (i, t)
let map_typ k v = MapType (k, v)

(****************************************************************)
(*             Utility function for matching types              *)
(****************************************************************)

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
        
let extract_targs cn adt atyp = match atyp with
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
  | Address _ -> pure address_typ
  | Sha256 _ -> pure hash_typ
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

(****************************************************************)
(*                  Better error reporting                      *)
(****************************************************************)

let get_failure_msg e opt = match e with
  | App (f, _) ->
      sprintf "[%s] Type error in application of `%s`:\n"
        (get_loc_str (get_loc f)) (get_id f)
  | Let (i, _, _, _) ->
      sprintf "[%s] Type error in the initialiser of `%s`:\n"
        (get_loc_str (get_loc i)) (get_id i)
  | MatchExpr (x, _) ->
      sprintf
      "[%s] Type error in pattern matching on `%s`%s (or one of its branches):\n"
      (get_loc_str (get_loc x)) (get_id x) opt 
  | TApp (tf, _) ->
      sprintf "[%s] Type error in type application of `%s`:\n"
        (get_loc_str (get_loc tf)) (get_id tf)
  | Builtin (i, _) ->
      sprintf "[%s] Type error in built-in application of `%s`:\n"
        (get_loc_str (get_loc i)) (get_id i)
  | Fixpoint (f, _, _) ->
      sprintf "Type error in fixpoint application with an argument `%s`:\n"
        (get_id f)              
  | _ -> ""

let get_failure_msg_stmt s opt = match s with
  | Load (x, f) ->
      sprintf "[%s] Type error in reading value of `%s` into `%s`:\n"
        (get_loc_str (get_loc x)) (get_id f) (get_id x)
  | Store (f, r) ->
      sprintf "[%s] Type error in storing value of `%s` into the field `%s`:\n"
        (get_loc_str (get_loc f)) (get_id r) (get_id f)
  | Bind (x, _) ->
      sprintf "[%s] Type error in the binding to into `%s`:\n"
        (get_loc_str (get_loc x)) (get_id x)
  | ReadFromBC (x, _) ->
      sprintf "[%s] Error in reading from blockchain state into `%s`:\n"
        (get_loc_str (get_loc x)) (get_id x)
  | MatchStmt (x, _) ->
      sprintf
      "[%s] Type error in pattern matching on `%s`%s (or one of its branches):\n"
      (get_loc_str (get_loc x)) (get_id x) opt 
  | SendMsgs i ->
      sprintf "[%s] Error in sending messages `%s`:\n"
        (get_loc_str (get_loc i)) (get_id i)
  | _ -> ""


let wrap_with_info msg res = match res with
  | Ok _ -> res
  | Error msg' -> Error (sprintf "%s%s" msg msg')

let wrap_err e ?opt:(opt = "") = wrap_with_info (get_failure_msg e opt)

let wrap_serr s ?opt:(opt = "") =
  wrap_with_info (get_failure_msg_stmt s opt)

(*****************************************************************)
(*               Blockchain component typing                     *)
(*****************************************************************)

let blocknum_name = "BLOCKNUMBER"

(*****************************************************************)
(*                    Transition typing                          *)
(*****************************************************************)

