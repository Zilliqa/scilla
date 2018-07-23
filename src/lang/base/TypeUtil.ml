(*
 * Copyright (c) 2018 - present. 
 * Zilliqa, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open Core
open Sexplib.Std
open Yojson
open Big_int
open Syntax
open Result.Let_syntax
open MonadUtil
open Big_int
open Stdint
open Datatypes

(****************************************************************)
(*                  Type substitutions                          *)
(****************************************************************)

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

(* The same as above, but for a variable with locations *)
let subst_type_in_type' tv = subst_type_in_type (get_id tv)

let rec subst_type_in_literal tvar tp l = match l with
  | Map ((kt, vt), ls) -> 
      let kts = subst_type_in_type' tvar tp kt in
      let vts = subst_type_in_type' tvar tp vt in
      let ls' = List.map ls (fun (k, v) ->
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
      let tl' = List.map tl (fun t -> subst_type_in_type' tvar tp t) in
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
    
    let mk =
      let t1 = Hashtbl.create 50 in
      let t2 = Hashtbl.create 10 in
      {tenv = t1; tvars = t2}
      
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
        
    (* TODO: Add support for tvars *)    
    let pp ?f:(f = fun _ -> true) env  =
      let lst = List.filter (to_list env) ~f:f in
      let ps = List.map lst
          ~f:(fun (k, v) -> " [" ^ k ^ " -> " ^ (rr_pp v) ^ "]") in
      let cs = String.concat ~sep:",\n " ps in
      "{" ^ cs ^ " }"

    let resolveT ?lopt:(lopt = None) env id =
      match Hashtbl.find_opt env.tenv id with
      | Some r -> pure r
      | None ->
          let loc_str = (match lopt with
              | Some l -> sprintf " at a location [%s]" (get_loc_str l)
              | None -> "") in
          fail @@ sprintf
            "Couldn't resolve the identifier %s%s in the type environment\n%s"
            id loc_str (pp env)

    let copy e = {
      tenv = Hashtbl.copy e.tenv;
      tvars = Hashtbl.copy e.tvars
    }

  end
end


(****************************************************************)
(*               Specific instantiations                        *)
(****************************************************************)

module PlainTypes : QualifiedTypes = struct
  type t = unit
  let mk_qualified_type t = {tp = t; qual = ()}
end

(****************************************************************)
(*                     Typing literals                          *)
(****************************************************************)

let literal_type l =
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
  | Msg _ -> pure msg_typ
  (* TODO: Add structural type checking *)
  | Map ((kt, vt), _) ->
      if PrimTypes.is_prim_type kt
      then pure (MapType (kt, vt))
      else fail @@
        (sprintf "Not a primitive map key tpye: %s." (pp_typ kt))        
  (* TODO: Add structural type checking *)
  | ADTValue (cname, ts, _) ->
      let%bind (adt, _) = DataTypeDictionary.lookup_constructor cname in
      let tparams = adt.targs in
      let tname = adt.tname in
      if not (List.length tparams = List.length ts)
      then fail @@
        sprintf "Wrong number of type parameters for ADT %s (%i) in constructor %s."
          tname (List.length ts) cname
      else
        pure @@ ADT (tname, ts)

(* Some useful data type constructors *)
let fun_typ t s = FunType (t, s)
let tvar i = TypeVar i
let tfun_typ i t = PolyFun (i, t)
let map_typ k v = MapType (k, v)

(****************************************************************)
(*             Utility function for matching types              *)
(****************************************************************)

(* Type equivalence *)
(* TODO: Generalise for up to alpha renaming of TVars *)
let type_equiv t1 t2 =
  t1 = t2

let rec is_ground_type t = match t with 
  | FunType (a, r) -> is_ground_type a && is_ground_type r
  | MapType (k, v) -> is_ground_type k && is_ground_type v
  | ADT (_, ts) -> List.for_all ~f:(fun t -> is_ground_type t) ts
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
  | FunType (argt, rest), a :: ats
    when argt = a -> fun_type_applies rest ats 
  | t, []  -> pure t
  | _ -> fail @@ sprintf
        "The type %s doesn't apply to the arguments %s." (pp_typ ft)
        (pp_typ_list argtypes)

let rec elab_tfun_with_args tf args = match tf, args with
  | PolyFun (n, tp), a :: args' ->
      let tp' = subst_type_in_type n a tp in
      elab_tfun_with_args tp' args'
  | t, [] -> pure t
  | _ ->
      let msg = sprintf
        "Cannot elaborate %s with type arguments %s." (pp_typ tf)
        (pp_typ_list args) in
      fail msg

(****************************************************************)
(*             Utility function for pattern matches             *)
(****************************************************************)
let is_catchall p =
    match p with
    | Wildcard -> true
    | Binder _ -> true
    | Constructor _ -> false
