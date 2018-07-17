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

(* Instantiated the type environment *)
module SimpleTEnv = MakeTEnv(PlainTypes)
open SimpleTEnv

(* TODO: Check if the type is well-formed: support type variables *)
let rec get_type e tenv = match e with
  | Var i ->
      let%bind r = TEnv.resolveT tenv (get_id i) ~lopt:(Some (get_loc i)) in
      pure @@ (rr_typ r)
  |  Fun (arg, t, body) ->
      let tenv' = TEnv.addT (TEnv.copy tenv) arg t in
      let%bind bt = get_type body tenv' in
      pure @@ mk_qual_tp (FunType (t, bt.tp))

  (* 1. Type-check primitive literals *)
  (* 2. ADTs and pattern-matching *)
  (* 3. Recursin principles (hard-coded) *)
  (* 3. Built-ins // make this a functor taking built-in signatures *)
  (* 4. Type-check maps *)
  (* 5. Type-check ADTs *)
      

  (* TODO: Implement other expressions *)
  | _ -> fail @@ "Failed to resolve the type"

let literal_type l =
  let open PrimTypes in 
  match l with
  | IntLit (32, _) -> pure int32_typ
  | IntLit (64, _) -> pure int64_typ
  | IntLit (128, _) -> pure int128_typ
  | UintLit (32, _) -> pure uint32_typ
  | UintLit (64, _) -> pure uint64_typ
  | UintLit (128, _) -> pure uint128_typ
  | IntLit(w, _) ->
      fail @@ (sprintf "Wrong bit depth for integer: %i." w)
  | UintLit(w, _) ->
      fail @@ (sprintf "Wrong bit depth for unsigned integer: %i." w)
  | StringLit _ -> pure string_typ
  | BNum _ -> pure bnum_typ
  | Address _ -> pure address_typ
  | Sha256 _ -> pure hash_typ
  | Msg _ -> pure msg_typ
  | Map ((kt, vt), _) ->
      if PrimTypes.is_prim_type kt
      then pure (MapType (kt, vt))
      else fail @@
        (sprintf "Not a primitive map key tpye: %s." (pp_typ kt))        
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
