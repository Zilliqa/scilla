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
open Utils

(* Instantiated the type environment *)
module SimpleTEnv = MakeTEnv(PlainTypes)
open SimpleTEnv

let rec check_patterns_exhaustive expected_type patterns =
  (* Return (true, _) if a catchall is seen.
     Return (false, dict) if no catchall is seen,
     where dict is a dictionary mapping constructor names to subpatterns *)
  let rec dict_builder patterns dict =
    match patterns with
    | (Constructor (s, sps)) :: p_rest ->
        (match AssocDictionary.lookup s dict with
         | None -> fail @@ sprintf "Constructor %s does not match type %s" s (pp_typ expected_type)
         | Some v ->
             let new_dict = AssocDictionary.insert s (sps :: v) dict in
             dict_builder p_rest new_dict)
    | Wildcard :: _
    | Binder _ :: _ -> pure @@ (true, dict)
    | [] -> pure @@ (false, dict) in
  let rec checker adt patterns =
    let ctr_names = List.map adt.tconstr ~f:(fun ctr -> ctr.cname) in
    let init_dict = List.fold ctr_names
        ~init:(AssocDictionary.make_dict())
        ~f:(fun dict ctr_name -> AssocDictionary.insert ctr_name [] dict) in
    let%bind (catchall, pattern_dict) = dict_builder patterns init_dict in
    if catchall
    then pure @@ ()
    else
      (* No catchall. Check that subpatterns for each constructor are exhaustive. *)
      let%bind _ = mapM
          ~f:(fun (ctr_name, sps) ->
              match sps with
              (* TODO: Improve error messages for non-exhaustive subpatterns *)
              | [] -> fail @@ sprintf "Non-exhaustive match. No pattern found for constructor %s" ctr_name
              | _ ->
                  (* At least one subpattern found for this constructor *)
                  match List.findi adt.tmap (fun _ (ctrn, ctr_map) -> ctrn = ctr_name) with
                  | None ->
                      (* No type mapping for this constructor.
                         Since at least one subpattern exists, the match is exhaustive.*)
                      pure @@ ()
                  | Some (_, (_, tmaps)) ->
                      (* Type mapping found. Unzip subpatterns *)
                      let arity = List.length tmaps in
                      let%bind unzipped_sps = unzipN_rev arity sps
                          ~msg:(sprintf "Incorrect arity for constructor %s in pattern." ctr_name) in
                      (* Check that subpatterns are exhaustive *)
                      let types = List.map tmaps ~f:(fun (_, t) -> t) in
                      let%bind _ = map2M ~f:(fun t sps -> check_patterns_exhaustive t sps) types unzipped_sps in
                      pure @@ ())
          (AssocDictionary.to_list pattern_dict) in
         pure @@ () in
  match expected_type with
  | ADT (adt_name, adt_types) ->
      let%bind adt = DataTypeDictionary.lookup_name adt_name in
      (* TODO: Unify expected_type with adt, and pass the resulting type to checker *)
      checker adt patterns
  | TypeVar typevar_name ->
      (* Figure out the expected constructors from the first pattern *)
      (match patterns with
       | Wildcard :: _
       | Binder _ :: _ -> pure @@ () (* Pattern is exhaustive *)
       | (Constructor (ctr_name, ctr_values)) :: _ ->
           let%bind (adt, _) = DataTypeDictionary.lookup_constructor ctr_name in
           checker adt patterns
       | [] -> fail @@ sprintf "Non-exhaustive match. No pattern found for type variable %s" typevar_name)
  | _ -> (* No constructors available. Type is only matched by a single binder or wildcard *)
      match patterns with
      | [Wildcard]
      | [Binder _] -> pure @@ ()
      | _ -> pure @@ () (* Illegal, but caught elsewhere *)


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
      let tenv' = TEnv.addT (TEnv.copy tenv) arg t in
      let%bind bt = get_type body tenv' in
      pure @@ mk_qual_tp (FunType (t, bt.tp))
  | App (f, actuals) ->
      let%bind fres = TEnv.resolveT tenv (get_id f) ~lopt:(Some (get_loc f)) in
      let ftyp = (rr_typ fres).tp in
      let%bind tresults = mapM actuals
          ~f:(fun arg -> TEnv.resolveT tenv (get_id arg) ~lopt:(Some (get_loc arg))) in
      let targs = List.map tresults ~f:(fun rr -> (rr_typ rr).tp) in
      let%bind res_type = fun_type_applies ftyp targs in
      pure @@ mk_qual_tp res_type
  | Builtin (i, actuals) ->
      let%bind tresults = mapM actuals
          ~f:(fun arg -> TEnv.resolveT tenv (get_id arg) ~lopt:(Some (get_loc arg))) in
      let targs = List.map tresults ~f:(fun rr -> (rr_typ rr).tp) in
      let%bind (_, ret_typ, _) = BuiltInDictionary.find_builtin_op i targs in
      pure @@ mk_qual_tp ret_typ
  | Let (i, t, lhs, rhs) ->
      (* TODO: Check that matches type *)
      let%bind ityp = get_type lhs tenv in
      let tenv' = TEnv.addT (TEnv.copy tenv) i ityp.tp in
      get_type rhs tenv'
  | MatchExpr (i, pes) ->
      (* TODO: Check that type of i matches types of patterns *)
      (* TODO: Check types in patterns match *)
      (* TODO: Check that identifiers are not used more than once for each pattern. *)
      let%bind r = TEnv.resolveT tenv (get_id i) ~lopt:(Some (get_loc i)) in
      let%bind _ = check_patterns_exhaustive (rr_typ r).tp (List.map pes ~f:(fun (p, e) -> p)) in
      (* TODO: Perform actual type check *)
      fail @@ sprintf
      "Unable to typecheck Match expression %s" (expr_str e)

  (* 1. Type-check primitive literals *)
  (* 2. ADTs and pattern-matching *)
  (* 3. Recursin principles (hard-coded) *)
  (* 4. Type-check maps *)
  (* 5. Type-check ADTs *)

  (* TODO: Implement other expressions *)
  | _ -> fail @@ sprintf
      "Failed to resolve the type of the expresssion:\n%s\n" (expr_str e)
