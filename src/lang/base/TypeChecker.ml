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


let rec check_patterns t patterns =
  (* builds dictionary of subpatterns for each constructor.
     dict is assumed to contain all constructor keys. *)
  let rec dict_builder ps dict =
    match ps with
    | (Constructor (s, ps)) :: p_rest ->
        (match AssocDictionary.lookup s dict with
         | None -> fail @@ sprintf "Unexpected constructor %s in pattern" s
         | Some v ->
             let new_dict = AssocDictionary.insert s (ps :: v) dict in
             dict_builder p_rest new_dict)
    | [Wildcard]
    | [Binder _] -> pure @@ (true, dict)
    | [] -> pure @@ (false, dict)
    | _ -> fail @@ "Unreachable pattern" in

  (* Check exhaustive match *)
  match t with
  | FunType _ -> pure @@ false (* Matching is impossible *) 
  | ADT (s, ts) ->
      (* Find ADT description *)
      (let%bind adt = DataTypeDictionary.lookup_name s in
       let ctr_names = List.map adt.tconstr ~f:(fun ctr -> ctr.cname) in
       (* Build dictionary mapping constructor names to subpatterns *)
       let dict_init = List.fold ctr_names
           ~init:(AssocDictionary.make_dict())
           ~f:(fun dict ctr_name -> AssocDictionary.insert ctr_name [] dict) in
       let%bind (catchall, pattern_dict) = dict_builder patterns dict_init in
       (* Catchall means patterns are exhaustive *)
       if catchall
       then pure @@ true
       else
         (* No catchall. Check that subpatterns for each constructor are exhaustive. *)
         let%bind _ = mapM
             ~f:(fun (ctr_name, sps) ->
                 match sps with
                 | [] -> fail @@ sprintf "Non-exhaustive match. No pattern found for constructor %s" ctr_name
                 | _ ->
                     (* At least one subpattern found for this constructor *)
                     match List.findi adt.tmap (fun _ (ctrn, ctr_map) -> ctrn = ctr_name) with
                     | None ->
                         (* No type mapping for this constructor.
                              The only legal list of subpatterns is
                              therefore one empty subpattern. *)
                         (match sps with
                          | [[]] -> pure @@ true
                          | _ -> fail @@ "Illegal subpattern")
                     | Some (_, (ctr, tmaps)) ->
                         (* Type mapping found. Check arity *)
                         let arity = List.length tmaps in
                         (* Unzip subpatterns, while checking for correct arities.
                              Then check for exhaustive matches in each *)
                         let%bind unzipped_sps = unzipN_rev arity sps 
                             ~msg:(sprintf "Incorrect arity for constructor %s in pattern." ctr) in
                         let types = List.map tmaps ~f:(fun (_, t) -> t) in
                         let%bind _ = map2M ~f:(fun t sps -> check_patterns t sps) types unzipped_sps in
               pure @@ true)
             (AssocDictionary.to_list pattern_dict) in
         pure @@ true)
  | _ ->
      (* Only one catchall allowed *)
      match patterns with
      | [Wildcard]
      | [Binder _] -> pure @@ true
      | _ -> pure @@ false



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
      let%bind _ = check_patterns (rr_typ r).tp (List.map pes ~f:(fun (p, e) -> p)) in
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
