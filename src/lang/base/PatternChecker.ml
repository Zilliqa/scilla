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
open Array

(*
(* Map [[x1; x2; ...; xn]; [y1; y2; ...; yn]; ...]
   to [[...; y1; x1]; [...; y2; x2]; ...; [...; yn; xn]] *)
let rec unzipN_rev n ls ~msg =
  let rec build_empties n =
    match n with
    | 0 -> []
    | m -> [] :: (build_empties (m-1)) in
  let rec unzip_level n l acc =
    match (n, l, acc) with
    | (0, [], []) -> pure @@ []
    | (m, v :: vs, r :: rs) ->
        let%bind unzip_res = unzip_level (m-1) vs rs in
        pure @@ (v :: r) :: unzip_res
    | _ -> fail @@ msg in
  let rec unzip_rev n ls acc =
    match ls with
    | [] -> pure @@ acc
    | l :: ls' ->
        let%bind new_acc = (unzip_level n l acc) in
        unzip_rev n ls' new_acc in
  unzip_rev n ls (build_empties n)
*)

let rec iterM ~f ls = match ls with
  | [] -> Ok ()
  | x :: ls' ->
      match f x with
      | Ok z -> iterM ~f:f ls'
      | err -> err

module Exp_descriptions = struct
  type yes_no_maybe =
    | Yes
    | No
    | Maybe

  type exp_dsc =
    | Pos of string * exp_dsc list (* expression matches constructor name with listed subdescriptions *)
    | Neg of string list (* expression does not match list of constructor names *)

  let add_neg dsc c_name =
    match dsc with
    | Pos _ -> raise (InternalError (sprintf "Internal error: Can only add negative constructor %s to Neg description" c_name))
    | Neg cs -> Neg (c_name :: cs)

  let rec build_dsc ctx dsc sps =
    match ctx with
    | [] -> dsc
    | (c_name, args) :: ctx_rest ->
        match sps with
        | [] -> raise (InternalError "Internal error: Cannot build expression description from pattern match context")
        | (_, _, dargs) :: spss ->
            build_dsc ctx_rest (Pos (c_name, List.rev args @ (dsc :: dargs))) spss

  let augment_ctx ctx dsc =
    match ctx with
    | [] -> []
    | (c_name, args) :: rest -> (c_name, dsc :: args) :: rest

  let pos_ctx ctx =
    match ctx with
    | (c_name, args) :: rest -> augment_ctx rest (Pos (c_name, List.rev args))
    | [] -> raise (InternalError "Internal error: pattern match context is empty")
end

module Decision_Tree = struct

  type ('v, 'tv, 'cv) decision_tree =
    | Success of 'v
    | Fail
    | IfEq of 'tv * 'cv * ('v, 'tv, 'cv) decision_tree * ('v, 'tv, 'cv) decision_tree

end


open Exp_descriptions
open Decision_Tree

let pm_check t clauses =
  let reachable = Array.create (List.length clauses) false in
  let static_match c_name arity dsc =
    match dsc with
    | Pos (dsc_c_name, _) -> if c_name = dsc_c_name then Yes else No
    | Neg c_names ->
      match List.findi c_names ~f:(fun _ c -> c = c_name) with
      | None -> if List.length c_names = arity - 1 then Yes else Maybe
      | Some _ -> No
  in
  let rec traverse_clauses dsc i rest_clauses =
    match rest_clauses with
    | [] -> fail @@ "Non-exhaustive pattern match." (* TODO: Give counter-example based on dsc *)
    | (p1, _) :: rest_clauses' -> match_pattern p1 t dsc [] [] i rest_clauses'
  and traverse_pattern ctx sps i rest_clauses  =
    match sps with
    | [] ->
        (* Pattern can be matched *)
        let _ = Array.set reachable i true in
        let e = match List.nth clauses i with
          | Some (_, e) -> e
          | None -> raise (InternalError (sprintf "Pattern index %d too high (or low)" i))
        in
        pure @@ Success e
    | ([], [], []) :: sps_rest ->
        traverse_pattern (pos_ctx ctx) sps_rest i rest_clauses
    | (p1::ps, t1::ts, dsc1::dscs) :: sps_rest ->
        match_pattern p1 t1 dsc1 ctx ((ps, ts, dscs)::sps_rest) i rest_clauses
    | _ -> fail @@ "Internal error - pattern match uses incorrect arity"
  and match_pattern p t dsc ctx sps_rest i rest_clauses =
    match p with
    | Wildcard
    | Binder _ -> traverse_pattern (augment_ctx ctx dsc) sps_rest i rest_clauses
    | Constructor (c_name, sps) ->
        let arity () =
          match t with
          | ADT (_, args) -> List.length args
          | _ -> raise (InternalError "Attempt to pattern-match value of non-ADT type") in
        let get_t_args () =
          match t with
          | ADT (_, args) -> args
          | _ -> raise (InternalError "Attempt to pattern-match value of non-ADT type") in
        let get_dsc_args dsc =
          match dsc with
          | Pos (_, args) -> args
          | Neg _ -> List.init (arity()) ~f:(fun _ -> Neg []) in
        let success () =
          traverse_pattern ((c_name, [])::ctx) ((sps, get_t_args (), get_dsc_args dsc)::sps_rest) i rest_clauses in
        let failure new_dsc =
          traverse_clauses (build_dsc ctx new_dsc sps_rest) (i+1) rest_clauses in
        match static_match c_name (arity()) dsc with
        | Yes -> success ()
        | No -> failure dsc
        | Maybe ->
            let%bind s_tree = success() in
            let%bind f_tree = failure (add_neg dsc c_name) in
            pure @@ IfEq (t, c_name, s_tree, f_tree)
  in
  let decision_tree = traverse_clauses (Neg []) 0 clauses in
  match Array.findi reachable ~f:(fun i r -> not r) with
  | None -> decision_tree (* All patterns reachable *)
  | Some (i, _) -> fail @@ "Unreachable pattern" (* TODO: look up relevant pattern in clauses and report it *)




(* 

let rec pm_expr e = match e with
  | Literal _
  | Var _
  | App _
  | Builtin _
  | Constr _
  | TApp _
  | Message _ -> pure @@ ()
  | Fun (_, _, body)
  | Fixpoint (_, _, body)
  | TFun (_, body) ->  pm_expr body
  | Let (i, t, lhs, rhs) ->
      let%bind _ = pm_expr lhs in
      pm_expr rhs
  | MatchExpr (x, clauses) -> ...
        *)



(*

let rec map2M ~f ls1 ls2 = match (ls1, ls2) with
  | (x :: ls1', y :: ls2') ->
      (match f x y, map2M ~f:f ls1' ls2' with
       | Ok z, Ok zs -> Ok (z :: zs)
       | Error z as err, _ -> err
       | _, (Error _ as err) -> err)
  | ([], []) -> Ok []
  | _ -> Error "Internal error: map2M given lists of different lengths"


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


*)
