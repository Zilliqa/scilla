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
open Utils
open PatternUtil

open Exp_descriptions
open Decision_Tree

let pm_check t clauses =
  let reachable = Array.create ~len:(List.length clauses) false in
  let static_match c_name span dsc =
    match dsc with
    | Pos (dsc_c_name, _) -> if c_name = dsc_c_name then Yes else No
    | Neg c_names ->
      match List.findi c_names ~f:(fun _ c -> c = c_name) with
      | None -> if List.length c_names = span - 1 then Yes else Maybe
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
    | _ ->
        fail @@ "Internal error - pattern match uses incorrect arity"
  and match_pattern p t dsc ctx sps_rest i rest_clauses =
    match p with
    | Wildcard
    | Binder _ ->
        traverse_pattern (augment_ctx ctx dsc) sps_rest i rest_clauses
    | Constructor (c_name, sps_cons) ->
        let arity () = List.length sps_cons in
        let get_t_args () = constr_pattern_arg_types t c_name in
        let get_dsc_args dsc =
          match dsc with
          | Pos (_, args) -> args
          | Neg _ -> List.init (arity()) ~f:(fun _ -> Neg []) in
        let success () =
          let%bind t_args = get_t_args() in
          traverse_pattern ((c_name, [])::ctx) ((sps_cons, t_args, get_dsc_args dsc)::sps_rest) i rest_clauses in
        let failure new_dsc =
          traverse_clauses (build_dsc ctx new_dsc sps_rest) (i+1) rest_clauses in
        let%bind (adt, _) = DataTypeDictionary.lookup_constructor c_name in
        let span = List.length adt.tconstr in
        match static_match c_name span dsc with
        | Yes ->
            success ()
        | No ->
            failure dsc
        | Maybe ->
            let%bind s_tree = success() in
            let%bind f_tree = failure (add_neg dsc c_name) in
            pure @@ IfEq (t, c_name, s_tree, f_tree)
  in
  let%bind decision_tree = traverse_clauses (Neg []) 0 clauses in
  match Array.findi reachable ~f:(fun _ r -> not r) with
  | None -> pure @@ decision_tree (* All patterns reachable *)
  | Some _ -> fail @@ "Unreachable pattern." (* TODO: look up relevant pattern in clauses and report it *)
