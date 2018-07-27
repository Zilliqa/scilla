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
open Syntax
open Utils

(****************************************************************)
(*             Pattern match expression descriptions            *)
(****************************************************************)
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



(****************************************************************)
(*             Decision tree                                    *)
(****************************************************************)
module Decision_Tree = struct

  type ('v, 'tv, 'cv) decision_tree =
    | Success of 'v
    | Fail
    | IfEq of 'tv * 'cv * ('v, 'tv, 'cv) decision_tree * ('v, 'tv, 'cv) decision_tree

end

