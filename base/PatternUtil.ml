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
    | Pos _ -> raise (mk_internal_error (sprintf "Internal error: Can only add negative constructor %s to Neg description" c_name))
    | Neg cs -> Neg (c_name :: cs)

  let rec build_dsc ctx dsc sps =
    match ctx with
    | [] -> dsc
    | (c_name, args) :: ctx_rest ->
        match sps with
        | [] -> raise (mk_internal_error "Internal error: Cannot build expression description from pattern match context")
        | (_, _, dargs) :: spss ->
            build_dsc ctx_rest (Pos (c_name, List.rev args @ (dsc :: dargs))) spss

  let augment_ctx ctx dsc =
    match ctx with
    | [] -> []
    | (c_name, args) :: rest -> (c_name, dsc :: args) :: rest

  let pos_ctx ctx =
    match ctx with
    | (c_name, args) :: rest -> augment_ctx rest (Pos (c_name, List.rev args))
    | [] -> raise (mk_internal_error "Internal error: pattern match context is empty")
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

