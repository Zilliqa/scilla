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

open Core_kernel
open Result.Let_syntax
open MonadUtil

type gas_charge =
  | StaticCost of int
  (* The size of the literal to which the variable resolves to. *)
  | SizeOf of string
  (* The value of the integer to which the variable resolves to. *)
  | ValueOf of string
  | SumOf of gas_charge * gas_charge
  | ProdOf of gas_charge * gas_charge
[@@deriving sexp]

let replace_variable_name g ~f =
  let rec recurser = function
    | StaticCost _ -> g
    | SizeOf v -> SizeOf (f v)
    | ValueOf v -> ValueOf (f v)
    | SumOf (g1, g2) -> SumOf (recurser g1, recurser g2)
    | ProdOf (g1, g2) -> ProdOf (recurser g1, recurser g2)
  in
  recurser g

(* Given two resolvers:
     1. Resolve variable to a literal and provide it's size
     2. Resolve variable to an integer 
   evaluat gas_charge.
*)
let eval vsize_resolver ival_resolver g =
  let rec recurser = function
    | StaticCost i -> pure i
    | SizeOf v ->
        let%bind i = vsize_resolver v in
        pure i
    | ValueOf v ->
        let%bind i = ival_resolver v in
        pure i
    | SumOf (g1, g2) ->
        let%bind i1 = recurser g1 in
        let%bind i2 = recurser g2 in
        pure (i1 + i2)
    | ProdOf (g1, g2) ->
        let%bind i1 = recurser g1 in
        let%bind i2 = recurser g2 in
        pure (i1 * i2)
  in
  recurser g
