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
  (* The length of the Scilla list / map to which the varible resolves to. *)
  | LengthOf of string
  (* The cost incurred in sorting a map read from the database.
   * leveldb, our backend database doesn't guarantee order. *)
  | MapSortCost of string
  (* Sum of two gas charges. *)
  | SumOf of gas_charge * gas_charge
  (* Product of two gas charges. *)
  | ProdOf of gas_charge * gas_charge
  (* The minimum of two gas charges. *)
  | MinOf of gas_charge * gas_charge
  (* div_ceil x y = if x % y = 0 then x / y else (x / y) + 1 *)
  | DivCeil of gas_charge * gas_charge
  (* For a Scilla integer I: log(float(I) + 1.0) *)
  | LogOf of string
[@@deriving sexp]

let replace_variable_name g ~f =
  let rec recurser = function
    | StaticCost _ -> g
    | SizeOf v -> SizeOf (f v)
    | ValueOf v -> ValueOf (f v)
    | LengthOf v -> LengthOf (f v)
    | MapSortCost m -> MapSortCost (f m)
    | SumOf (g1, g2) -> SumOf (recurser g1, recurser g2)
    | ProdOf (g1, g2) -> ProdOf (recurser g1, recurser g2)
    | MinOf (g1, g2) -> MinOf (recurser g1, recurser g2)
    | DivCeil (g1, g2) -> DivCeil (recurser g1, recurser g2)
    | LogOf v -> LogOf (f v)
  in
  recurser g

(* Assuming that resolver resolves
 *   SizeOf v : To the literal_size of v
 *   ValueOf v : The value of the integer literal
 *   Other special purpose charges (ListLength, LogOf, etc)
 * to an integer compute the total gas charge (an integer) for g.
 *)
let eval resolver g =
  let rec recurser g =
    match g with
    | StaticCost i -> pure i
    | SizeOf _ | ValueOf _ | LengthOf _ | LogOf _ | MapSortCost _ -> resolver g
    | SumOf (g1, g2) ->
        let%bind i1 = recurser g1 in
        let%bind i2 = recurser g2 in
        pure (i1 + i2)
    | ProdOf (g1, g2) ->
        let%bind i1 = recurser g1 in
        let%bind i2 = recurser g2 in
        pure (i1 * i2)
    | MinOf (g1, g2) ->
        let%bind i1 = recurser g1 in
        let%bind i2 = recurser g2 in
        pure (Int.min i1 i2)
    | DivCeil (g1, g2) ->
        let div_ceil x y = if x % y = 0 then x / y else (x / y) + 1 in
        let%bind g1_i = recurser g1 in
        let%bind g2_i = recurser g2 in
        pure (div_ceil g1_i g2_i)
  in
  recurser g

let rec pp_gas_charge = function
  | StaticCost i -> Int.to_string i
  | SizeOf v -> sprintf "sizeof (%s)" v
  | ValueOf v -> sprintf "valueof (%s)" v
  | LengthOf v -> sprintf "lengthof (%s)" v
  | MapSortCost m -> sprintf "mapsortcost (%s)" m
  | SumOf (g1, g2) -> sprintf "(%s + %s)" (pp_gas_charge g1) (pp_gas_charge g2)
  | ProdOf (g1, g2) -> sprintf "(%s * %s)" (pp_gas_charge g1) (pp_gas_charge g2)
  | MinOf (g1, g2) ->
      sprintf "min(%s, %s)" (pp_gas_charge g1) (pp_gas_charge g2)
  | DivCeil (g1, g2) ->
      sprintf "divceil(%s, %s)" (pp_gas_charge g1) (pp_gas_charge g2)
  | LogOf v -> sprintf "log (%s)" v
