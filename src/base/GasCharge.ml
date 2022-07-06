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
open Result.Let_syntax
open ErrorUtils
open MonadUtil
open Identifier

(* Poor man's dependent type for int > 0.
 * https://stackoverflow.com/a/55364673/2128804 *)
module PositiveInt : sig
  type t [@@deriving sexp]

  val create : int -> (t, scilla_error list) result

  val get : t -> int
end = struct
  type t = int [@@deriving sexp]

  let create i =
    if i > 0 then pure i
    else fail0 ~kind:"PositiveInt: Constructed failed." ?inst:None

  let get i = i
end

type resolved_ValueOf = GFloat of float | GInt of int

module type GC = sig
  module Name : QualifiedName

  type gas_charge =
    | StaticCost of int
    (* The size of the literal to which the variable resolves to. *)
    | SizeOf of Name.t
    (* The value of the integer to which the variable resolves to. *)
    | ValueOf of Name.t
    (* The length of the Scilla list / map to which the varible resolves to. *)
    | LengthOf of Name.t
    (* The cost incurred in sorting a map read from the database.
     * leveldb, our backend database doesn't guarantee order. *)
    | MapSortCost of Name.t
    (* Sum of two gas charges. *)
    | SumOf of gas_charge * gas_charge
    (* Product of two gas charges. *)
    | ProdOf of gas_charge * gas_charge
    (* The minimum of two gas charges. *)
    | MinOf of gas_charge * gas_charge
    (* div_ceil x y = if x % y = 0 then x / y else (x / y) + 1 *)
    | DivCeil of gas_charge * PositiveInt.t
    (* LogOf(I) = int (log(float(I) + 1.0)) + 1 *)
    | LogOf of gas_charge
  [@@deriving sexp]
end

module ScillaGasCharge (N : QualifiedName) = struct
  module Name = N

  type gas_charge =
    | StaticCost of int
    (* The size of the literal to which the variable resolves to. *)
    | SizeOf of Name.t
    (* The value of the integer to which the variable resolves to. *)
    | ValueOf of Name.t
    (* The length of the Scilla list / map to which the varible resolves to. *)
    | LengthOf of Name.t
    (* The cost incurred in sorting a map read from the database.
     * leveldb, our backend database doesn't guarantee order. *)
    | MapSortCost of Name.t
    (* Sum of two gas charges. *)
    | SumOf of gas_charge * gas_charge
    (* Product of two gas charges. *)
    | ProdOf of gas_charge * gas_charge
    (* The minimum of two gas charges. *)
    | MinOf of gas_charge * gas_charge
    (* div_ceil x y = if x % y = 0 then x / y else (x / y) + 1 *)
    | DivCeil of gas_charge * PositiveInt.t
    (* LogOf(I) = int (log(float(I) + 1.0)) + 1 *)
    | LogOf of gas_charge
  [@@deriving sexp]

  let rec replace_variable_name ~f = function
    | StaticCost _ as g -> g
    | SizeOf v -> SizeOf (f v)
    | ValueOf v -> ValueOf (f v)
    | LengthOf v -> LengthOf (f v)
    | MapSortCost m -> MapSortCost (f m)
    | SumOf (g1, g2) ->
        SumOf (replace_variable_name ~f g1, replace_variable_name ~f g2)
    | ProdOf (g1, g2) ->
        ProdOf (replace_variable_name ~f g1, replace_variable_name ~f g2)
    | MinOf (g1, g2) ->
        MinOf (replace_variable_name ~f g1, replace_variable_name ~f g2)
    | DivCeil (g1, g2) -> DivCeil (replace_variable_name ~f g1, g2)
    | LogOf g -> LogOf (replace_variable_name ~f g)

  (* Assuming that resolver resolves
   *   SizeOf v : To the literal_size of v
   *   ValueOf v : The value of the integer literal
   *   Other special purpose charges (ListLength, etc)
   * to an integer compute the total gas charge (an integer) for g.
   *)
  let eval resolver g =
    let rec recurser g =
      match g with
      | StaticCost i -> pure @@ GInt i
      | SizeOf _ | ValueOf _ | LengthOf _ | MapSortCost _ -> resolver g
      | SumOf (g1, g2) -> (
          let%bind i1 = recurser g1 in
          let%bind i2 = recurser g2 in
          match (i1, i2) with
          | GInt i1', GInt i2' -> pure @@ GInt (i1' + i2')
          | _ ->
              fail0 ~kind:"GasCharge: Cannot evaluate float charge" ?inst:None)
      | ProdOf (g1, g2) -> (
          let%bind i1 = recurser g1 in
          let%bind i2 = recurser g2 in
          match (i1, i2) with
          | GInt i1', GInt i2' -> pure @@ GInt (i1' * i2')
          | _ ->
              fail0 ~kind:"GasCharge: Cannot evaluate float charge" ?inst:None)
      | MinOf (g1, g2) -> (
          let%bind i1 = recurser g1 in
          let%bind i2 = recurser g2 in
          match (i1, i2) with
          | GInt i1', GInt i2' -> pure @@ GInt (Int.min i1' i2')
          | _ ->
              fail0 ~kind:"GasCharge: Cannot evaluate float charge" ?inst:None)
      | DivCeil (g1, g2) -> (
          let div_ceil x y = if x % y = 0 then x / y else (x / y) + 1 in
          let%bind g1_i = recurser g1 in
          match g1_i with
          | GInt g1_i' -> pure @@ GInt (div_ceil g1_i' (PositiveInt.get g2))
          | _ ->
              fail0 ~kind:"GasCharge: Cannot evaluate float charge" ?inst:None)
      | LogOf g ->
          let logger uf =
            let f = match uf with GFloat f -> f | GInt i -> Float.of_int i in
            (Float.to_int @@ Float.log (f +. 1.0)) + 1
          in
          let%bind g' = recurser g in
          pure @@ GInt (logger g')
    in
    recurser g

  let rec pp_gas_charge = function
    | StaticCost i -> Int.to_string i
    | SizeOf v -> sprintf "sizeof (%s)" (Name.as_string v)
    | ValueOf v -> sprintf "valueof (%s)" (Name.as_string v)
    | LengthOf v -> sprintf "lengthof (%s)" (Name.as_string v)
    | MapSortCost m -> sprintf "mapsortcost (%s)" (Name.as_string m)
    | SumOf (g1, g2) ->
        sprintf "(%s + %s)" (pp_gas_charge g1) (pp_gas_charge g2)
    | ProdOf (g1, g2) ->
        sprintf "(%s * %s)" (pp_gas_charge g1) (pp_gas_charge g2)
    | MinOf (g1, g2) ->
        sprintf "min(%s, %s)" (pp_gas_charge g1) (pp_gas_charge g2)
    | DivCeil (g1, g2) ->
        sprintf "divceil(%s, %d)" (pp_gas_charge g1) (PositiveInt.get g2)
    | LogOf g -> sprintf "log (%s)" (pp_gas_charge g)
end
