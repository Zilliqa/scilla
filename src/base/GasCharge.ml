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
