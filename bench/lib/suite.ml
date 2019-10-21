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

type t =
  | Expressions
  | Contracts
  | Modules
[@@deriving compare]

let equal = [%compare.equal: t]

let all =
  [ Expressions; Contracts; Modules ]

let load suite ~cfg ~env =
  let open Config_t in
  match suite with
  | Expressions -> List.map cfg.expressions ~f:(Expression_bench.mk ~env)
  | Contracts -> Contract_bench.mk cfg.contracts ~env
  | Modules ->
      (* TODO: load internal/module benchmarks *)
      []

let of_string = function
  | "expressions" -> Expressions
  | "contracts" -> Contracts
  | "modules" -> Modules
  | _ -> raise (Failure "Not a valid benchmark suite type")

let arg_type = Command.Param.Arg_type.create of_string
