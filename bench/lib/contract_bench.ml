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
open Core_bench
open Config_t

(* Create a benchmark group given the [contract] and [group] *)
let mk_bench_group contract ~group ~env =
  contract.transitions
  |> List.mapi ~f:(Transition_bench.mk ~contract ~group ~env)
  |> Bench.Test.create_group ~name:("contract/" ^ group.name ^ "/" ^ contract.name)

(* Create a benchmark group out of the
   given [group] of benchmark contracts *)
let to_bench_group group ~env =
  group
  |> Config.Contract.read_group ~env
  |> List.map ~f:(mk_bench_group ~group ~env)

let mk contracts ~env =
  List.concat_map contracts ~f:(to_bench_group ~env)
