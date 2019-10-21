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
open Textutils
open ScillaUtil

let load ~params ~cfg ~env =
  let open Core_bench in
  let open Config_t in
  let open Params in
  let include_suite b = List.mem params.suites b ~equal:Suite.equal in
  (* Load benchmarks for standalone closed expressions *)
  let expression_tests =
    if include_suite Suite.Expressions
    then Suite.(load Expressions ~cfg ~env)
    else []
  in
  (* Load contract benchmarks *)
  let contract_tests =
    if include_suite Suite.Contracts
    then Suite.(load Contracts ~cfg ~env)
    else []
  in
  let tests = expression_tests @ contract_tests in
  (* Filter by the regex, if needed *)
  match params.regex with
  | Some re -> List.filter tests ~f:(fun t -> Re2.matches re (Test.name t))
  | None -> tests

let list groups =
  let open Core_bench in
  let open Ascii_table in
  List.iter groups ~f:(fun group ->
      let cells =
        List.map (Test.tests group)
          ~f:(fun test ->
              [ Test.name group
              ; Test.Basic_test.name test
              ])
      in simple_list_table
        ~display:Display.column_titles
        ["group"; "test"] cells)

let compare_and_display _ ~env = ()

let exec tests ~params ~env =
  let module Bench = Core_bench.Bench in
  let open Params in
  let open Defaults in
  (* Run the benchmarks and get back the measurements *)
  let meas = Bench.measure ~run_config tests in
  (* First, save measurements into a file system, if needed *)
  if params.save then Measurements.save meas ~env;
  (* Analyze the measurements, get the results *)
  let results = Measurements.analyze meas in
  if params.compare then compare_and_display meas ~env;
  (* Finally, display and diff the benchmarking results *)
  if params.display then Bench.display results ~display_config
