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

let list = Display.print_tests

let analyze_and_display meas ~params ~env =
  let module Bench = Core_bench.Bench in
  let open Params in
  let open Defaults in
  (* Analyze the measurements, get the results *)
  let results = Measurements.analyze meas in
  (* Print the current results *)
  print_endline "Current results";
  Bench.display results ~display_config;
  (* Now, load the measurements to compare with *)
  match Measurements.load ~timestamp:params.timestamp ~env with
  | None -> print_endline "Nothing to compare with, the comparison is skipped"
  | Some orig_meas ->
      (* We have to analyze the loaded measurements here to be display them.
         This is because of how the [core_bench] is designed:
         it stores and loads measurements, not their results *)
      let orig_results = Measurements.analyze orig_meas in
      (* Print the original measurement results *)
      print_endline "Previous results";
      Bench.display orig_results ~display_config;
      let deltas = Measurements.calc_deltas ~orig_meas ~curr_meas:meas in
      let deltas_results = Measurements.analyze deltas in
      (* Print the comparison results (time detlas) *)
      print_endline "Deltas";
      Display.print_deltas (deltas, deltas_results)

let exec tests ~params ~env =
  let module B = Core_bench in
  let open Params in
  let open Defaults in
  (* First, run the benchmarks and get back the measurements.
     Note that we can't just use the [B.measure] here because it returns
     the [B.Bench.Measurement.t], but we want use the [B.Measurement.t] to
     be able to access its fields for deltas calculation later.
     Hence we have to repeat it's implementation here ourselves: *)
  let meas =
    tests
    |> B.Test.expand
    |> B.Benchmark.measure_all run_config in
  (* let meas = Bench.measure ~run_config tests in *)
  (* Next, save measurements into a file system, if needed *)
  if params.save then Measurements.save meas ~env;
  (* Finally, display the benchmarking results *)
  analyze_and_display meas ~params ~env
