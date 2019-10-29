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

let load ~params ~cfg ~env =
  let open Core_bench in
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

let list =
  Display.print_tests

let save results ~params ~env =
  let open Params in
  if params.save
  then Some (Measurement_results.save results ~env)
  else None

let exec tests ~params ~env =
  let module B = Core_bench in
  let open Params in
  let run_config = Defaults.mk_run_config params.quota in
  (* First, run the benchmarks and get back the measurements.
     Note that we can't just use the [B.measure] here because it returns
     the [B.Bench.Measurement.t], but we want use the [B.Measurement.t] to
     be able to access its fields for deltas calculation later.
     Hence we have to repeat it's implementation here ourselves: *)
  let meas =
    tests
    |> B.Test.expand
    |> B.Benchmark.measure_all run_config in
  (* Analyze the measurements, get the results *)
  let analysis_results = Measurements.analyze meas in
  (* Create an intermediate representation for the
     benchmark results, which we use for storage and comparison *)
  let results = Measurement_results.mk analysis_results in
  (* Save current benchmark results, get back the timestamp/directory *)
  let current_dir = save results ~params ~env in
  let current_timestamp = Option.value current_dir ~default:"current (not saved)" in
  (* Now, load the benchmark results we want to compare with *)
  let latest = Measurement_results.load_latest
      ~timestamp:params.timestamp
      ~current:current_dir ~env in
  match latest with
  | None ->
      Display.print_results results;
      print_endline "Nothing to compare with, the comparison is skipped"
  | Some (previous_results, previous_timestamp) ->
      (* Calculate benchmark results deltas *)
      let deltas = Measurement_results.calc_deltas
          ~previous:previous_results ~current:results in
      (* Print the comparison results (along with the time deltas) *)
      Display.print_comparison
        ~previous:(previous_results, previous_timestamp)
        ~current:(results, current_timestamp)
        ~deltas
