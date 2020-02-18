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

type params = {
  suites : Suite.t list;
  quota : Core_bench.Quota.t;
  regex : Re2.t option;
  list : bool;
  save : bool;
  display : bool;
  compare : bool;
  threshold : float;
  ci : bool;
  timestamp : string option;
}
[@@deriving make]

(** Load benchmarks *)
let load ~params ~cfg ~env =
  let open Core_bench in
  let include_suite b = List.mem params.suites b ~equal:Suite.equal in
  (* Load benchmarks for standalone closed expressions *)
  let expression_tests =
    if include_suite Suite.Expressions then Suite.(load Expressions ~cfg ~env)
    else []
  in
  (* Load contract benchmarks *)
  let contract_tests =
    if include_suite Suite.Contracts then Suite.(load Contracts ~cfg ~env)
    else []
  in
  let tests = expression_tests @ contract_tests in
  (* Filter by the regex, if needed *)
  match params.regex with
  | Some re -> List.filter tests ~f:(fun t -> Re2.matches re (Test.name t))
  | None -> tests

(** Output benchmark groups along with their tests *)
let list = Display.print_tests

let save results ~params ~env =
  if params.save then Some (Measurement_results.save results ~env) else None

let compare_and_display ~current_dir ~results ~params ~env =
  (* Now, load the benchmark results we want to compare with *)
  let latest =
    Measurement_results.load_latest ~timestamp:params.timestamp
      ~current:current_dir ~env
  in
  match latest with
  | None ->
      Display.print_results results;
      print_endline "Nothing to compare with, the comparison is skipped"
  | Some previous ->
      (* Calculate benchmark results deltas *)
      let deltas = Measurement_results.calc_deltas ~previous ~current:results in
      (* Print the comparison results (along with the time deltas) *)
      Display.print_comparison ~previous ~current:results ~deltas;
      (* Detect significant performance regressions when
         running on CI and fail with non-zero exit code, if any *)
      if params.ci then
        Measurement_results.detect_regressions ~previous ~deltas
          ~threshold:params.threshold

(** Run the given benchmarks *)
let exec tests ~params ~env =
  let module B = Core_bench in
  let run_config = B.Run_config.create ~quota:params.quota () in
  (* First, run the benchmarks and get back the measurements.
     Note that we can't just use the [B.measure] here because it
     returns the [B.Bench.Measurement.t], but we want to use the
     [B.Measurement.t] to be able to access its fields for
     deltas calculation later. Hence we have to repeat it's
     implementation here ourselves *)
  let meas = tests |> B.Test.expand |> B.Benchmark.measure_all run_config in
  (* Analyze the measurements, get the results *)
  let analysis_results = Measurements.analyze meas in
  (* Create an intermediate representation for the
     benchmark results, which we use for storage and comparison *)
  let results = Measurement_results.mk analysis_results in
  (* Save current benchmark results, get back the timestamp/directory *)
  let current_dir = save results ~params ~env in
  compare_and_display ~current_dir ~results ~params ~env
