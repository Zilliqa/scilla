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
open Core_bench.Simplified_benchmark
open ScillaUtil.FilePathInfix

module Timestamp = struct
  (* see [man strptime] *)

  let fmt = "%Y%m%d%H%M%S"
  let zone = force Time.Zone.local

  (** Make a timestamp string from the given [Time.t] *)
  let format time = Time.format time fmt ~zone

  (** Parse timestamp *)
  let parse = Time.parse ~fmt ~zone

  (** Make a timestamp that looks like "20191019192411" *)
  let mk () = format @@ Time.now ()

  (** Parse the given list of timestamps and
      sort them in a descending order *)
  let sort_desc ts =
    ts
    |> List.map ~f:parse
    |> List.sort ~compare:Time.compare
    |> List.rev_map ~f:format

  let%test "roundtrip" =
    let ts = Time.now () in
    format ts = format (parse (format ts))
end

let mk_path dir ~env =
  env.Env.results_dir ^/ dir

let only_dirs ~env =
  List.filter ~f:(fun dir -> Sys.is_directory_exn @@ mk_path dir ~env)

(** List paths containing benchmark results *)
let ls_results ~env =
  env.Env.results_dir
  |> Sys.ls_dir
  |> only_dirs ~env
  |> Timestamp.sort_desc

(** Given the [timestamp] of the benchmark results to
    compare with return a directory named after that timestamp,
    otherwise return the latest one, if it exists and
    it is not the same as the [current] timestamp *)
let latest_result_path ~timestamp ~current ~env =
  (* List paths containing benchmark results *)
  let paths = ls_results ~env in
  (* Helper function to find a dir with
     the latest (previous) benchmark results *)
  let find_latest () =
    let paths = match current with
    | None -> paths
    | Some s -> List.filter paths ~f:(fun dir -> dir <> s)
    in List.hd paths
  in match timestamp with
  | None -> find_latest ()
  | Some s -> List.find paths ~f:(fun p -> p = s)

let sort =
  List.sort ~compare:(fun x y ->
      Result.(String.compare x.full_benchmark_name y.full_benchmark_name))

let mk results =
  results
  |> to_sexp
  |> Results.t_of_sexp
  |> sort

let save results ~env =
  (* Save the benchmark results in an
     appropriate place in the file system *)
  let dir = Timestamp.mk () in
  let path = env.Env.results_dir ^/ dir in
  printf "Results will be saved to %s.\n%!" dir;
  (* Create a directory to place the results into,
     use the current timestamp as its name  *)
  Unix.mkdir path;
  (* Save each benchmark result as a separate file *)
  List.iter results ~f:(Measurement_result.save ~path);
  dir

let load_from path =
  path
  |> Sys.ls_dir
  |> List.map ~f:(fun fn -> Measurement_result.load @@ path ^/ fn)
  |> sort

let load_latest ~timestamp ~current ~env =
  let path = latest_result_path ~timestamp ~current ~env in
  Option.map path ~f:(fun s -> load_from (env.Env.results_dir ^/ s))

let calc_deltas ~previous ~current =
  (* We assume that both lists are already sorted *)
  List.map2_exn previous current ~f:Measurement_result_delta.calc

let detect_regressions ~previous ~deltas ~threshold =
  let open Result in
  let open Measurement_result_delta in
  let detect prev delta =
    (* Check if there is a significant performance drop *)
    if delta.percentage > threshold
    then raise (Failure (
        sprintf "Detected performance regression in benchmark %s. Time per run delta: %s (> %.2f percent threshold)"
          prev.full_benchmark_name (Util.ns_to_ms_string delta.result.time_per_run_nanos) threshold)) in
  List.iter2_exn previous deltas ~f:detect
