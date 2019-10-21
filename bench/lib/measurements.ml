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
open ScillaUtil

module B = Core_bench

let result_to_option = function
  | Error err ->
      eprintf "Error %s\n%!" (Error.to_string_hum err);
      None
  | Ok r -> Some r

let load_from path =
  path
  |> Sys.ls_dir
  |> List.map ~f:(fun filename -> B.Measurement.load ~filename)

let load ~timestamp ~env =
  let open Env in
  let open FilePathInfix in
  let mk_path dir = env.results_dir ^/ dir in
  (* Get paths containing measurement results *)
  let paths =
    env.results_dir
    |> Sys.ls_dir
    |> List.map ~f:mk_path
    |> List.filter ~f:Sys.is_directory_exn in
  (* Helper function to find dir with
     the latest (previous) measurements *)
  let find_latest () =
    paths
    |> List.sort ~compare:String.compare (* TODO: use the [Time.parse] instead *)
    |> List.hd in
  (* If we're given the timestamp of measurements to compare with
     then try to find a directory named after that timestamp,
     otherwise just find the latest one, if it exists *)
  let dir =
    match timestamp with
    | Some ts -> List.find paths ~f:(fun p -> p = ts)
    | None -> find_latest () in
  Option.map dir ~f:load_from

(* Helper function to make timestamps
   that look like "201910191924" *)
let mk_timestamp () =
  Time.format (Time.now ()) "%Y%m%d%H%M"
    ~zone:(force Time.Zone.local)

let sanitize =
  String.map ~f:(fun c ->
      if Char.is_alphanum c || String.mem "-_." c
      then c
      else '_')

let save_one m ~path =
  let open FilePathInfix in
  let name = m |> B.Measurement.name |> sanitize in
  let filename = path ^/ name ^. "txt" in
  B.Measurement.save m ~filename

let save meas ~env =
  let open FilePathInfix in
  (* Save the measurements in a more
     appropriate place in the file system *)
  let dir = mk_timestamp () |> sanitize in
  let path = env.Env.results_dir ^/ dir in
  printf "Measurements will be saved to %s.\n%!" dir;
  (* Create a directory to place measurements into,
     use the current timestamp as a name for it *)
  Unix.mkdir path;
  (* Save each measurement as a separate file *)
  List.iter meas ~f:(save_one ~path)

(* Run the [B.analyze] for each
   measurement and get back the results *)
let analyze (meas : B.Measurement.t list) =
  meas
  |> List.map ~f:(fun m -> B.Analysis.analyze m Defaults.analysis_configs)
  |> List.filter_map ~f:result_to_option

(* Calculate the absolute delta for
   each measurement of one run of the benchmark *)
let calc_samples_delta x y =
  let open B.Measurement_sample in
  (* Actually, we're only interested in the [runs] and [nanos].
     But let's just calculate everything for consistency.
     Maybe we'll need those later *)
  { runs              = abs (x.runs - y.runs);
    cycles            = abs (x.cycles - y.cycles);
    nanos             = abs (x.nanos - y.nanos);
    compactions       = abs (x.compactions - y.compactions);
    minor_allocated   = abs (x.minor_allocated - y.minor_allocated);
    major_allocated   = abs (x.major_allocated - y.major_allocated);
    promoted          = abs (x.promoted - y.promoted);
    major_collections = abs (x.major_collections - y.major_collections);
    minor_collections = abs (x.minor_collections - y.minor_collections);
  }

let calc_delta orig curr =
  let open B.Measurement in
  let samples = Array.map2_exn
      (samples orig) (samples curr)
      ~f:calc_samples_delta in
  create
    ~name:(name orig ^ "_deltas")
    ~test_name:(test_name orig)
    ~file_name:(file_name orig)
    ~module_name:(module_name orig)
    ~largest_run:(largest_run orig)
    ~sample_count:(sample_count orig)
    ~samples

let calc_deltas ~orig_meas ~curr_meas =
  List.map2_exn orig_meas curr_meas ~f:calc_delta
