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
open ScillaUtil.FilePathInfix

module B = Core_bench

let result_to_option = function
  | Error err ->
      eprintf "Error %s\n%!" (Error.to_string_hum err);
      None
  | Ok r -> Some r

let load_from path =
  path
  |> Sys.ls_dir
  |> List.map ~f:(fun fn ->
      let filename = path ^/ fn in
      B.Measurement.load ~filename)

let load ~dir ~current_dir ~env =
  let open Env in
  let mk_path dir = env.results_dir ^/ dir in
  (* Get paths containing measurement results *)
  let paths =
    env.results_dir
    |> Sys.ls_dir
    |> Timestamp.sort_desc
    |> List.filter ~f:(fun dir ->
        let path = mk_path dir in
        Sys.is_directory_exn path) in
  (* Helper function to find dir with
     the latest (previous) measurements *)
  let find_latest () =
    let paths =
      match current_dir with
      | None -> paths
      | Some s -> List.filter paths ~f:(fun dir -> dir <> s)
    in List.hd paths
  in
  (* If we're given the timestamp of measurements to compare with
     then try to find a directory named after that timestamp,
     otherwise just find the latest one, if it exists *)
  let path =
    match dir with
    | Some s -> List.find paths ~f:(fun p -> p = s)
    | None -> find_latest () in
  Option.map path ~f:(fun s ->
      load_from (mk_path s), s)

let sanitize =
  String.map ~f:(fun c ->
      if Char.is_alphanum c || String.mem "-_." c
      then c
      else '_')

let save_one m ~path =
  let name = m |> B.Measurement.name |> sanitize in
  let filename = path ^/ name ^. "txt" in
  B.Measurement.save m ~filename

let save meas ~env =
  (* Save the measurements in a more
     appropriate place in the file system *)
  let dir = Timestamp.mk () in
  let path = env.Env.results_dir ^/ dir in
  printf "Measurements will be saved to %s.\n%!" dir;
  (* Create a directory to place measurements into,
     use the current timestamp as a name for it *)
  Unix.mkdir path;
  (* Save each measurement as a separate file *)
  List.iter meas ~f:(save_one ~path);
  dir

(* Run the [B.analyze] for each
   measurement and get back the results *)
let analyze meas =
  let analyze_one m = B.Analysis.analyze m Defaults.analysis_configs in
  meas
  |> List.map ~f:analyze_one
  |> List.filter_map ~f:result_to_option
