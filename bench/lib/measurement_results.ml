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

let sort =
  List.sort ~compare:(fun x y ->
      Measurement_result.(String.compare x.benchmark_name y.benchmark_name))

let mk results =
  results
  |> List.map ~f:Measurement_result.mk
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
  let path = Storage.latest ~timestamp ~current ~env in
  Option.map path ~f:(fun s -> load_from (env.Env.results_dir ^/ s), s)

let calc_deltas ~previous ~current =
  (* We assume that both lists are already sorted *)
  List.map2_exn previous current
    ~f:Measurement_result.calc_delta
