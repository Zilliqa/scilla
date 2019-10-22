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

let calc_one orig curr =
  let open Measurement in
  (* print_endline @@ (sprintf "orig measurement name: %s\n" (name orig));
   * print_endline @@ (sprintf "curr measurement name: %s\n" (name curr)); *)
  let orig_samples = samples orig in
  let curr_samples = samples curr in
  (* Let's make sure we have the same
     number of samples for each measurement. *)
  Sample_delta.assert_length
    (samples orig) (samples curr) ~name:(name orig);
  let samples = Array.map2_exn
      orig_samples curr_samples
      ~f:Sample_delta.calc
  in
  create
    ~name:(name orig ^ "_deltas")
    ~test_name:(test_name orig)
    ~file_name:(file_name orig)
    ~module_name:(module_name orig)
    ~largest_run:(largest_run orig)
    ~sample_count:(sample_count orig)
    ~samples

let sort =
  List.sort ~compare:(fun x y ->
      Measurement.(String.compare (name x) (name y)))

let calc_all orig curr =
  List.map2_exn (sort orig) (sort curr) ~f:calc_one
