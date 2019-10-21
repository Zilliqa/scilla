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

(* How long to measure, 1 second should be enough *)
let time_quota_float = 1.0
let time_quota = Time.Span.of_sec time_quota_float

(* Usually, we only interested in the number of nano secs taken
   and the relative execution time as a percentage *)

let display_config =
  Bench.Display_config.create
    ~show_samples: false
    ~show_speedup: false
    ~show_percentage: true
    ()

let analysis_configs =
  [Analysis_config.nanos_vs_runs]

(* Override the default time quota *)
let run_config =
  Run_config.create ~time_quota ()

(* Save benchmark data *)
let save = true

(* Display benchmarking results *)
let display = true

(* Compare with previous benchmarking
   results (if available) and output the difference *)
let compare = true
