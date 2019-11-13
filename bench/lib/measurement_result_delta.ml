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
open Measurement_result
open Float

let calc x y =
  { benchmark_name = y.benchmark_name;
    machine_id = y.machine_id;
    ocaml_version = y.ocaml_version;

    time_r_square = y.time_r_square - x.time_r_square;
    time_per_run_nanos = y.time_per_run_nanos - x.time_per_run_nanos;

    ci95_upper_bound = y.ci95_upper_bound - x.ci95_upper_bound;
    ci95_lower_bound = y.ci95_lower_bound - x.ci95_upper_bound;

    minor_words_per_run = y.minor_words_per_run - x.minor_words_per_run;
    major_words_per_run = y.major_words_per_run - x.major_words_per_run;

    promoted_words_per_run = y.promoted_words_per_run - x.promoted_words_per_run;
  }

let is_regression ~prev ~delta ~threshold =
  let one_percent = prev.time_per_run_nanos / 100.0 in
  let actual_threshold = one_percent * threshold in
  delta.time_per_run_nanos > actual_threshold
