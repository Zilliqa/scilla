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

open Core_bench
open Core_bench.Simplified_benchmark

val sort : Result.t list -> Result.t list
(** Sorts benchmark results by the [benchmark_name] field *)

val mk : Analysis_result.t list -> Result.t list
(** Make a list of regression results given
    the [Analysis_result.t list] *)

val save : Result.t list -> env:Env.t -> string
(** Save the benchmark results, return
    the directory where it was saved *)

val load_latest :
  timestamp:string option ->
  current:string option ->
  env:Env.t ->
  Result.t list option
(** Load the benchmark results from the specified [timestamp] directory.
    If the [timestamp] dir is not given then the results will
    be loaded by finding the directory named after the
    latest timestamp, which is not the same as the [current] directory *)

val calc_deltas :
  previous:Result.t list ->
  current:Result.t list ->
  Measurement_result_delta.t list
(** Compare the [previous] and [current], return [t] containing deltas.
    This function assumes that both lists are
    already sorted by the [benchmark_name] field *)

val detect_regressions :
  previous:Result.t list ->
  deltas:Measurement_result_delta.t list ->
  threshold:float ->
  unit
(** Check for significant performance regressions and
    raise an exception, if any *)
