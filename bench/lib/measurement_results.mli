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

(** Sorts benchmark results by the [benchmark_name] field. *)
val sort : Measurement_result.t list -> Measurement_result.t list

(** Make a list of regression results given
    the [Analysis_result.t list]. *)
val mk : Analysis_result.t list -> Measurement_result.t list

(** Save the benchmark results, return
    the directory where it was saved. *)
val save : Measurement_result.t list -> env:Env.t -> string

(** Load the benchmark results from the specified [timestamp] directory.
    If the [timestamp] dir is not given then the results will
    be loaded by finding the directory named after the
    latest timestamp, which is not the same as the [current] directory.
    Return the [t list] along with used timestamp (directory name)
    represented as a [string]. *)
val load_latest
  :  timestamp:string option
  -> current:string option
  -> env:Env.t
  -> (Measurement_result.t list * string) option

(** Compare the [previous] and [current], return [t] containing deltas.
    This function assumes that both lists are already sorted by the [benchmark_name] field.*)
val calc_deltas
  :  previous:(Measurement_result.t list)
  -> current:(Measurement_result.t list)
  -> Measurement_result.t list
