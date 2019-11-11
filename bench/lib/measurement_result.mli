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

(* We have to repeat the [Simplified_benchmark] module functionality here.
   Not all of it, just the parts we need to be able to save/load the regression results. *)

type t =
  { benchmark_name : string; (** User defined name for the benchmark test. *)
    machine_id : string; (** Stores the name of the machine used to run the benchmarks. *)
    ocaml_version : string;

    (* Various stats computed by bench: **)
    time_r_square : float;
    time_per_run_nanos : float;
    ci95_upper_bound : float;
    ci95_lower_bound : float;
    minor_words_per_run : float;
    major_words_per_run : float;
    promoted_words_per_run : float;
  } [@@deriving sexp]

(** Make an actual regression result given the [Analysis_result.t]. *)
val mk : Analysis_result.t -> t

(** Save the benchmark result at the given [path]. *)
val save : t -> path:string -> unit

(** Load the benchmark result from the specified path. *)
val load : string -> t

(** Compare the previous and current results,
     return [t] containing deltas. *)
val calc_delta : t -> t -> t
