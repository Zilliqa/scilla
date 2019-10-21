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

(** Print the benchmark groups along with their tests. *)
val print_tests : Test.t list -> unit

(* TODO: In the future we might want to print the original, current and deltas side-by-side,
   but after taking a quick look at the [Core_bench.Display] module I've concluded that it
   might make sense to postpone that *)

(** Print the comparison results (deltas). *)
val print_deltas : (Measurement.t list * Analysis_result.t list) -> unit
