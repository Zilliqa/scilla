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

(** Fails with an error if two measurements
    with the same [name] have a different number of samples. **)
val assert_length
  :  Measurement_sample.t array
  -> Measurement_sample.t array
  -> name:string
  -> unit

(** Calculate the absolute delta for each
    measurement of one run of the benchmark. *)
val calc
  :  Measurement_sample.t
  -> Measurement_sample.t
  -> Measurement_sample.t
