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

(** Save benchmark measurements, return
    the directory where it was saved. *)
val save : Measurement.t list -> env:Env.t -> string

(** Load measurements from the specified [dir].
    If the [dir] is not given then previous measurements will
    be loaded by finding the directory named after the
    latest timestamp, which is not the same as the [current_dir].
    Return the [Measurement.t list] along with
    used timestamp (directory name) represented as a [string]. *)
val load
  :  dir:string option
  -> current_dir:string option
  -> env:Env.t
  -> (Measurement.t list * string) option

(** Analyze benchmark measurements. *)
val analyze : Measurement.t list -> Analysis_result.t list
