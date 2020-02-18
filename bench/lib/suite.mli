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

(** Types of the benchmark suites *)
type t =
  | Expressions  (** Benchmarks for standalone expressions *)
  | Contracts  (** Contract (and transition) benchmarks *)
[@@deriving compare]

val equal : t -> t -> bool

val all : t list

val load : t -> cfg:Config_t.config -> env:Env.t -> Test.t list
(** Load benchmark test suite *)

val of_string : string -> t

val arg_type : t Command.Arg_type.t
