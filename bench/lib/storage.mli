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

(** List paths containing benchmark results *)
val ls : env:Env.t -> string list

(** Given the [timestamp] of the benchmark results to
    compare with return a directory named after that timestamp,
    otherwise return the latest one, if it exists and
    it is not the same as the [current] timestamp *)
val latest
  :  timestamp:string option
  -> current:string option
  -> env:Env.t
  -> string option
