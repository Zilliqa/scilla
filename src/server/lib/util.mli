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

(** Helper function similar to [Exn.protect],
    except that it re-raises exceptions. *)
val protect_reraise : f:(unit -> 'a) -> finally:(unit -> unit) -> 'a

(** Helper function to create a directory with
    the given permissions if it doesn't already exist. *)
val mkdir_rec : dir:string -> perm:int -> unit

(** Send msg with delimiting character "0xA". **)
val send_delimited : out_channel -> string -> unit
