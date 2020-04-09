(*
  This file is part of scilla.

  Copyright (c) 2020 - present Zilliqa Research Pvt. Ltd.
  
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

open ErrorUtils

type 'rep t = Ident of string * 'rep [@@deriving sexp]

val asId : string -> loc t

val asIdL : string -> 'a -> 'a t

val get_id : 'a t -> string

val get_rep : 'a t -> 'a

(* A few utilities on id. *)
val equal_id : 'a t -> 'b t -> bool

val compare_id : 'a t -> 'b t -> int

val dedup_id_list : 'a t list -> 'a t list

val is_mem_id : 'a t -> 'a t list -> bool
