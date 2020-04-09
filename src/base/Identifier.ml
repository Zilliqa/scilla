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

open Core_kernel
open ErrorUtils

type 'rep t = Ident of string * 'rep [@@deriving sexp]

let asId i = Ident (i, dummy_loc)

let asIdL i loc = Ident (i, loc)

let get_id i = match i with Ident (x, _) -> x

let get_rep i = match i with Ident (_, l) -> l

(* A few utilities on id. *)
let equal_id a b = String.(get_id a = get_id b)

let compare_id a b = String.(compare (get_id a) (get_id b))

let dedup_id_list l = List.dedup_and_sort ~compare:compare_id l

let is_mem_id i l = List.exists l ~f:(equal_id i)
