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

(**********************************************************)
(*   Dictionaries, i.e., maps from strings to values      *)
(**********************************************************)

open Core_kernel
open! Int.Replace_polymorphic_compare

(* Simple association list implementation of a dictionary. *)
type key = string

type 'a dict = (key * 'a) list

let make_dict () = []

(* removes just the first key-value binding, if it exists *)
let rec remove k d =
  match d with
  | [] -> []
  | (kd, vd) :: rest ->
      if String.(k = kd) then rest else (kd, vd) :: remove k rest

let remove_all k d = List.Assoc.remove d k ~equal:String.( = )

let insert k v d = (k, v) :: d

let insert_all other_d this_d = other_d @ this_d

let lookup k d = List.Assoc.find d k ~equal:String.( = )

(* updates just the first key-value binding, if it exists *)
let rec update k v d =
  match d with
  | [] -> []
  | (kd, vd) :: rest ->
      if String.(k = kd) then (k, v) :: rest else (kd, vd) :: update k v rest

let update_all k v d = List.Assoc.add d k v ~equal:String.( = )

let insert_unique k v d = List.Assoc.add d k v ~equal:String.( = )

let filter ~f d = List.filter d ~f:(fun (k, _) -> f k)

let is_empty d = List.is_empty d

let to_list d = d

let size d = List.length d
