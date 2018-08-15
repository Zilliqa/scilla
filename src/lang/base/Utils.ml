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

module type Dictionary = sig

  type key = string

  type 'a dict

  val make_dict : unit -> 'a dict
  val insert : key -> 'a -> 'a dict -> 'a dict
  val lookup : key -> 'a dict -> 'a option

  val is_empty : 'a dict -> bool

  val to_list : 'a dict -> (key * 'a) list
end

(* Simple association list implementation of a dictionary.
   Note that old entries for a key k are removed when a new value is
   added for k. *)
module AssocDictionary : Dictionary = struct

  type key = string

  type 'a dict = (key * 'a) list

  let make_dict () = []

  let insert k v d =
    (k, v) :: (List.filter (fun (k', _) -> not (k = k')) d)

  let lookup k d =
    match List.find_opt (fun (kd, _) -> k = kd) d with
    | None -> None
    | Some (_, v) -> Some v

  let is_empty d =
    match d with
    | [] -> true
    | _ -> false

  let to_list d = d
end

exception InternalError of string
