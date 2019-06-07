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
  (* Remove the first occurrence of key. *)
  val remove : key -> 'a dict -> 'a dict
  (* Remove all occurrences of key. *)
  val remove_all : key -> 'a dict -> 'a dict
  (* Inserts new kv pair without checking for duplicates. *)
  val insert : key -> 'a -> 'a dict -> 'a dict
  val lookup : key -> 'a dict -> 'a option
  (* Updates the first key if it exists. No new entry created. *)
  val update : key -> 'a -> 'a dict -> 'a dict
  (* Updates all existing kv pairs without inserting any new entries. *)
  val update_all : key -> 'a -> 'a dict -> 'a dict
  (* Removes all matching kv pairs and creates a new one. *)
  val insert_unique : key -> 'a -> 'a dict -> 'a dict
  (* Only retain keys for which "fb k" is true. *)
  val filter : f:(key -> bool) -> 'a dict -> 'a dict

  val is_empty : 'a dict -> bool

  val to_list : 'a dict -> (key * 'a) list

  val size : 'a dict -> int
end

(* Simple association list implementation of a dictionary. *)
module AssocDictionary : Dictionary = struct

  type key = string

  type 'a dict = (key * 'a) list

  let make_dict () = []

  let rec remove k d =
    match d with
    | []              -> []
    | (kd, vd) :: rest -> if k = kd then rest else (kd, vd) :: (remove k rest)

  let rec remove_all k d =
    match d with
    | []              -> []
    | (kd, vd) :: rest -> if k = kd then (remove_all k rest) else (kd, vd) :: (remove_all k rest)

  let insert k v d =
    (k, v) :: d

  let lookup k d =
    match List.find_opt (fun (kd, _) -> k = kd) d with
    | None -> None
    | Some (_, v) -> Some v

  let rec update k v d =
    match d with
    | []               -> []
    | (kd, vd) :: rest -> if k = kd then (k, v) :: rest else (kd, vd) :: (update k v rest)

  let rec update_all k v d =
    match d with
    | []               -> []
    | (kd, vd) :: rest -> if k = kd then (k, v) :: (update_all k v rest) else (kd, vd) :: (update_all k v rest)

  let insert_unique k v d =
    let d' = remove_all k d in
    insert k v d'

  let filter ~f d =
    List.filter (fun (k, _) -> f k) d

  let is_empty d =
    match d with
    | [] -> true
    | _ -> false

  let to_list d = d

  let size d = List.length d
end

(* Add item a to list if it isn't already present. Use ~equal to check presence. *)
let list_add_unique ~equal ls a =
  if Core.List.mem ls a ~equal then ls else (a :: ls)

(* Fold n times, each time applying 0-(n-1) and accummulator to f. *)
let int_fold ~init ~(f : 'a -> int -> 'a) n =
  let rec recurser acc i =
    if i = n then acc else
    let acc' = f acc i in
    recurser acc' (i+1)
  in
  recurser init 0

open ErrorUtils
exception InternalError of scilla_error list
let mk_internal_error msg = InternalError (mk_error0 msg)
