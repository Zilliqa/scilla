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

(* Simple association list implementation of a dictionary. *)
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

let insert_all other_d this_d =
  other_d @ this_d

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

