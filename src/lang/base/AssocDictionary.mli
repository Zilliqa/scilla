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

type key = string

type 'a dict

(* Creates empty dictionary *)
val make_dict : unit -> 'a dict

(* Remove first occurence of key *)
val remove : key -> 'a dict -> 'a dict

(* Remove all occurences of key *)
val remove_all : key -> 'a dict -> 'a dict

(* Add key value pair, no duplicate checking *)
val insert : key -> 'a -> 'a dict -> 'a dict

(* Inserts all kv pairs from the first dictionary into the second dictionary, without checking for duplicates.  *)
val insert_all : 'a dict -> 'a dict -> 'a dict

(* Safe key lookup *)
val lookup : key -> 'a dict -> 'a option

(* Updates the first key only if it exists *)
val update : key -> 'a -> 'a dict -> 'a dict

(* Updates all existing key value pairs *)
val update_all : key -> 'a -> 'a dict -> 'a dict

(* Removes all matching kv pairs, making a new one *)
val insert_unique : key -> 'a -> 'a dict -> 'a dict

(* Filter based on predicate *)
val filter : f:(key -> bool) -> 'a dict -> 'a dict

(* Check if empty dictionary *)
val is_empty : 'a dict -> bool

(* Convert a dictionary to an assoc list *)
val to_list : 'a dict -> (key * 'a) list

(* Size of a dictionary *)
val size : 'a dict -> int

