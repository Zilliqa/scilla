(*
 * Copyright (c) 2018 - present.
 * Zilliqa, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
