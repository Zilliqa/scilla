open Core
open ErrorUtils

module type Dictionary =
  sig
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
    val to_list : 'a dict -> (key * 'a) list
    val size : 'a dict -> int
  end

module AssocDictionary : Dictionary

(* Add to list only if unique considering equal *)
val list_add_unique :
  equal:('a -> 'a -> bool) -> 'a List.t -> 'a -> 'a List.t

val int_fold : init:'a -> f:('a -> int -> 'a) -> int -> 'a

exception InternalError of scilla_error list
val mk_internal_error : string -> exn
