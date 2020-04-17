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

module type QualifiedName = sig
  type t [@@deriving sexp]

  val as_string : t -> string
  val as_error_string : t -> string

  val equal_name : t -> t -> bool
  val compare_name : t -> t -> int

  val parse_builtin_adt_name : string -> t
  val parse_variable_name : string -> t
end

val concat_qualifier_and_name : string -> string -> string

module FlattenedName : QualifiedName

module LocalName : QualifiedName

module CanonicalName : QualifiedName

module type Identifier = sig

  module Name : QualifiedName
  
  type 'rep t [@@deriving sexp]

  val asId : Name.t -> loc t
  val asIdL : Name.t -> 'a -> 'a t

  val get_id : 'a t -> Name.t
  val as_string : 'a t -> string
  val as_error_string : 'a t -> string
  val get_rep : 'a t -> 'a

  val parse_builtin_adt_name : string -> loc t
  val parse_variable_name : string -> 'a -> 'a t

  (* A few utilities on id. *)
  val equal_id : 'a t -> 'b t -> bool
  val compare_id : 'a t -> 'b t -> int

  val dedup_id_list : 'a t list -> 'a t list
  val is_mem_id : 'a t -> 'a t list -> bool

end

module MkIdentifier : functor (Name : QualifiedName) -> Identifier
