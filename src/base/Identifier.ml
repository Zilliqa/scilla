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

open Core_kernel
open ErrorUtils

let flatten_name ns n = ns ^ "." ^ n

module type QualifiedName = sig
  type t [@@deriving sexp, equal, compare]

  val as_string : t -> string

  (* User-friendly string *)
  val as_error_string : t -> string

  val parse_simple_name : string -> t

  val parse_qualified_name : string -> string -> t
end

(* Localised names within a contract.
   Name qualifiers refer to the contract's import namespaces.
   Fields, parameters, variables and type variables are never qualified. *)
module LocalName = struct
  type t =
    (* A SimpleLocal is a name that is either defined within the module, or
       defined in an external library imported without a namespace, i.e.
       "import X" *)
    | SimpleLocal of string
    (* A QualifiedLocal is a name that is defined in an external library imported
       with a namespace, i.e., "import X as Y".
       The first string is the namespace "Y".
       The second string is the externally defined name. *)
    | QualifiedLocal of string * string
  [@@deriving sexp, equal, compare]

  let as_string = function
    | SimpleLocal n -> n
    | QualifiedLocal (ns, n) -> flatten_name ns n

  let as_error_string = as_string

  let parse_simple_name n = SimpleLocal n

  let parse_qualified_name ns n = QualifiedLocal (ns, n)
end

(* Global, canonical names.
   Name qualifiers refer to the defining library's address.
   Fields, parameters, variables and type variables are never qualified. *)
module GlobalName = struct
  type t_name =
    (* A SimpleGlobal is a name defined within the module. *)
    | SimpleGlobal of string (* name *)
    (* A QualifiedGlobal is a name that is defined in an external library.
       The first string is the address of the external library (which
       corresponds to the filename of the library file).
       The second string is the externally defined name. *)
    | QualifiedGlobal of string * string (* address and name *)
  [@@deriving sexp, equal, compare]

  type t = t_name * string (* error string *) [@@deriving sexp, equal, compare]

  let as_string = function
    | SimpleGlobal n, _ -> n
    | QualifiedGlobal (ns, n), _ -> flatten_name ns n

  let as_error_string = function _, s -> s

  let parse_simple_name n = (SimpleGlobal n, n)

  let parse_qualified_name ns n = (QualifiedGlobal (ns, n), flatten_name ns n)
end

module type ScillaIdentifier = sig
  module Name : QualifiedName

  type 'rep t = private Ident of Name.t * 'rep [@@deriving sexp]

  val mk_loc_id : Name.t -> loc t

  val mk_id : Name.t -> 'a -> 'a t

  val get_id : 'a t -> Name.t

  val get_rep : 'a t -> 'a

  val as_string : 'a t -> string

  val as_error_string : 'a t -> string

  (* A few utilities on id. *)
  val equal : 'a t -> 'b t -> bool

  val compare : 'a t -> 'b t -> int

  val dedup_id_list : 'a t list -> 'a t list

  val is_mem_id : 'a t -> 'a t list -> bool
end

module MkIdentifier (Name : QualifiedName) = struct
  module Name = Name

  type 'rep t = Ident of Name.t * 'rep [@@deriving sexp]

  let mk_id i r = Ident (i, r)

  let mk_loc_id i = mk_id i dummy_loc

  let get_id i = match i with Ident (x, _) -> x

  let get_rep i = match i with Ident (_, l) -> l

  let as_string i = Name.as_string (get_id i)

  let as_error_string i = Name.as_error_string (get_id i)

  (* A few utilities on id. *)
  let equal a b = Name.equal (get_id a) (get_id b)

  let compare a b = Name.compare (get_id a) (get_id b)

  let dedup_id_list l = List.dedup_and_sort ~compare l

  let is_mem_id i l = List.exists l ~f:(equal i)
end
