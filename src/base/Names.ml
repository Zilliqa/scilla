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
open! Int.Replace_polymorphic_compare

(* Signature for qualified names *)
module type QualifiedNames = sig
  type name [@@deriving sexp]

  val as_string : name -> string

  val as_error_string : name -> string
  
  val equal_name : name -> name -> bool

  val compare_name : name -> name -> int
end

let concat_qualifer_and_name q id = q ^ "." ^ id

(* Flattened names. This will be removed later *)
module FlattenedNames : QualifiedNames =
struct
  type name = string [@@deriving sexp]

  let as_string n = n

  let as_error_string n = n
  
  let equal_name = String.(=)

  let compare_name = String.compare
end

(* Locally defined qualified names *)
module LocalNames : QualifiedNames =
struct
  type name =
    | Simple of string
    | NamespaceQualified of string * string
  [@@deriving sexp]

  let as_string = function
    | Simple n -> n
    | NamespaceQualified (ns, n) -> concat_qualifer_and_name ns n

  let as_error_string = as_string
  
  let equal_name a b =
    match a, b with
    | Simple an, Simple bn
      when String.(an = bn)
      -> true
    | NamespaceQualified (ans, an), NamespaceQualified (bns, bn)
      when String.(ans = bns) && String.(an = bn)
      -> true
    | _, _ -> false

  let compare_name a b = String.compare (as_string a) (as_string b)
end

(* Canonical (i.e., globally uniquely) defined qualified names *)
module CanonicalNames : QualifiedNames =
struct
  type name =
    | Simple of string
    | AddressQualified of string * string
  [@@deriving sexp]

  let as_string = function
    | Simple n -> n
    | AddressQualified (adr, n) -> concat_qualifer_and_name adr n

  (* TODO: Report local names rather than canonical names *)
  let as_error_string = as_string
  
  let equal_name a b =
    match a, b with
    | Simple an, Simple bn
      when String.(an = bn)
      -> true
    | AddressQualified (aadr, an), AddressQualified (badr, bn)
      when String.(aadr = badr) && String.(an = bn)
      -> true
    | _, _ -> false

  let compare_name a b = String.compare (as_string a) (as_string b)
end
