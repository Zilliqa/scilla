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
open Yojson
module JSONTypeUtilities = TypeUtil.TypeUtilities
module JSONIdentifier = TypeUtil.TUIdentifier
module JSONName = JSONIdentifier.Name
module JSONType = TypeUtil.TUType
module JSONLiteral = TypeUtil.TULiteral

(*************************************)
(***** Exception and wrappers ********)
(*************************************)

val json_exn_wrapper : ?filename:string -> (unit -> 'a) -> 'a

(* member_exn k obj returns the value associated with the key k in the JSON object obj,
   or raises an exception if k is not present in obj. *)
val member_exn : string -> Basic.t -> Basic.t

(* String JSON value to OCaml string with exceptions translated. *)
val to_string_exn : Yojson.Basic.t -> string

(* Wrapper for constr_pattern_arg_types. Throws Invalid_json exception instead of using result type *)
val constr_pattern_arg_types_exn : JSONType.t -> JSONName.t -> JSONType.t list

(*  Wrapper for DataTypeDictionary.lookup_name  *)
val lookup_adt_name_exn : 'a JSONIdentifier.t -> Datatypes.adt

(*************************************)
(*********** ADT parsers *************)
(*************************************)

type adt_parser_entry = Incomplete | Parser of (Basic.t -> JSONLiteral.t)

(* ADT parsers table *)
val adt_parsers : (string, adt_parser_entry) Caml.Hashtbl.t

(* Put an ADT parser to the table *)
val add_adt_parser : string -> adt_parser_entry -> unit

(* Safe lookup of an ADT parser in the table *)
val lookup_adt_parser_opt : string -> adt_parser_entry option

(* Look up an ADT parser in the table, throws if not found *)
val lookup_adt_parser : string -> adt_parser_entry

(* Generate a parser *)
val gen_parser : JSONType.t -> Basic.t -> JSONLiteral.t

(* Parse JSON *)
val parse_json : JSONType.t -> Basic.t -> JSONLiteral.t
