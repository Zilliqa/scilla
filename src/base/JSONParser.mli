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
module JSONSanitisedLiteral = TypeUtil.TULiteral

(* Specialised literal type for JSON parsing. Needed in order to parse unrecognised ADT literals *)
type json_literal =
  | StringLit of string
  (* Cannot have different integer literals here directly as Stdint does not derive sexp. *)
  | IntLit of JSONSanitisedLiteral.int_lit
  | UintLit of JSONSanitisedLiteral.uint_lit
  | BNum of string
  (* Byte string with a statically known length. *)
  | ByStrX of JSONSanitisedLiteral.Bystrx.t
  (* Byte string without a statically known length. *)
  | ByStr of JSONSanitisedLiteral.Bystr.t
  (* Message: an associative array *)
  | Msg of (string * JSONType.t * json_literal) list
  (* A dynamic map of literals *)
  | Map of JSONSanitisedLiteral.mtype * (json_literal, json_literal) Sexplib.Std.Hashtbl.t
  (* A constructor in HNF *)
  | ADTValue of JSONIdentifier.Name.t * JSONType.t list * json_literal list
  | Unrecognised of string

val build_nil_lit : JSONType.t -> json_literal
val build_cons_lit : json_literal -> JSONType.t -> json_literal -> json_literal

val sanitise_literal : json_literal -> JSONSanitisedLiteral.t
val map_info : json_literal -> JSONType.t * JSONType.t * json_literal

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

type adt_parser_entry = Incomplete | Parser of (Basic.t -> json_literal)

(* ADT parsers table *)
val adt_parsers : (string, adt_parser_entry) Caml.Hashtbl.t

(* Put an ADT parser to the table *)
val add_adt_parser : string -> adt_parser_entry -> unit

(* Safe lookup of an ADT parser in the table *)
val lookup_adt_parser_opt : string -> adt_parser_entry option

(* Look up an ADT parser in the table, throws if not found *)
val lookup_adt_parser : string -> adt_parser_entry

(* Generate a parser *)
val gen_parser : JSONType.t -> Basic.t -> json_literal

(* Parse JSON *)
val parse_json : JSONType.t -> Basic.t -> json_literal
