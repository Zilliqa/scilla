open Syntax
open Yojson

module JSONTypeUtilities = TypeUtil.TypeUtilities

(* check membership, fail if not *)
val member_exn : string -> Basic.t -> Basic.t

val constr_pattern_arg_types_exn : typ -> string -> typ list
val lookup_adt_name_exn : string -> Datatypes.adt
type adt_parser_entry =
    Incomplete
  | Parser of (Basic.t -> literal)

val adt_parsers : (string, adt_parser_entry) Core.Caml.Hashtbl.t

(* Add an ADT parser to the hash table *)
val add_adt_parser : string -> adt_parser_entry -> unit

val lookup_adt_parser : string -> adt_parser_entry

(* Safe version of lookup_adt_parser *)
val lookup_adt_parser_opt : string -> adt_parser_entry option

val gen_parser : typ -> Basic.t -> literal

(* Appliation of gen_parser *)
val parse_json : typ -> Basic.t -> literal
