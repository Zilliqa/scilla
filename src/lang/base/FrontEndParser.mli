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

open Syntax
open ErrorUtils
open Lexing
open ParserUtil

module MInter = ScillaParser.MenhirInterpreter

(* Main parser *)
val parse_lexbuf : (position -> 'a MInter.checkpoint) -> lexbuf -> string -> ('a, scilla_error list) result

(* Parse a string using an entry point checkpoint starter *)
val parse_string : (position -> 'a MInter.checkpoint) -> string -> ('a, scilla_error list) result

(* Parse a file using an entry point checkpoint starter *)
val parse_file : (position -> 'a MInter.checkpoint) -> string -> ('a, scilla_error list) result

(* Parse a Scilla type *)
val parse_type : string -> (typ, scilla_error list) result

(* Parse an expression *)
val parse_expr : string -> (ParsedSyntax.expr_annot, scilla_error list) result

(* Read and expression from a file and parse it *)
val parse_expr_from_file : string -> (ParsedSyntax.expr_annot, scilla_error list) result

(* Parse a library module *)
val parse_lmodule : string -> (ParsedSyntax.lmodule, scilla_error list) result

(* Parse a contract module *)
val parse_cmodule : string -> (ParsedSyntax.cmodule, scilla_error list) result
