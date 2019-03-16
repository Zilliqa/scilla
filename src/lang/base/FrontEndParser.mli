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
open ParserUtil
open ErrorUtils

val parse_file : ((Lexing.lexbuf -> ScillaParser.token) ->
                  Lexing.lexbuf -> 'a) -> string -> ('a, scilla_error list) result

val parse_string : ((Lexing.lexbuf -> ScillaParser.token) ->
                  Lexing.lexbuf -> 'a) -> string -> ('a, scilla_error list) result

val parse_type : string -> (typ, scilla_error list) result

val parse_expr : string -> (ParsedSyntax.expr_annot, scilla_error list) result
