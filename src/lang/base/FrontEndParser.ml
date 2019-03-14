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


open Core
open Lexing
open ErrorUtils

(* TODO: Use DebugMessage perr/pout instead of fprintf. *)
let fail_err msg lexbuf  =
  let e = mk_error1 msg (toLoc lexbuf.lex_curr_p) in
  MonadUtil.fail e

let parse_lexbuf parser lexbuf filename =
  lexbuf.lex_curr_p  <- { lexbuf.lex_curr_p with pos_fname = filename };
  try
    let expr = parser ScillaLexer.read lexbuf in
    MonadUtil.pure @@ expr
  with
  | ScillaLexer.Error msg ->
    fail_err ("Lexical error: " ^ msg) lexbuf
  | Syntax.SyntaxError msg ->
    fail_err ("Syntax error: " ^ msg) lexbuf
  | ScillaParser.Error ->
    fail_err "Syntax error." lexbuf

let parse_file parser filename  =
  In_channel.with_file filename ~f:(fun inx ->
    let lexbuf = Lexing.from_channel inx in
    parse_lexbuf parser lexbuf filename)

let parse_string parser s =
  let lexbuf = Lexing.from_string s in
  parse_lexbuf parser lexbuf "Prelude"

let parse_type s =
  parse_string ScillaParser.type_term s

let parse_expr s =
  parse_string ScillaParser.exp_term s

