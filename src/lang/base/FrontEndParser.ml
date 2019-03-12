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
open PrettyPrinters
open ErrorUtils

(* TODO: Use DebugMessage perr/pout instead of fprintf. *)
let print_err msg lexbuf  =
  let e = mk_error1 msg (toLoc lexbuf.lex_curr_p) in
  let msg' = scilla_error_to_string e in
  printf "%s\n" msg'

let parse_lexbuf parser lexbuf filename =
  lexbuf.lex_curr_p  <- { lexbuf.lex_curr_p with pos_fname = filename };
  try
    let expr = parser ScillaLexer.read lexbuf in
    Some expr
  with
  | ScillaLexer.Error msg ->
      print_err ("Lexical error: " ^ msg) lexbuf;
      None
  | Syntax.SyntaxError msg ->
      print_err ("Syntax error: " ^ msg) lexbuf;
      None
  | ScillaParser.Error ->
      print_err "Syntax error." lexbuf;
      None

let parse_file parser filename  =
  In_channel.with_file filename ~f:(fun inx ->
    let lexbuf = Lexing.from_channel inx in
    parse_lexbuf parser lexbuf filename)

let parse_string parser s =
  let lexbuf = Lexing.from_string s in
  parse_lexbuf parser lexbuf "Prelude"

let parse_type s =
  match parse_string ScillaParser.type_term s with
  | Some t -> t
  | _ -> raise ScillaParser.Error

let parse_expr s =
  match parse_string ScillaParser.exp_term s with
  | Some e -> e
  | _ -> raise ScillaParser.Error

