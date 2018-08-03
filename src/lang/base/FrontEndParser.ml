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

(* TODO: Use DebugMessage perr/pout instead of fprintf. *)

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "file %s: line %d, position %d." pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_file parser filename =
  In_channel.with_file filename ~f:(fun inx ->
      let lexbuf = Lexing.from_channel inx in
      lexbuf.lex_curr_p  <- { lexbuf.lex_curr_p with pos_fname = filename };
      try
        let exprs = parser ScillaLexer.read lexbuf in
        Some exprs
      with
      | ScillaLexer.Error msg 
      | Syntax.SyntaxError msg ->
          fprintf stderr "Syntax error in %a: %s\n" print_position lexbuf msg;
          None
      | ScillaParser.Error ->
          fprintf stderr "Syntax error in %a\n" print_position lexbuf;
          None)
    
let parse_string parser s =
  let lexbuf = Lexing.from_string s in
  lexbuf.lex_curr_p  <- { lexbuf.lex_curr_p with pos_fname = "Prelude" };
  try
    let exprs = parser ScillaLexer.read lexbuf in
    Some exprs
  with
  | ScillaLexer.Error msg
  | Syntax.SyntaxError msg ->
      printf "Lexical error in %a: %s\n" print_position lexbuf msg;
      fprintf stderr "Lexical in %a: %s\n" print_position lexbuf msg;
      None
  | ScillaParser.Error ->
      printf "Syntax error in %a\n" print_position lexbuf;
      fprintf stderr "Syntax error in %a\n" print_position lexbuf;
      None

let parse_type s =
  match parse_string ScillaParser.types s with
  | Some [t] -> t
  | _ -> raise ScillaParser.Error

let parse_expr s =
  match parse_string ScillaParser.exps s with
  | Some [e] -> e
  | _ -> raise ScillaParser.Error

