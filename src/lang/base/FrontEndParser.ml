(*
 * Copyright (c) 2018 - present Zilliqa, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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

