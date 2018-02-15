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

(* exception Scanning_error of Lexing.position * string *)
(* exception Syntax_error of Lexing.position *)

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_file filename =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  try
    let exprs = SParser.exps SLexer.read lexbuf in
    Some exprs
  with
  | SLexer.Error msg ->
      fprintf stderr "%a: %s\n" print_position lexbuf msg;
      None
  | SParser.Error ->
      fprintf stderr "At offset %d: syntax error.\n%!" (Lexing.lexeme_start lexbuf);
      None
