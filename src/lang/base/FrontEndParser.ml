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
open MonadUtil
open ParserFaults

module MInter = ScillaParser.MenhirInterpreter

(* TODO: Use DebugMessage perr/pout instead of fprintf. *)
let fail_err msg lexbuf =
  fail1 msg (toLoc lexbuf.lex_curr_p)

let parse_lexbuf checkpoint_starter lexbuf filename =
  lexbuf.lex_curr_p  <- { lexbuf.lex_curr_p with pos_fname = filename };
  (* Supply of tokens *)
  let supplier = MInter.lexer_lexbuf_to_supplier ScillaLexer.read lexbuf in
  (* Parsing checkpoint, determines what we parse for *)
  let checkpoint = checkpoint_starter lexbuf.lex_curr_p in
  let success a = pure a in
  let failure state_error =
    let env = match state_error with
    | MInter.HandlingError env -> env
    (* failure only called for HandlingError or Reject
     * but Reject never happens as we finish parsing here *)
    | _ -> assert false in
    let state_number = MInter.current_state_number env in
    let error_message =
      try message state_number with
      | Caml.Not_found ->
      (Printf.sprintf "Syntax error, state number %d" state_number) in
    fail_err error_message lexbuf in
  try MInter.loop_handle success failure supplier checkpoint
  with
  | ScillaLexer.Error msg -> fail_err ("Lexical error: " ^ msg) lexbuf
  | Syntax.SyntaxError (msg, loc) -> fail1 ("Syntax error: " ^ msg) loc
  | ScillaParser.Error -> fail_err "Syntax error." lexbuf

let parse_file checkpoint_starter filename  =
  In_channel.with_file filename ~f:(fun inx ->
    let lexbuf = Lexing.from_channel inx in
    parse_lexbuf checkpoint_starter lexbuf filename)

let parse_string checkpoint_starter s =
  let lexbuf = Lexing.from_string s in
  parse_lexbuf checkpoint_starter lexbuf "Prelude"

let parse_type s =
  parse_string ScillaParser.Incremental.type_term s

let parse_expr s =
  parse_string ScillaParser.Incremental.exp_term s

