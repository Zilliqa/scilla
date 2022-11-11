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
open ParserUtil
open Literal

module ScillaFrontEndParser (Literal : ScillaLiteral) = struct
  (* Module files, expression file and type files use LocalLiteral, because
     name qualifiers are locally defined namespaces. *)
  module FESyntax = ParserSyntax (Literal)
  module Parser = ScillaParser.Make (FESyntax)
  module Lexer = ScillaLexer.MkLexer (FESyntax)
  module MInter = Parser.MenhirInterpreter
  module FEPType = FESyntax.SType
  module FEPIdentifier = FEPType.TIdentifier

  module FEPIdentifierComp = struct
    include FEPIdentifier.Name
    include Comparable.Make (FEPIdentifier.Name)
  end

  module FEPIdentifierSet = Set.Make (FEPIdentifierComp)

  let emp_idset = FEPIdentifierSet.empty

  (* TODO: Use DebugMessage perr/pout instead of fprintf. *)
  let fail_err msg lexbuf = fail1 ~kind:msg ?inst:None (toLoc lexbuf.lex_curr_p)

  (** Disambiguates calls of procedures without values and pure function calls
      and variables.
      They have the same syntax: [id = proc param1 param2] or [id = proc].
      Therefore, the parser doesn't know what is actually is called and saves
      such cases as [Bind(id, App(proc, [param1, param2]))] or
      [Bind(id, Var(proc)].
      This function finishes parsing and places [CallProc(Some(id), ...)]
      statements when the contract contains a procedure [id]. *)
  let disambiguate_calls cmod =
    let open FESyntax in
    let procedures_with_return =
      List.fold_left cmod.contr.ccomps ~init:emp_idset ~f:(fun s comp ->
          if Option.is_some comp.comp_return then
            FEPIdentifierSet.add s (FEPIdentifier.get_id comp.comp_name)
          else s)
    in
    let disambiguate_stmt (stmt, annot) =
      match stmt with
      | Bind (id, (App (f, args), _))
        when Set.mem procedures_with_return (FEPIdentifier.get_id f) ->
          (CallProc (Some id, f, args), annot)
      | Bind (id, (Var f, _))
        when Set.mem procedures_with_return (FEPIdentifier.get_id f) ->
          (CallProc (Some id, f, []), annot)
      | _ -> (stmt, annot)
    in
    let contr' =
      {
        cmod.contr with
        ccomps =
          List.map cmod.contr.ccomps ~f:(fun comp ->
              {
                comp with
                comp_body =
                  List.map comp.comp_body ~f:(fun stmt ->
                      disambiguate_stmt stmt);
              });
      }
    in
    { cmod with contr = contr' }

  let parse_lexbuf checkpoint_starter lexbuf filename =
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
    (* Supply of tokens *)
    let supplier = MInter.lexer_lexbuf_to_supplier Lexer.read lexbuf in
    (* Parsing checkpoint, determines what we parse for *)
    let checkpoint = checkpoint_starter lexbuf.lex_curr_p in
    let success a = pure a in
    let failure state_error =
      let env =
        match state_error with
        | MInter.HandlingError env -> env
        (* failure only called for HandlingError or Reject
         * but Reject never happens as we finish parsing here *)
        | _ -> assert false
      in
      let state_number = MInter.current_state_number env in
      let error_message =
        try message state_number
        with Caml.Not_found ->
          Printf.sprintf "Syntax error, state number %d" state_number
      in
      fail_err error_message lexbuf
    in
    try MInter.loop_handle success failure supplier checkpoint with
    | Lexer.Error msg -> fail_err ("Lexical error: " ^ msg) lexbuf
    | Syntax.SyntaxError (msg, loc) ->
        fail1 ~kind:("Syntax error: " ^ msg) ?inst:None loc
    | Parser.Error -> fail_err "Syntax error." lexbuf

  let parse_file checkpoint_starter filename =
    In_channel.with_file filename ~f:(fun inx ->
        let lexbuf = Lexing.from_channel inx in
        parse_lexbuf checkpoint_starter lexbuf filename)

  let parse_string checkpoint_starter s =
    let lexbuf = Lexing.from_string s in
    parse_lexbuf checkpoint_starter lexbuf "Prelude"

  let parse_stdin checkpoint_starter =
    let lexbuf = Lexing.from_channel Stdio.stdin in
    parse_lexbuf checkpoint_starter lexbuf "Prelude"

  let parse_type s = parse_string Parser.Incremental.type_term s
  let parse_expr s = parse_string Parser.Incremental.exp_term s

  let parse_expr_from_file filename =
    parse_file Parser.Incremental.exp_term filename

  let parse_expr_from_stdin () = parse_stdin Parser.Incremental.exp_term
  let parse_lmodule filename = parse_file Parser.Incremental.lmodule filename

  let parse_cmodule filename =
    let open Result.Let_syntax in
    let%bind cmod = parse_file Parser.Incremental.cmodule filename in
    pure @@ disambiguate_calls cmod

  let get_comments () = Lexer.get_comments ()
end
