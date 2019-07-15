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

{
open Lexing
open ScillaParser
open Big_int
open Base

exception Error of string

}

let digit = ['0'-'9']
let posint = digit+
let int = '-'? posint
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let alphanum = ['a'-'z' 'A'-'Z' '0'-'9' '_']
let id = ['a'-'z'] alphanum*
let spid = ['_'] alphanum*
let cid =   ['A'-'Z'] alphanum*
let tid =   '\'' ['A'-'Z'] alphanum*
let hexdigit = ['a'-'f' 'A'-'F' '0'-'9']
let hex = '0' 'x' (hexdigit hexdigit)+
let intty = "Int32" | "Int64" | "Int128" | "Int256" | "Uint32" |
            "Uint64" | "Uint128" | "Uint256"

rule read =
  parse

  (* Whitespaces *)    
  | newline       { new_line lexbuf; read lexbuf }
  | "(*"          { comment [lexbuf.lex_curr_p] lexbuf }
  | white         { read lexbuf }

  (* Numbers and hashes *)
  | int as i      { NUMLIT (big_int_of_string i) }
  | hex    as i   { HEXLIT i }

  (* Integer types *)
  | intty as i    { CID i }

  (* Keywords *)          
  | "forall"      { FORALL }      
  | "builtin"     { BUILTIN }      
  | "library"     { LIBRARY }
  | "import"      { IMPORT }
  | "let"         { LET }
  | "in"          { IN }
  | "match"       { MATCH }
  | "with"        { WITH }
  | "end"         { END }                 
  | "fun"         { FUN }                 
  | "tfun"        { TFUN }                 
  | "contract"    { CONTRACT }      
  | "transition"  { TRANSITION }      
  | "send"        { SEND }      
  | "event"       { EVENT }
  | "field"       { FIELD }      
  | "accept"      { ACCEPT }      
  | "exists"      { EXISTS }
  | "delete"      { DELETE }
  | "Emp"         { EMP }
  | "Map"         { MAP }
  | "scilla_version" { SCILLA_VERSION }
  | "type"        { TYPE }
  | "of"          { OF }
  | "try"         { TRY }
  | "catch"       { CATCH }
  | "as"          { AS }
  | "procedure"   { PROCEDURE }
  | "throw"       { THROW }


  (* Separators *)    
  | ';'           { SEMICOLON }
  | ':'           { COLON }
  | '.'           { PERIOD }      
  | '|'           { BAR }
  | '['           { LSQB }
  | ']'           { RSQB }
  | '('           { LPAREN }
  | ')'           { RPAREN }
  | "{"           { LBRACE }
  | "}"           { RBRACE }
  | ","           { COMMA }
  | "=>"          { ARROW }                  
  | "->"          { TARROW }                  
  | "="           { EQ }                  
  | "&"           { AND }                  
  | "<-"          { BIND }                  
  | ":="          { ASSIGN }                  
  | "@"           { AT }                  
  | "_"           { UNDERSCORE } 
  | '"'           { read_string (Buffer.create 17) lexbuf }

  (* Identifiers *)    
  | id as i       { ID i }
  | cid as i      { CID i }
  | tid as i      { TID i }
  | spid as i   { SPID i }

  (* Other tokens *)     
  | _             { raise (Error ("Unexpected character: " ^ Lexing.lexeme lexbuf)) }
  | eof           { EOF }

and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (Error ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (Error ("String is not terminated")) }

(* Nested comments, keeping a list of where comments open *)
and comment braces =
  parse
  | "(*"      { comment (lexbuf.lex_curr_p::braces) lexbuf}
  | "*)"      { match braces with
                  _::[] -> read lexbuf
                | _ -> comment (List.tl_exn braces) lexbuf }
  | newline   { new_line lexbuf; comment braces lexbuf}
  | _         { comment braces lexbuf}
  | eof       { lexbuf.lex_curr_p <- List.hd_exn braces; raise (Error ("Comment unfinished"))}
