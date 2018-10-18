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

exception Error of string

}

let digit = ['0'-'9']
let posint = digit+
let int = '-'? ['0'-'9'] ['0'-'9']*
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?
                     
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let cid =   ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let tid =   '\'' ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*                     
let lcomment = "(*" (_ # ['\r' '\n'])* "*)" white* newline
let hex = '0' 'x' ['a'-'f' 'A'-'F' '0'-'9']+
let intty = "Int32" | "Int64" | "Int128" | "Int256" | "Uint32" |
            "Uint64" | "Uint128" | "Uint256"
                                         
rule read =
  parse

  (* Whitespaces *)    
  | newline       { new_line lexbuf; read lexbuf }
  | lcomment      { new_line lexbuf; read lexbuf }
  | white         { read lexbuf }

  (* Numbers and hashes *)
  | int as i      { NUMLIT (big_int_of_string i) }
  | hex    as i   { HEXLIT i }

  (* Integer types *)
  | intty as i    { CID i }

  (* Keywords *)          
  | "forall"      { FORALL }      
  | "builtin"     { BUILTIN }      
  | "block"       { BLOCK }      
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
