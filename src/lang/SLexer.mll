{
open Lexing
open SParser

exception Error of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let int = '-'? ['0'-'9'] ['0'-'9']*

let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let varid = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let cid =   ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let lcomment = "//" (_ # ['\r' '\n'])* newline
  
rule read =
  parse
  | newline     { next_line lexbuf; read lexbuf }
  | lcomment    { next_line lexbuf; read lexbuf }
  | varid as i  { VAR i }
  | white       { read lexbuf }
  | "not"       { NOT }      
  | '('         { LPAREN }
  | ')'         { RPAREN }
  | ';'         { SEMICOLON }
  | "&&"        { AND }
  | "||"        { OR }
  | _           { raise (Error ("Unexpected character: " ^ Lexing.lexeme lexbuf)) }
  | eof         { EOF }

