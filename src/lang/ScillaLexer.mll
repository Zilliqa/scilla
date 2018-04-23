{
open Lexing
open ScillaParser

exception Error of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
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
let lcomment = "(*" (_ # ['\r' '\n'])* "*)" newline
let sha3256 = ['a'-'f' '0'-'9']+
let address = '0' 'x' sha3256                
                                         
rule read =
  parse

  (* Whitespaces *)    
  | newline       { next_line lexbuf; read lexbuf }
  | lcomment      { next_line lexbuf; read lexbuf }
  | white         { read lexbuf }

  (* Identifiers *)    
  | id as i       { ID i }
  | cid as i      { CID i }

  (* Numbers and hashes *)
  | posint as i   { NUMLIT (int_of_string i) }
  | sha3256 as i  { SHA3LIT i }
  | address as i  { ADDRESS i }                 
                  
  (* Keywords *)          
  | "builtin"     { BUILTIN }      
  | "library"     { LIBRARY }
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
  | "field"       { FIELD }      
                  
  (* Separators *)    
  | ';'           { SEMICOLON }
  | ':'           { COLON }
  | '|'           { BAR }
  | '('           { LPAREN }
  | ')'           { RPAREN }
  | '<'           { LANGLE }
  | '>'           { RANGLE }
  | "{"           { LBRACE }
  | "}"           { RBRACE }
  | ","           { COMMA }
  | "=>"          { ARROW }                  
  | "="           { EQ }                  
  | "&"           { AND }                  
  | "<-"          { BIND }                  
  | ":="          { ASSIGN }                  
  | "@"           { AT }                  
  | "_"           { UNDERSCORE }                  
                  
  (* Other tokens *)     
  | _             { raise (Error ("Unexpected character: " ^ Lexing.lexeme lexbuf)) }
  | eof           { EOF }

