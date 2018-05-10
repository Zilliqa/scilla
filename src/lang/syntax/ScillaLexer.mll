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
let lcomment = "(*" (_ # ['\r' '\n'])* "*)" newline
let hex = '0' 'x' ['a'-'f' '0'-'9']+
                                         
rule read =
  parse

  (* Whitespaces *)    
  | newline       { new_line lexbuf; read lexbuf }
  | lcomment      { new_line lexbuf; read lexbuf }
  | white         { read lexbuf }

  (* Numbers and hashes *)
  | posint as i   { NUMLIT (big_int_of_string i) }
  | hex    as i   { HEXLIT i }
                  
  (* Keywords *)          
  | "builtin"     { BUILTIN }      
  | "block"       { BLOCK }      
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
  | "accept"      { ACCEPT }      
  | "Emp"         { EMP }
  | "Map"         { MAP }


  (* Separators *)    
  | ';'           { SEMICOLON }
  | ':'           { COLON }
  | '|'           { BAR }
  | '('           { LPAREN }
  | ')'           { RPAREN }
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
  (* | '<'           { LANGLE }
   * | '>'           { RANGLE } *)

  (* Identifiers *)    
  | id as i       { ID i }
  | cid as i      { CID i }

  (* Other tokens *)     
  | _             { raise (Error ("Unexpected character: " ^ Lexing.lexeme lexbuf)) }
  | eof           { EOF }

