(*
 * Copyright (c) 2018 - present Zilliqa, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)


%{
  open Syntax
%}

(* Identifiers *)    
%token <string> ID
%token <string> CID

(* Numbers and hashes *)
%token <int> NUMLIT
%token <string> SHA3LIT
%token <string> ADDRESS
                  
(* Separators *)    
%token SEMICOLON
%token COLON
%token BAR
%token LPAREN
%token RPAREN
%token ARROW
%token AT
%token UNDERSCORE
%token LBRACE       
%token RBRACE
%token LANGLE
%token RANGLE
%token COMMA
%token EQ
%token AND
%token BIND
%token ASSIGN
       
(* Keywords *)    
%token BUILTIN
%token EMP
%token LIBRARY
%token FIELD
%token LET
%token IN
%token MATCH
%token WITH
%token END       
%token FUN
%token TFUN
%token CONTRACT       
%token TRANSITION
%token SEND
%token ACCEPT
       
(*  Other tokens *)
%token EOF

(* Associativity *)

(* %left PLUS *)
(* %nonassoc NEG *)

%start <unit Syntax.expr list> exps

%%

exp:
| f = simple_exp {f}    
| LET; x = ID;
  t = ioption(type_annot) 
  EQ; f = simple_exp; IN; e = exp
  {Let ((Ident (x, ())), t, f, e) }
                                             
simple_exp :    
(* Function *)    
| FUN;
  LPAREN; i = ID; COLON; t = typ; RPAREN;
  ARROW; e = exp
  { Fun (Ident (i, ()), t, e) } 
(* Application *)  
| f = ID;
  args = nonempty_list(ID)
  { let xs = List.map (fun i -> Ident (i, ())) args
    in App ((Ident (f, ())), xs) }
(* Atomic expression *)
| a = atomic_exp {a} 
(* TODO Built-in call *)
| BUILTIN; b = ID; args = nonempty_list(ID)
  { let xs = List.map (fun i -> Ident (i, ())) args
    in Builtin ((Ident (b, ())), xs) }
(* TODO: Match expression *)
  
(* TODO Message construction *)

(* TODO: Patterns *)

(* TODO: Statements *)

  
  
type_annot:
| COLON; t = typ {t}
      
typ :
| t = CID { match t with
            | "Int" | "Hash" | "Address" -> PrimType t
            | s -> ADT (s, []) }   

atomic_exp :
| n = NUMLIT   { Literal (IntLit n) }
| i = ID       { Var (Ident (i, ())) }
| a = ADDRESS  { Literal (Address a) }
| h = SHA3LIT  { Literal (Sha256 h) }
        
(* | NOT p = exp { Not p } *)
(* | p1 = exp AND p2 = exp { And (p1, p2) }  *)
(* | p1 = exp OR  p2 = exp { Or  (p1, p2) }  *)
(* | LPAREN p = exp RPAREN { p } *)

(* Parse expressions separated by semicolons *)    
exps : 
| EOF { [] }
| e = exp es = exps { e :: es }
