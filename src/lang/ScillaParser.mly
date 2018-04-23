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
       
(*  Other tokens *)
%token EOF

(* Associativity *)

(* %left PLUS *)
(* %nonassoc NEG *)

%start <Syntax.expr list> exps

%%

exp :
| i = ID  { Var i }
| c = CID { Constr c }
    
(* | NOT p = exp { Not p } *)
(* | p1 = exp AND p2 = exp { And (p1, p2) }  *)
(* | p1 = exp OR  p2 = exp { Or  (p1, p2) }  *)
(* | LPAREN p = exp RPAREN { p } *)

(* Parse expressions separated by semicolons *)    
exps : 
| EOF { [] }
| e = exp es = exps { e :: es }
