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
| LET; x = ID; EQ; f = simple_exp; IN; e = exp
  {Let ((Ident (x, ())), None, f, e) }
                                             
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


  
    
typ :
| t = CID { match t with
            | "Int" | "Hash" | "Address" -> PrimType t
            | s -> ADT (s, []) }   

atomic_exp :
| n = NUMLIT   { IntLit n }
| i = ID       { Var (Ident (i, ())) }
| a = ADDRESS  { Address a }
| h = SHA3LIT  { Sha256 h }
        
(* | NOT p = exp { Not p } *)
(* | p1 = exp AND p2 = exp { And (p1, p2) }  *)
(* | p1 = exp OR  p2 = exp { Or  (p1, p2) }  *)
(* | LPAREN p = exp RPAREN { p } *)

(* Parse expressions separated by semicolons *)    
exps : 
| EOF { [] }
| e = exp es = exps { e :: es }
