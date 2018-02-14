%{

  open Syntax

%}

%token AND
%token OR
%token NOT
%token <string> VAR
%token LPAREN
%token RPAREN

%token SEMICOLON
%token EOF

%left OR
%left AND
%nonassoc NOT

%start <Syntax.expr list> exps

%%

exp :
| n = VAR { Var n }
| NOT p = exp { Not p }
| p1 = exp AND p2 = exp { And (p1, p2) } 
| p1 = exp OR  p2 = exp { Or  (p1, p2) } 
| LPAREN p = exp RPAREN { p }

(* Parse expressions separated by semicolons *)    
exps : 
| EOF { [] }
| p = exp SEMICOLON ps=exps { p :: ps }
