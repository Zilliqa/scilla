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

  let asId i = Ident (i, ())

  let toType d = match d with
      | "Int" | "Hash" | "Address" | "BNum" | "Message" -> PrimType d
      | _ -> ADT (d, [])
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
%token COMMA
%token EQ
%token AND
%token BIND
%token ASSIGN
(* %token LANGLE
 * %token RANGLE *)       
       
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
%token MAP
       
(*  Other tokens *)
%token EOF

(* Associativity *)

(* %left PLUS *)
(* %nonassoc NEG *)

%start <unit Syntax.expr list> exps
%start <unit Syntax.stmt list> stmts_term
%start <unit Syntax.cmodule> cmodule
                                 
%%

(* Types *)
typ :
| d = CID; targs=list(targ)
  { match targs with
    | [] -> toType d                       
    | _ -> ADT (d, targs)
  }   
| MAP; k=targ; v = targ; { MapType (k, v) }
(* TODO: Add other type annotations *)

ctargs:
| LBRACE; ts = list(ctarg); RBRACE { ts }
                             
targ :
| LPAREN; t = typ; RPAREN; { t }
| d = CID { toType d }

ctarg :
| LPAREN; t = typ; RPAREN; { t }
| t = typ { t }

(***********************************************)
(*                 Expressions                 *)
(***********************************************)
  
exp:
| f = simple_exp {f}    
| LET; x = ID;
  t = ioption(type_annot) 
  EQ; f = simple_exp; IN; e = exp
  {Let ((Ident (x, ())), t, f, e) }
                                             
simple_exp :    
(* Function *)    
| FUN; LPAREN; i = ID; COLON; t = typ; RPAREN; ARROW; e = exp
  { Fun (Ident (i, ()), t, e) } 
(* Application *)  
| f = ID;
  args = nonempty_list(ID)
  { let xs = List.map (fun i -> Ident (i, ())) args
    in App ((Ident (f, ())), xs) }
(* Atomic expression *)
| a = atomic_exp {a} 
(* Built-in call *)
| BUILTIN; b = ID; args = nonempty_list(ID)
  { let xs = List.map (fun i -> Ident (i, ())) args
    in Builtin ((Ident (b, ())), xs) }
(* Message construction *)
| LBRACE; es = separated_list(SEMICOLON, msg_entry); RBRACE                { Message es } 
(* Data constructor application *)
| c = CID ts=option(ctargs) args=list(ID)
  { let targs =
      (match ts with
       | None -> []
       | Some ls -> ls) in
    let xs = List.map (fun i -> Ident (i, ())) args in
    Constr (c, targs, xs)
  }
(* Match expression *)
| MATCH; x = ID; WITH; cs=list(exp_pm_clause); END
  { MatchExpr (Ident (x, ()), cs) }
(* Type function *)
| TFUN; i = ID ARROW; e = exp
  { TFun (Ident (i, ()), e) } 
(* Type application *)
| AT; f = ID; targs = nonempty_list(targ)
  { TApp ((Ident (f, ())), targs) }

  atomic_exp :
| i = ID       { Var (Ident (i, ())) }
| l = lit      { Literal l } 
               
lit :        
| n = NUMLIT   { IntLit n }
| a = ADDRESS  { Address a }
| h = SHA3LIT  { Sha256 h }
| EMP          { Map [] }

pattern:
| UNDERSCORE { Wildcard }
| x = ID {Binder (Ident (x, ()))}
| LPAREN; p = pattern RPAREN; { p }         
| c = CID; ps = list(pattern) { Constructor (c, ps) }

exp_pm_clause:
| BAR ; p = pattern ; ARROW ; e = exp { p, e }                                  
msg_entry :
| i = ID; COLON;  l = lit { i, MLit l }
| i = ID; COLON;  c = CID { i, MTag c }
| i = ID; COLON;  v = ID { i,  MVar (asId v) }

type_annot:
| COLON; t = typ { t }


exps : 
| EOF { [] }
| e = exp es = exps { e :: es }

(***********************************************)
(*                 Statements                  *)
(***********************************************)

stmt:
| l = ID; BIND; r = ID   { Load (asId l, asId r) }
| l = ID; ASSIGN; r = ID { Store (asId l, asId r) }
| l = ID; EQ; r = exp    { Bind (asId l, r) }
| l=ID; BIND; AND; c=CID  { ReadFromBC (asId l, c) }
| ACCEPT; a = ID         { AcceptPayment (asId a) }
| SEND; m = ID;          { SendMsgs (asId m) }
| MATCH; x = ID; WITH; cs=list(stmt_pm_clause); END
  { MatchStmt (Ident (x, ()), cs) }

stmt_pm_clause:
| BAR ; p = pattern ; ARROW ;
  ss = separated_list(SEMICOLON, stmt) { p, ss }                           
stmts : 
| ss = separated_list(SEMICOLON, stmt) { ss }

stmts_term: 
| ss = stmts; EOF { ss }

(***********************************************)
(*            Contracts and Modules            *)
(***********************************************)

param_pair:
| n = ID; COLON; t = typ { asId n, t }

transition:
| TRANSITION; t = CID;
  LPAREN; params = separated_list(COMMA, param_pair); RPAREN;
  ss = stmts;
  END;
  { { tname = asId t;
      tparams = params;
      tbody = ss } }

field:
| FIELD; f = ID; COLON; t=typ;
  EQ; rhs = exp
  { asId f, t, rhs }

contract:
| CONTRACT; c = CID;
  LPAREN; params = separated_list(COMMA, param_pair); RPAREN;
  fs = list(field);
  ts = list(transition)
  { { cname   = asId c;
      cparams = params;
      cfields = fs;
      ctrans  = ts } }

libentry :
| LET; ns = ID; EQ; e= exp { { lname = asId ns; lexp = e } }

cmodule:
| LIBRARY; n = CID; ls = list(libentry); c = contract; EOF
  { { cname = asId n;
      libs = ls;
      contr = c } }
