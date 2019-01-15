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

%{
  open Syntax
  open ErrorUtils
  open ParserUtil

  open ParsedSyntax

  let to_type d = match d with
    | x when PrimTypes.is_prim_type (PrimType x) -> PrimType x
    | _ -> ADT (d, [])
  
  let build_prim_literal_exn t v =
    match PrimTypes.build_prim_literal t v with
    | Some l -> l
    | None -> raise (SyntaxError ("Invalid " ^ (pp_typ t) ^ " literal " ^ v))
%}

(* Identifiers *)    
%token <string> ID
%token <string> CID
%token <string> TID

(* Strings *)    
%token <string> STRING

(* Numbers and hashes *)
%token <Big_int.big_int> NUMLIT
%token <string> HEXLIT
                  
(* Separators *)    
%token SEMICOLON
%token COLON
%token BAR
%token LSQB
%token RSQB
%token LPAREN
%token RPAREN
%token ARROW
%token TARROW
%token AT
%token UNDERSCORE
%token LBRACE       
%token RBRACE
%token COMMA
%token PERIOD
%token EQ
%token AND
%token BIND
%token ASSIGN
(* %token LANGLE
 * %token RANGLE *)       
       
(* Keywords *)    
%token BUILTIN
%token FORALL
%token EMP
%token LIBRARY
%token IMPORT
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
%token EVENT
%token ACCEPT
%token MAP
%token DELETE
%token EXISTS
%token SCILLA_VERSION
%token TYPE
%token OF
       
(*  Other tokens *)
%token EOF

(* Associativity *)

(* %left PLUS *)
(* %nonassoc NEG *)
%right TARROW

%start <ParserUtil.ParsedSyntax.expr_annot list> exps
%start <Syntax.typ list> types
%start <ParserUtil.ParsedSyntax.stmt_annot list> stmts_term
%start <ParserUtil.ParsedSyntax.cmodule> cmodule
%start <ParserUtil.ParsedSyntax.library> lmodule

%%

(***********************************************)
(*                  Types                      *)
(***********************************************)
typ :
| d = CID; targs=list(targ)
  { match targs with
    | [] -> to_type d                       
    | _ -> ADT (d, targs)
  }   
| MAP; k=targ; v = targ; { MapType (k, v) }
| t1 = typ; TARROW; t2 = typ; { FunType (t1, t2) }
| LPAREN; t = typ; RPAREN; { t }
| FORALL; tv = TID; PERIOD; t = typ; {PolyFun (tv, t)}
| t = TID; { TypeVar t }        
                                  
targ:
| LPAREN; t = typ; RPAREN; { t }
| d = CID; { to_type d }
| t = TID; { TypeVar t }
| MAP; k=targ; v = targ; { MapType (k, v) }             

(***********************************************)
(*                 Expressions                 *)
(***********************************************)
  
exp:
| f = simple_exp {f}    
| LET; x = ID;
  t = ioption(type_annot) 
  EQ; f = simple_exp; IN; e = exp
  {(Let ((Ident (x, toLoc $startpos)), t, f, e), toLoc $startpos) }
                                             
simple_exp :    
(* Function *)    
| FUN; LPAREN; i = ID; COLON; t = typ; RPAREN; ARROW; e = exp
  { (Fun (Ident (i, toLoc $startpos), t, e), toLoc $startpos ) } 
(* Application *)  
| f = ID;
  args = nonempty_list(ident)
  { (App ((Ident (f, toLoc $startpos)), args), toLoc $startpos ) }
(* Atomic expression *)
| a = atomic_exp {a} 
(* Built-in call *)
| BUILTIN; b = ID; xs = builtin_args
  { (Builtin ((Ident (b, toLoc $startpos(b))), xs), toLoc $startpos) }
(* Message construction *)
| LBRACE; es = separated_list(SEMICOLON, msg_entry); RBRACE
  { (Message es, toLoc $startpos) } 
(* Data constructor application *)
| c = CID ts=option(ctargs) args=list(ident)
  { let targs =
      (match ts with
       | None -> []
       | Some ls -> ls) in
    (Constr (c, targs, args), toLoc $startpos)
  }
(* Match expression *)
| MATCH; x = ID; WITH; cs=list(exp_pm_clause); END
  { (MatchExpr (Ident (x, toLoc $startpos(x)), cs), toLoc $startpos) }
(* Type function *)
| TFUN; i = TID ARROW; e = exp
  { (TFun (Ident (i, toLoc $startpos), e), toLoc $startpos) } 
(* Type application *)
| AT; f = ID; targs = nonempty_list(targ)
  { (TApp ((Ident (f, toLoc $startpos)), targs), toLoc $startpos) }

  atomic_exp :
| i = ID       { (Var (Ident (i, toLoc $startpos)), toLoc $startpos) }
| l = lit      { (Literal l, toLoc $startpos) } 
               
lit :        
| i = CID;
  n = NUMLIT   {
    let string_of_n = Big_int.string_of_big_int n in
    build_prim_literal_exn (to_type i) string_of_n
  }
| h = HEXLIT   { 
  let l = String.length h in
  build_prim_literal_exn (PrimTypes.bystrx_typ ((l-1)/2)) h
}
| s = STRING   { StringLit s }
| EMP; kt = targ; vt = targ
{
  Map ((kt, vt), Hashtbl.create 4) (* 4 is arbitrary here. *)
  (* if isPrimType kt
   * then Map ((kt, vt), [])
   * else
   *   raise (SyntaxError (Core.sprintf "Non-primitive type (%s) cannot be a map key."
   *                    (pp_typ kt))) *)
}

ctargs:
| LBRACE; ts = list(targ); RBRACE { ts }

map_access:
| LSQB; i = ident; RSQB { i }

pattern:
| UNDERSCORE { Wildcard }
| x = ID {Binder (Ident (x, toLoc $startpos))}
| c = CID; ps = list(arg_pattern) { Constructor (c, ps) }

arg_pattern:
| UNDERSCORE { Wildcard }
| x = ID {Binder (Ident (x, toLoc $startpos))}
| c = CID;  { Constructor (c, []) }
| LPAREN; p = pattern RPAREN; { p }         

exp_pm_clause:
| BAR ; p = pattern ; ARROW ; e = exp { p, e }                                  
msg_entry :
| i = ID; COLON;  l = lit { i, MLit l }
| i = ID; COLON;  c = CID { i, MTag c }
| i = ID; COLON;  v = ID  { i,  MVar (asIdL v (toLoc $startpos(v))) }

builtin_args :
| args = nonempty_list(ident) { args }
| LPAREN; RPAREN { [] }

ident :
| i = ID { Ident(i, toLoc $startpos) }

cident :
| c = CID { Ident(c, toLoc $startpos) }

type_annot:
| COLON; t = typ { t }

exps : 
| EOF { [] }
| e = exp; es = exps { e :: es }

types : 
| EOF { [] }
| t = typ; ts = types {t :: ts}

(***********************************************)
(*                 Statements                  *)
(***********************************************)

stmt:
| l = ID; BIND; r = ID   { (Load (asIdL l (toLoc $startpos($2)), asIdL r (toLoc $startpos(r))), toLoc $startpos) }
| l = ID; ASSIGN; r = ID { (Store (asIdL l (toLoc $startpos($2)), asIdL r (toLoc $startpos(r))), toLoc $startpos) }
| l = ID; EQ; r = exp    { (Bind (asIdL l (toLoc $startpos($2)), r), toLoc $startpos) }
| l=ID; BIND; AND; c=CID { (ReadFromBC (asIdL l (toLoc $startpos($2)), c), toLoc $startpos) }
| l = ID; BIND; r = ID; keys = nonempty_list(map_access)
  { MapGet(asIdL l (toLoc $startpos(l)), asIdL r (toLoc $startpos(r)), keys, true), toLoc $startpos }
| l = ID; BIND; EXISTS; r = ID; keys = nonempty_list(map_access)
  { MapGet(asIdL l (toLoc $startpos(l)), asIdL r (toLoc $startpos(r)), keys, false), toLoc $startpos }
| l = ID; keys = nonempty_list(map_access); ASSIGN; r = ID
  { MapUpdate(asIdL l (toLoc $startpos(l)), keys, Some (asIdL r (toLoc $startpos(r)))), toLoc $startpos }
| DELETE; l = ID; keys = nonempty_list(map_access)
  { MapUpdate(asIdL l (toLoc $startpos(l)), keys, None), toLoc $startpos }
| ACCEPT                 { (AcceptPayment, toLoc $startpos) }
| SEND; m = ID;          { (SendMsgs (asIdL m (toLoc $startpos)), toLoc $startpos) }
| EVENT; m = ID; { (CreateEvnt (asIdL m (toLoc $startpos)), toLoc $startpos) }
| MATCH; x = ID; WITH; cs=list(stmt_pm_clause); END
  { (MatchStmt (Ident (x, toLoc $startpos(x)), cs), toLoc $startpos)  }

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
| n = ID; COLON; t = typ { asIdL n (toLoc $startpos(n)), t }

transition:
| TRANSITION; t = trans_id;
  LPAREN; params = separated_list(COMMA, param_pair); RPAREN;
  ss = stmts;
  END;
  { { tname = t;
      tparams = params;
      tbody = ss } }

trans_id:
| c = CID { asIdL c (toLoc $startpos(c)) }
| i = ID { asIdL i (toLoc $startpos(i)) }

field:
| FIELD; f = ID; COLON; t=typ;
  EQ; rhs = exp
  { asIdL f (toLoc $startpos(f)), t, rhs }

contract:
| CONTRACT; c = CID;
  LPAREN; params = separated_list(COMMA, param_pair); RPAREN;
  fs = list(field);
  ts = list(transition)
  { { cname   = asIdL c (toLoc $startpos(c));
      cparams = params;
      cfields = fs;
      ctrans  = ts } }

tconstr :
| BAR; tn = CID;
  { { cname = asIdL tn (toLoc $startpos); c_arg_types = [] } }
| BAR; tn = CID; OF; t = nonempty_list(targ);
  { { cname = asIdL tn (toLoc $startpos); c_arg_types = t }}

libentry :
| LET; ns = ID; EQ; e= exp { LibVar (asIdL ns (toLoc $startpos(ns)), e) }
| TYPE; tname = CID; EQ; constrs = list(tconstr)
  { LibTyp (asIdL tname (toLoc $startpos), constrs) }

library :
| LIBRARY; n = CID; ls = list(libentry);
  { {lname = asIdL n (toLoc $startpos);
     lentries = ls } }

lmodule :
| l = library; EOF { l }

imports :
| IMPORT; els = list(cident) { els }
| { [] }

cmodule:
| SCILLA_VERSION; cver = NUMLIT; els = imports; ls = option(library); c = contract; EOF
  { { smver = Big_int.int_of_big_int cver;
      cname = c.cname;
      libs = ls;
      elibs = els;
      contr = c } }

