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

  let to_prim_type_exn d loc = match d with
    | "Int32" -> Int_typ Bits32
    | "Int64" -> Int_typ Bits64
    | "Int128" -> Int_typ Bits128
    | "Int256" -> Int_typ Bits256
    | "Uint32" -> Uint_typ Bits32
    | "Uint64" -> Uint_typ Bits64
    | "Uint128" -> Uint_typ Bits128
    | "Uint256" -> Uint_typ Bits256
    | "String" -> String_typ
    | "BNum" -> Bnum_typ
    | "Message" -> Msg_typ
    | "Event" -> Event_typ
    | "ByStr" -> Bystr_typ
    | _ -> let re = Str.regexp "ByStr\\([0-9]+\\)$" in
           if Str.string_match re d 0 then
             let open Core in
             let b = Int.of_string (Str.matched_group 1 d) in
             Bystrx_typ b
           else raise (SyntaxError ("Invalid primitive type", loc))

  let to_type d =
    try PrimType (to_prim_type_exn d dummy_loc)
    with | _ -> ADT (d, [])

  let to_map_key_type_exn d loc =
    let exn () = SyntaxError (("Invalid map key type " ^ d), loc) in
    try
      match to_prim_type_exn d loc with
      | Msg_typ | Event_typ -> raise (exn ())
      | t -> PrimType t
    with | _ -> raise (exn ())

  let build_prim_literal_exn t v loc =
    match PrimTypes.build_prim_literal t v with
    | Some l -> l
    | None -> raise (SyntaxError (("Invalid " ^ (pp_prim_typ t) ^ " literal " ^ v), loc))
%}

(* Identifiers *)
%token <string> ID
%token <string> CID
%token <string> TID
%token <string> SPID

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
%token TRY
%token CATCH
%token AS
%token PROCEDURE
%token THROW
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

%start <ParserUtil.ParsedSyntax.expr_annot> exp_term
%start <Syntax.typ> type_term
%start <ParserUtil.ParsedSyntax.stmt_annot list> stmts_term
%start <ParserUtil.ParsedSyntax.cmodule> cmodule
%start <ParserUtil.ParsedSyntax.lmodule> lmodule

%%

sid :
| name = ID { name }
| name = SPID { name }
| ns = CID; PERIOD; name = ID { ns ^ "." ^ name }

sident :
| name = ID { Ident (name, toLoc $startpos) }
| name = SPID { Ident (name, toLoc $startpos) }
| ns = CID; PERIOD; name = ID { Ident (ns ^ "." ^ name, toLoc $startpos) }

scid :
| name = CID { name }
| ns = CID; PERIOD; name = CID { ns ^ "." ^ name }

(***********************************************)
(*                  Types                      *)
(***********************************************)
(* TODO: This is a temporary fix of issue #166 *)
t_map_key :
| kt = scid { to_map_key_type_exn kt (toLoc $startpos) }
| LPAREN; kt = scid; RPAREN; { to_map_key_type_exn kt (toLoc $startpos(kt)) }

(* TODO: This is a temporary fix of issue #261 *)
t_map_value_args:
| LPAREN; t = t_map_value_args; RPAREN; { t }
| d = scid; { to_type d }
| MAP; k=t_map_key; v = t_map_value; { MapType (k, v) }

t_map_value :
| LPAREN; d = scid; targs=list(t_map_value_args); RPAREN;
    { match targs with
      | [] -> to_type d
      | _ -> ADT (d, targs) }
| LPAREN; MAP; k=t_map_key; v = t_map_value_args; RPAREN; { MapType (k, v) }
| d = scid; targs=list(t_map_value_args)
    { match targs with
      | [] -> to_type d
      | _ -> ADT (d, targs) }
| MAP; k=t_map_key; v = t_map_value; { MapType (k, v) }

typ :
| d = scid; targs=list(targ)
  { match targs with
    | [] -> to_type d
    | _ -> ADT (d, targs)
  }
| MAP; k=t_map_key; v = t_map_value; { MapType (k, v) }
| t1 = typ; TARROW; t2 = typ; { FunType (t1, t2) }
| LPAREN; t = typ; RPAREN; { t }
| FORALL; tv = TID; PERIOD; t = typ; {PolyFun (tv, t)}
| t = TID; { TypeVar t }

targ:
| LPAREN; t = typ; RPAREN; { t }
| d = scid; { to_type d }
| t = TID; { TypeVar t }
| MAP; k=t_map_key; v = t_map_value; { MapType (k, v) }

(***********************************************)
(*                 Expressions                 *)
(***********************************************)

exp:
| f = simple_exp {f}

simple_exp :
| LET; x = ID;
  t = ioption(type_annot)
  EQ; f = simple_exp; IN; e = exp
  {(Let ((Ident (x, toLoc $startpos)), t, f, e), toLoc $startpos) }
(* Function *)
| FUN; LPAREN; i = ID; COLON; t = typ; RPAREN; ARROW; e = exp
  { (Fun (Ident (i, toLoc $startpos), t, e), toLoc $startpos ) }
(* Application *)
| f = sid;
  args = nonempty_list(sident)
  { (App ((Ident (f, toLoc $startpos)), args), toLoc $startpos ) }
(* Atomic expression *)
| a = atomic_exp {a}
(* Built-in call *)
| BUILTIN; b = ID; xs = builtin_args
  { let bloc = toLoc $startpos(b) in
    (Builtin ((parse_builtin b bloc, bloc), xs)), toLoc $startpos }
(* Message construction *)
| LBRACE; es = separated_list(SEMICOLON, msg_entry); RBRACE
  { (Message es, toLoc $startpos) }
(* Data constructor application *)
| c = scid ts=option(ctargs) args=list(sident)
  { let targs =
      (match ts with
       | None -> []
       | Some ls -> ls) in
    (Constr (c, targs, args), toLoc $startpos)
  }
(* Match expression *)
| MATCH; x = sid; WITH; cs=list(exp_pm_clause); END
  { (MatchExpr (Ident (x, toLoc $startpos(x)), cs), toLoc $startpos) }
(* Type function *)
| TFUN; i = TID ARROW; e = exp
  { (TFun (Ident (i, toLoc $startpos), e), toLoc $startpos) }
(* Type application *)
| AT; f = sid; targs = nonempty_list(targ)
  { (TApp ((Ident (f, toLoc $startpos)), targs), toLoc $startpos) }

atomic_exp :
| i = sid       { (Var (Ident (i, toLoc $startpos)), toLoc $startpos) }
| l = lit      { (Literal l, toLoc $startpos) }

lit :
| i = CID;
  n = NUMLIT   {
    let string_of_n = Big_int.string_of_big_int n in
    let iloc = toLoc $startpos(i) in
    (* XXX: for Int32 -11111111111111111111 we will report error pointing to Int32,
            not the numeral, because the user might have forgotten to switch e.g. Int32 to Int64
     *)
    build_prim_literal_exn (to_prim_type_exn i iloc) string_of_n (toLoc $startpos)
  }
| h = HEXLIT   { ByStrX (Bystrx.parse_hex h) }
| s = STRING   { StringLit s }
| EMP; kt = t_map_key; vt = t_map_value
{
  Map ((kt, vt), Hashtbl.create 4) (* 4 is arbitrary here. *)
}

ctargs:
| LBRACE; ts = list(targ); RBRACE { ts }

map_access:
| LSQB; i = sident; RSQB { i }

pattern:
| UNDERSCORE { Wildcard }
| x = ID { Binder (Ident (x, toLoc $startpos)) }
| c = scid; ps = list(arg_pattern) { Constructor (c, ps) }

arg_pattern:
| UNDERSCORE { Wildcard }
| x = ID { Binder (Ident (x, toLoc $startpos)) }
| c = scid;  { Constructor (c, []) }
| LPAREN; p = pattern RPAREN; { p }

exp_pm_clause:
| BAR ; p = pattern ; ARROW ; e = exp { p, e }
msg_entry :
| i = sid; COLON;  l = lit { i, MLit l }
| i = sid; COLON;  v = sid  { i,  MVar (asIdL v (toLoc $startpos(v))) }

builtin_args :
| args = nonempty_list(sident) { args }
| LPAREN; RPAREN { [] }

type_annot:
| COLON; t = typ { t }

exp_term :
| e = exp; EOF { e }

type_term :
| t = typ; EOF { t }

(***********************************************)
(*                 Statements                  *)
(***********************************************)

stmt:
| l = ID; BIND; r = sid   { (Load (asIdL l (toLoc $startpos($2)), asIdL r (toLoc $startpos(r))), toLoc $startpos) }
| l = ID; ASSIGN; r = sid { (Store (asIdL l (toLoc $startpos($2)), asIdL r (toLoc $startpos(r))), toLoc $startpos) }
| l = ID; EQ; r = exp    { (Bind (asIdL l (toLoc $startpos($2)), r), toLoc $startpos) }
| l = ID; BIND; AND; c = CID { (ReadFromBC (asIdL l (toLoc $startpos($2)), c), toLoc $startpos) }
| l = ID; BIND; r = ID; keys = nonempty_list(map_access)
  { MapGet(asIdL l (toLoc $startpos(l)), asIdL r (toLoc $startpos(r)), keys, true), toLoc $startpos }
| l = ID; BIND; EXISTS; r = ID; keys = nonempty_list(map_access)
  { MapGet(asIdL l (toLoc $startpos(l)), asIdL r (toLoc $startpos(r)), keys, false), toLoc $startpos }
| l = ID; keys = nonempty_list(map_access); ASSIGN; r = sid
  { MapUpdate(asIdL l (toLoc $startpos(l)), keys, Some (asIdL r (toLoc $startpos(r)))), toLoc $startpos }
| DELETE; l = ID; keys = nonempty_list(map_access)
  { MapUpdate(asIdL l (toLoc $startpos(l)), keys, None), toLoc $startpos }
| ACCEPT                 { (AcceptPayment, toLoc $startpos) }
| SEND; m = sid;          { (SendMsgs (asIdL m (toLoc $startpos)), toLoc $startpos) }
| EVENT; m = sid; { (CreateEvnt (asIdL m (toLoc $startpos)), toLoc $startpos) }
| THROW; mopt = option(sid); { Throw (BatOption.map (fun m -> (asIdL m (toLoc $startpos))) mopt), toLoc $startpos }
| MATCH; x = sid; WITH; cs=list(stmt_pm_clause); END
  { (MatchStmt (Ident (x, toLoc $startpos(x)), cs), toLoc $startpos)  }
| (* procedure call *)
  p = component_id;
  args = list(sident)
  { (CallProc (p, args), toLoc $startpos)  }

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

component:
| t = transition
  { t }
| p = procedure 
  { p }

procedure:
| PROCEDURE; t = component_id;
  params = component_params;
  ss = component_body;
  { { comp_type = CompProc;
      comp_name = t;
      comp_params = params;
      comp_body = ss } }

transition:
| TRANSITION; t = component_id;
  params = component_params;
  ss = component_body;
  { { comp_type = CompTrans;
      comp_name = t;
      comp_params = params;
      comp_body = ss } }

component_id:
| c = CID { asIdL c (toLoc $startpos(c)) }
| i = ID { asIdL i (toLoc $startpos(i)) }

component_params:
| LPAREN; params = separated_list(COMMA, param_pair); RPAREN;
  { params }

component_body:
| ss = stmts; END;
  { ss }

field:
| FIELD; f = ID; COLON; t=typ;
  EQ; rhs = exp
  { asIdL f (toLoc $startpos(f)), t, rhs }

contract:
| CONTRACT; c = CID;
  LPAREN; params = separated_list(COMMA, param_pair); RPAREN;
  fs = list(field);
  comps = list(component)
  { { cname   = asIdL c (toLoc $startpos(c));
      cparams = params;
      cfields = fs;
      ccomps = comps } }

tconstr :
| BAR; tn = CID;
  { { cname = asIdL tn (toLoc $startpos); c_arg_types = [] } }
| BAR; tn = CID; OF; t = nonempty_list(targ);
  { { cname = asIdL tn (toLoc $startpos); c_arg_types = t }}

libentry :
| LET; ns = ID;
  t = ioption(type_annot)
  EQ; e= exp { LibVar (asIdL ns (toLoc $startpos(ns)), t, e) }
| TYPE; tname = CID
  { LibTyp (asIdL tname (toLoc $startpos), []) }
| TYPE; tname = CID; EQ; constrs = nonempty_list(tconstr)
  { LibTyp (asIdL tname (toLoc $startpos), constrs) }

library :
| LIBRARY; n = CID; ls = list(libentry);
  { {lname = asIdL n (toLoc $startpos);
     lentries = ls } }

lmodule :
| els = imports; l = library; EOF { { elibs = els; libs = l } }

importname :
| c = CID { Ident(c, toLoc $startpos), None }
| c1 = CID AS c2 = CID { Ident(c1, toLoc $startpos(c1)), Some (Ident(c2, toLoc $startpos(c2)))}

imports :
| IMPORT; els = list(importname) { els }
| { [] }

cmodule:
| SCILLA_VERSION; cver = NUMLIT; els = imports; ls = option(library); c = contract; EOF
  { { smver = Big_int.int_of_big_int cver;
      cname = c.cname;
      libs = ls;
      elibs = els;
      contr = c } }

