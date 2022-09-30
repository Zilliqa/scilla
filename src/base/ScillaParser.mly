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

%parameter<S : ParserUtil.Syn>

%{
  open Syntax
  open ErrorUtils

  module ParserIdentifier = S.SIdentifier
  module ParserName = ParserIdentifier.Name
  open S

  let to_loc_id s loc = SIdentifier.mk_id (ParserName.parse_simple_name s) loc

  let to_prim_type_exn d loc =
    let open Type.PrimType in
    match d with
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
             let b = Core.Int.of_string (Str.matched_group 1 d) in
             Bystrx_typ b
           else raise (SyntaxError ("Invalid primitive type", loc))

  let to_type n sloc =
    let d = ParserName.as_string n in
    let open SType in
    try PrimType (to_prim_type_exn d sloc)
    with | _ -> ADT (SIdentifier.mk_id n sloc, [])

  let to_map_key_type_exn n loc =
    let d = ParserName.as_string n in
    let exn () = SyntaxError (("Invalid map key type " ^ d), loc) in
    try
      let open SType in
      match to_prim_type_exn d loc with
      | Msg_typ | Event_typ -> raise (exn ())
      | t -> PrimType t
    with | _ -> raise (exn ())

  let build_prim_literal_exn t v loc =
    match SLiteral.build_prim_literal t v with
    | Some l -> l
    | None -> raise (SyntaxError (("Invalid " ^ (Type.PrimType.pp_prim_typ t) ^ " literal " ^ v), loc))

  let build_bool_literal v loc =
    (Literal (SLiteral.build_bool_lit v), loc)

  let build_bcfetch op args loc =
    match op with
    | "BLOCKNUMBER" -> CurBlockNum
    | "CHAINID" -> ChainID
    | "TIMESTAMP" -> (
      match args with
      | [ id ] -> Timestamp(id)
      | _ -> raise (SyntaxError ("TIMESTAMP takes a single blocknumber argument", loc)))
    | "REPLICATE_CONTRACT" -> (
      match args with
      | [ addr; iparams ] -> ReplicateContr(addr, iparams)
      | _ -> raise (SyntaxError ("Usage: REPLICATE_CONTRACT(addr, iparams)", loc)))
    | _ -> raise (SyntaxError ("Unknown blockchain fetch operation " ^ op, loc))

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
%token FETCH
%token REMOTEFETCH
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

%start <S.expr_annot> exp_term
%start <S.SType.t> type_term
%start <S.stmt_annot list> stmts_term
%start <S.cmodule> cmodule
%start <S.lmodule> lmodule

%%

sid :
| name = ID { ParserName.parse_simple_name name }
| name = SPID { ParserName.parse_simple_name name }
| ns = CID; PERIOD; name = ID { ParserName.parse_qualified_name ns name }

sident :
| name = ID { ParserIdentifier.mk_id (ParserName.parse_simple_name name) (toLoc $startpos) }
| name = SPID { ParserIdentifier.mk_id (ParserName.parse_simple_name name) (toLoc $startpos) }
| ns = CID; PERIOD; name = ID { ParserIdentifier.mk_id (ParserName.parse_qualified_name ns name) (toLoc $startpos) }

scid :
| name = CID { ParserName.parse_simple_name name }
| ns = CID; PERIOD; name = CID { ParserName.parse_qualified_name ns name }
(* This production is necessary because message and state jsons contain global type names *)
| ns = HEXLIT; PERIOD; name = CID {
  let ns' = SLiteral.(Bystrx.hex_encoding (Bystrx.parse_hex ns)) in
  ParserName.parse_qualified_name ns' name
}

type_annot:
| COLON; t = typ { t }

id_with_typ :
| n = ID; t = type_annot { (to_loc_id n (toLoc $startpos(n)), t) }

(***********************************************)
(*                  Types                      *)
(***********************************************)
(* TODO: This is a temporary fix of issue #166 *)
t_map_key :
| kt = scid { to_map_key_type_exn kt (toLoc $startpos) }
| LPAREN; kt = scid; RPAREN; { to_map_key_type_exn kt (toLoc $startpos(kt)) }
| LPAREN; kt = address_typ; RPAREN; { kt }
| kt = address_typ; { kt }

(* TODO: This is a temporary fix of issue #261 *)
t_map_value_args:
| LPAREN; t = t_map_value_allow_targs; RPAREN; { t }
| d = scid; { to_type d (toLoc $startpos(d))}
| MAP; k=t_map_key; v = t_map_value; { SType.MapType (k, v) }

t_map_value :
(*
| d = scid; targs=list(t_map_value_args)
    { match targs with
      | [] -> to_type d (toLoc $startpos(d))
      | _ -> ADT (SIdentifier.mk_id d (toLoc $startpos(d)), targs) }
*)
| d = scid;
  { to_type d (toLoc $startpos(d)) }
| MAP; k=t_map_key; v = t_map_value; { SType.MapType (k, v) }
  | LPAREN; t = t_map_value_allow_targs; RPAREN;
    { (* We only allow targs when the type is surrounded by parentheses *)
      t }
| vt = address_typ; { vt }

t_map_value_allow_targs :
| d = scid; targs = nonempty_list(t_map_value_args)
  { (* We only allow targs when the type is surrounded by parentheses *)
    match targs with
    | [] -> to_type d (toLoc $startpos(d))
    | _ -> ADT (SIdentifier.mk_id d (toLoc $startpos(d)), targs) }
| t = t_map_value
  { t }

address_typ :
| d = CID; WITH; END;
    { if d = "ByStr20"
      then Address AnyAddr
      else raise (SyntaxError ("Invalid type", toLoc $startpos(d))) }
| d = CID; WITH; CONTRACT; fs = separated_list(COMMA, address_type_field); END;
    { if d = "ByStr20"
      then
        (* Add _this_address : ByStr20 to field list. This ensures the type is treated as a contract address *)
        let fs' = List.fold_left (fun acc (id, t) -> 
          SType.IdLoc_Comp.Map.set acc ~key:id ~data:t) SType.IdLoc_Comp.Map.empty fs
        in
        Address (ContrAddr fs')
      else raise (SyntaxError ("Invalid type", toLoc $startpos(d))) }
| d = CID; WITH; LIBRARY; END;
    { if d = "ByStr20"
      then Address LibAddr
      else raise (SyntaxError ("Invalid type", toLoc $startpos(d))) }
| d = CID; WITH; c = SPID; END;
    { if d = "ByStr20" && c = "_codehash"
      then Address CodeAddr
      else raise (SyntaxError ("Invalid type", toLoc $startpos(d))) }
| (* Adding this production in preparation for contract parameters *)
  d = CID; WITH; CONTRACT; LPAREN; _ps = separated_list(COMMA, param_pair); RPAREN; _fs = separated_list(COMMA, address_type_field); END;
    { if d = "ByStr20"
      then raise (SyntaxError ("Contract parameters in address types not yet supported", toLoc $startpos(d)))
      else raise (SyntaxError ("Invalid type", toLoc $startpos(d))) }

typ :
| d = scid; targs=list(targ)
  { match targs with
    | [] -> to_type d (toLoc $startpos(d))
    | _ -> ADT (SIdentifier.mk_id d (toLoc $startpos(d)), targs)
  }
| MAP; k=t_map_key; v = t_map_value; { SType.MapType (k, v) }
| t1 = typ; TARROW; t2 = typ; { SType.FunType (t1, t2) }
| LPAREN; t = typ; RPAREN; { t }
| t = address_typ { t }
| FORALL; tv = TID; PERIOD; t = typ; {SType.PolyFun (tv, t)}
%prec TARROW
| t = TID; { SType.TypeVar t }

targ:
| LPAREN; t = typ; RPAREN; { t }
| d = scid; { to_type d (toLoc $startpos(d))}
| t = TID; { TypeVar t }
| t = address_typ; { t }
| MAP; k=t_map_key; v = t_map_value; { MapType (k, v) }

address_type_field:
| FIELD; ft = id_with_typ { ft }

(***********************************************)
(*                 Expressions                 *)
(***********************************************)

exp:
| f = simple_exp {f}

simple_exp :
| LET; x = ID;
  t = ioption(type_annot)
  EQ; f = simple_exp; IN; e = exp
  {(Let ( to_loc_id x (toLoc $startpos(x)), t, f, e), toLoc $startpos(f)) }
(* Function *)
| FUN; LPAREN; iwt = id_with_typ; RPAREN; ARROW; e = exp
    { match iwt with
      | (i, t) -> (Fun (i, t, e), toLoc $startpos(e) ) }
(* Application *)
| f = sid;
  args = nonempty_list(sident)
  { (App (ParserIdentifier.mk_id f (toLoc $startpos(f)), args), toLoc $startpos ) }
(* Atomic expression *)
| a = atomic_exp {a}
(* Built-in call *)
| BUILTIN; b = ID; targs = option(ctargs); xs = builtin_args
  { let bloc = toLoc $startpos(b) in
    (Builtin ((parse_builtin b bloc, bloc), Option.value targs ~default:[], xs)), toLoc $startpos }
(* Message construction *)
| LBRACE; es = separated_list(SEMICOLON, msg_entry); RBRACE
  { (Message es, toLoc $startpos) }
(* Data constructor application *)
| c = scid ts=option(ctargs) args=list(sident)
  { let targs =
      (match ts with
       | None -> []
       | Some ls -> ls) in
    (Constr (SIdentifier.mk_id c (toLoc $startpos), targs, args), toLoc $startpos)
  }
(* Match expression *)
| MATCH; x = sid; WITH; cs=list(exp_pm_clause); END
  { (MatchExpr (ParserIdentifier.mk_id x (toLoc $startpos(x)), cs), toLoc $startpos) }
(* Type function *)
| TFUN; i = TID ARROW; e = exp
  { (TFun ( to_loc_id i (toLoc $startpos(i)), e), toLoc $startpos) }
(* Type application *)
| AT; f = sid; targs = nonempty_list(targ)
  { (TApp (ParserIdentifier.mk_id f (toLoc $startpos(f)), targs), toLoc $startpos) }

atomic_exp :
| i = sid       { (Var (ParserIdentifier.mk_id i (toLoc $startpos(i))), toLoc $startpos) }
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
| h = HEXLIT   { SLiteral.(ByStrX (Bystrx.parse_hex h)) }
| s = STRING   { build_prim_literal_exn String_typ s (toLoc $startpos) }
| EMP; kt = t_map_key; vt = t_map_value
{
  Map ((kt, vt), Caml.Hashtbl.create 4) (* 4 is arbitrary here. *)
}

ctargs:
| LBRACE; ts = list(targ); RBRACE { ts }

map_access:
| LSQB; i = sident; RSQB { i }

pattern:
| UNDERSCORE { Wildcard }
| x = ID { Binder (to_loc_id x (toLoc $startpos(x))) }
| c = scid; ps = list(arg_pattern) { Constructor (ParserIdentifier.mk_id c (toLoc $startpos(c)), ps) }

arg_pattern:
| UNDERSCORE { Wildcard }
| x = ID { Binder (to_loc_id x (toLoc $startpos(x))) }
| c = scid;  { Constructor (ParserIdentifier.mk_id c (toLoc $startpos(c)), []) }
| LPAREN; p = pattern RPAREN; { p }

exp_pm_clause:
| BAR ; p = pattern ; ARROW ; e = exp { p, e }

msg_entry :
| i = sid; COLON;  l = lit { ParserName.as_string i, MLit l }
| i = sid; COLON;  v = sid  { ParserName.as_string i,  MVar (ParserIdentifier.mk_id v (toLoc $startpos(v))) }

builtin_args :
| args = nonempty_list(sident) { args }
| LPAREN; RPAREN { [] }

bcfetch_args :
| LPAREN; args = separated_list(COMMA, sident); RPAREN { args }

exp_term :
| e = exp; EOF { e }

type_term :
| t = typ; EOF { t }

(***********************************************)
(*                 Statements                  *)
(***********************************************)

stmt:
| l = ID; FETCH; r = sid   { (Load (to_loc_id l (toLoc $startpos(l)), ParserIdentifier.mk_id r (toLoc $startpos(r))), toLoc $startpos) }
| r = remote_fetch_stmt { r }
| l = ID; ASSIGN; r = sid { (Store ( to_loc_id l (toLoc $startpos(l)), ParserIdentifier.mk_id r (toLoc $startpos(r))), toLoc $startpos) }
| l = ID; EQ; r = exp    { (Bind ( to_loc_id l (toLoc $startpos(l)), r), toLoc $startpos) }
| l = ID; FETCH; AND; c = CID; args_opt = option(bcfetch_args) { 
    let bcinfo = build_bcfetch c (Option.value args_opt ~default:[]) (toLoc $startpos) in
    (ReadFromBC ( to_loc_id l (toLoc $startpos(l)), bcinfo), toLoc $startpos) 
  }
| l = ID; FETCH; r = ID; keys = nonempty_list(map_access)
  { MapGet( to_loc_id l (toLoc $startpos(l)), to_loc_id r (toLoc $startpos(r)), keys, true), toLoc $startpos }
| l = ID; FETCH; EXISTS; r = ID; keys = nonempty_list(map_access)
  { MapGet( to_loc_id l (toLoc $startpos(l)), to_loc_id r (toLoc $startpos(r)), keys, false), toLoc $startpos }
| l = ID; keys = nonempty_list(map_access); ASSIGN; r = sid
  { MapUpdate( to_loc_id l (toLoc $startpos(l)), keys, Some (ParserIdentifier.mk_id r (toLoc $startpos(r)))), toLoc $startpos }
| DELETE; l = ID; keys = nonempty_list(map_access)
  { MapUpdate( to_loc_id l (toLoc $startpos(l)), keys, None), toLoc $startpos }
| ACCEPT                 { (AcceptPayment, toLoc $startpos) }
| SEND; m = sid;          { (SendMsgs (ParserIdentifier.mk_id m (toLoc $startpos(m))), toLoc $startpos) }
| EVENT; m = sid; { (CreateEvnt (ParserIdentifier.mk_id m (toLoc $startpos(m))), toLoc $startpos) }
| THROW; mopt = option(sid); { Throw (Core.Option.map mopt ~f:(fun m -> (ParserIdentifier.mk_id m (toLoc $startpos(mopt))))), toLoc $startpos }
| MATCH; x = sid; WITH; cs=list(stmt_pm_clause); END
  { (MatchStmt (ParserIdentifier.mk_id x (toLoc $startpos(x)), cs), toLoc $startpos)  }
| (* procedure call *)
  p = component_id;
  args = list(sident)
  { (CallProc (p, args), toLoc $startpos)  }
| (* list iterator *)
  FORALL; l = sident; p = component_id
  { Iterate (l, p), toLoc $startpos }

remote_fetch_stmt:
| l = ID; FETCH; AND; adr = ID; PERIOD; r = sident
  { RemoteLoad (to_loc_id l (toLoc $startpos(l)), to_loc_id adr (toLoc $startpos(adr)), r), toLoc $startpos }
| (* Reading _sender._balance or _origin._balance *)
  l = ID; FETCH; AND; adr = SPID; PERIOD; r = SPID
  { RemoteLoad (to_loc_id l (toLoc $startpos(l)), to_loc_id adr (toLoc $startpos(adr)), to_loc_id r (toLoc $startpos(r))), toLoc $startpos }
| (* Adding this production in preparation for remote reads of contract parameters *)
  _l = ID; FETCH; AND; _adr = ID; PERIOD; LPAREN; _r = sident; RPAREN;
  { raise (SyntaxError ("Remote fetch of contract parameters not yet supported", toLoc $startpos(_adr))) }
| l = ID; FETCH; AND; adr = ID; PERIOD; r = ID; keys = nonempty_list(map_access)
  { RemoteMapGet(to_loc_id l (toLoc $startpos(l)), to_loc_id adr (toLoc $startpos(adr)), to_loc_id r (toLoc $startpos(r)), keys, true), toLoc $startpos }
| l = ID; FETCH; AND; EXISTS; adr = ID; PERIOD; r = ID; keys = nonempty_list(map_access)
  { RemoteMapGet(to_loc_id l (toLoc $startpos(l)), to_loc_id adr (toLoc $startpos(adr)), to_loc_id r (toLoc $startpos(r)), keys, false), toLoc $startpos }
| (* Adding this production in preparation for address type casts *)
  l = ID; FETCH; AND; adr = sident; AS; t = address_typ
  { TypeCast(to_loc_id l (toLoc $startpos(l)), adr, t), toLoc $startpos }

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
| iwt = id_with_typ { iwt }

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
| c = CID { to_loc_id c (toLoc $startpos(c)) }
| i = ID { to_loc_id i (toLoc $startpos(i)) }

component_params:
| LPAREN; params = separated_list(COMMA, param_pair); RPAREN;
  { params }

component_body:
| ss = stmts; END;
  { ss }

field:
| FIELD; iwt = id_with_typ
  EQ; rhs = exp
    { match iwt with
      | (f, t) -> (f, t, rhs) }

with_constraint:
| WITH; f = exp; ARROW
  { f }  

contract:
| CONTRACT; c = CID;
  LPAREN; params = separated_list(COMMA, param_pair); RPAREN;
  ct = ioption(with_constraint);
  fs = list(field);
  comps = list(component)
  { { cname   = to_loc_id c (toLoc $startpos(c));
      cparams = params;
      cconstraint = Core.Option.value ct ~default:(build_bool_literal true dummy_loc);
      cfields = fs;
      ccomps = comps } }

tconstr :
| BAR; tn = CID;
    { { cname = to_loc_id tn (toLoc $startpos); c_arg_types = [] } }
| BAR; tn = CID; OF; t = nonempty_list(targ);
    { { cname = to_loc_id tn (toLoc $startpos); c_arg_types = t }}

libentry :
| LET; ns = ID;
  t = ioption(type_annot)
  EQ; e= exp { LibVar (to_loc_id ns (toLoc $startpos(ns)), t, e) }
| TYPE; tname = CID
  { LibTyp (to_loc_id tname (toLoc $startpos), []) }
| TYPE; tname = CID; EQ; constrs = nonempty_list(tconstr)
  { LibTyp (to_loc_id tname (toLoc $startpos), constrs) }

library :
| LIBRARY; n = CID; ls = list(libentry);
  { {lname = to_loc_id n (toLoc $startpos);
     lentries = ls } }

lmodule :
| SCILLA_VERSION; cver = NUMLIT; els = imports; l = library; EOF 
  { { smver = Big_int.int_of_big_int cver;
      elibs = els; libs = l } }

importname :
| c = CID { to_loc_id c (toLoc $startpos), None }
| c1 = CID AS c2 = CID { to_loc_id c1 (toLoc $startpos(c1)), Some ( to_loc_id c2 (toLoc $startpos(c2)))}

imports :
| IMPORT; els = list(importname) { els }
| { [] }

cmodule:
| SCILLA_VERSION; cver = NUMLIT; els = imports; ls = option(library); c = contract; EOF
  { { smver = Big_int.int_of_big_int cver;
      libs = ls;
      elibs = els;
      contr = c } }
