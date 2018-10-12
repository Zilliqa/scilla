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
*)

open Syntax
open TypeUtil

module MoneyFlowRep (R : Rep) = struct
  type money_tag =
    | Plain
    | Money
    | Map of money_tag
    | Top
  [@@deriving sexp]
    (* TODO: Add this if possible *)
  (*    | ADT of t *)
      
  type rep = money_tag * R.rep
  [@@deriving sexp]

  let get_loc r = match r with | (_, rr) -> R.get_loc rr

  let mk_id s =
    match s with
    | Ident (n, r) -> Ident (n, (Plain, r))

  let mk_id_address s = mk_id (R.mk_id_address s)
  let mk_id_uint128 s = mk_id (R.mk_id_uint128 s)
  let mk_id_bnum    s = mk_id (R.mk_id_bnum s)
  let mk_id_string  s = mk_id (R.mk_id_string s)
  
  let parse_rep s = (Plain, R.parse_rep s)
  let get_rep_str r = match r with | (_, rr) -> R.get_rep_str rr
end


module ScillaMoneyFlowChecker
    (SR : Rep)
    (ER : sig
       include Rep
       val get_type : rep -> PlainTypes.t inferred_type
     end) = struct

  module SMFR = SR
  module EMFR = MoneyFlowRep (ER)
  module TypedSyntax = ScillaSyntax (SR) (ER)
  module MFSyntax = ScillaSyntax (SMFR) (EMFR)
  (*   module TU = TypeUtilities (SR) (ER) *)
  (*   module SCU = ContractUtil.ScillaContractUtil (SR) (ER) *)

  open TypedSyntax
  (*   open SCU *)

  (* Lift Ident (n, rep) to Ident (n, (Plain, rep)) *)
  let add_plain_to_ident i =
    match i with
    | Ident (name, rep) -> Ident (name, (EMFR.Plain, rep))
  
  let rec mf_init_tag_pattern p =
    match p with
    | Wildcard -> MFSyntax.Wildcard
    | Binder x -> MFSyntax.Binder (add_plain_to_ident x)
    | Constructor (cn, ps) ->
        MFSyntax.Constructor (
          cn,
          List.map mf_init_tag_pattern ps)

  let mf_init_tag_payload p =
    match p with
    | MTag s -> MFSyntax.MTag s
    | MLit l -> MFSyntax.MLit l
    | MVar v -> MFSyntax.MVar (add_plain_to_ident v)
  
  let rec mf_init_tag_expr erep =
    let (e, rep) = erep in
    let res_e = 
      match e with
      | Literal l ->
          MFSyntax.Literal l
      | Var i ->
          MFSyntax.Var (add_plain_to_ident i)
      |  Fun (arg, t, body) ->
          MFSyntax.Fun (
              add_plain_to_ident arg,
              t,
              mf_init_tag_expr body)
      | App (f, actuals) ->
          MFSyntax.App (
              add_plain_to_ident f, 
              List.map add_plain_to_ident actuals)
      | Builtin (i, actuals) ->
          MFSyntax.Builtin (
              add_plain_to_ident i,
              List.map add_plain_to_ident actuals)
      | Let (i, topt, lhs, rhs) ->
          MFSyntax.Let (
              add_plain_to_ident i,
              topt,
              mf_init_tag_expr lhs,
              mf_init_tag_expr rhs)
      | Constr (cname, ts, actuals) ->
          MFSyntax.Constr (
              cname,
              ts,
              List.map add_plain_to_ident actuals)
      | MatchExpr (x, clauses) ->
          MFSyntax.MatchExpr (
              add_plain_to_ident x,
              List.map (fun (p, e) ->
                  (mf_init_tag_pattern p, mf_init_tag_expr e)) clauses)
      | Fixpoint (f, t, body) ->
          MFSyntax.Fixpoint (
              add_plain_to_ident f,
              t,
              mf_init_tag_expr body)
      | TFun (tvar, body) ->
          MFSyntax.TFun (
              add_plain_to_ident tvar,
              mf_init_tag_expr body)
      | TApp (tf, arg_types) ->
          MFSyntax.TApp (
              add_plain_to_ident tf,
              arg_types)
      | Message bs ->
          MFSyntax.Message (
              List.map (fun (s, p) -> (s, mf_init_tag_payload p)) bs) in
    (res_e, (EMFR.Plain, rep))

  let rec mf_init_tag_stmt srep =
    let (s, rep) = srep in
    let res_s = 
      match s with
      | Load (x, y) ->
          MFSyntax.Load (
            add_plain_to_ident x,
            add_plain_to_ident y)
      | Store (x, y) -> 
          MFSyntax.Store (
            add_plain_to_ident x,
            add_plain_to_ident y)
      | Bind (x, e) ->
          MFSyntax.Bind (
            add_plain_to_ident x,
            mf_init_tag_expr e)
      | MatchStmt (x, pss) ->
          MFSyntax.MatchStmt (
            add_plain_to_ident x,
            List.map (fun (p, ss) ->
                (mf_init_tag_pattern p,
                 List.map mf_init_tag_stmt ss)) pss)
      | ReadFromBC (x, s) ->
          MFSyntax.ReadFromBC (
            add_plain_to_ident x, s)
      | AcceptPayment ->
          MFSyntax.AcceptPayment
      | SendMsgs x ->
          MFSyntax.SendMsgs (add_plain_to_ident x)
      | CreateEvnt x ->
          MFSyntax.CreateEvnt (add_plain_to_ident x)
      | Throw x ->
          MFSyntax.Throw (add_plain_to_ident x) in
    (res_s, rep)

  let mf_init_tag_transition transition =
    let { tname ; tparams ; tbody } = transition in
    { MFSyntax.tname = tname;
      MFSyntax.tparams =
        List.map (fun (x, t) -> (add_plain_to_ident x, t)) tparams;
      MFSyntax.tbody =
        List.map mf_init_tag_stmt tbody }
  
  let mf_init_tag_contract contract =
    let { cname ; cparams ; cfields ; ctrans } = contract in
    { MFSyntax.cname = cname;
      MFSyntax.cparams =
        List.map (fun (x, t) -> (add_plain_to_ident x, t)) cparams;
      MFSyntax.cfields =
        List.map (fun (x, t, e) ->
            (add_plain_to_ident x,
             t,
             mf_init_tag_expr e)) cfields;
      MFSyntax.ctrans =
        List.map mf_init_tag_transition ctrans }
  
  let mf_init_tag_library lib =
    let { lname ; lentries } = lib in
    { MFSyntax.lname = lname;
      MFSyntax.lentries = List.map
          (fun { lname ; lexp } ->
             { MFSyntax.lname = add_plain_to_ident lname ;
               MFSyntax.lexp = mf_init_tag_expr lexp }) lentries }
  
  let mf_init_tag_module cmod =
    let { cname; libs; elibs; contr } = cmod in
    let res_libs =
      match libs with
      | None -> None
      | Some l -> Some (mf_init_tag_library l) in
    { MFSyntax.cname = cname;
      MFSyntax.libs = res_libs;
      MFSyntax.elibs = elibs;
      MFSyntax.contr = mf_init_tag_contract contr }
  
end

