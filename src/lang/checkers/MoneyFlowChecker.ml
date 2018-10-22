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
open Utils

module MoneyFlowRep (R : Rep) = struct
  type money_tag =
    | Plain
    | Money
    | Map of money_tag
    | Option of money_tag
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
  open EMFR
  (*   open SCU *)

  (*******************************************************)
  (*     Initial traversal: Set every tag to Plain       *)
  (*******************************************************)
  
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
      | MapUpdate (m, ks, v) ->
          MFSyntax.MapUpdate (
            add_plain_to_ident m,
            List.map add_plain_to_ident ks,
            match v with | None -> None | Some v' -> Some (add_plain_to_ident v')
          )
      | MapGet (x, m, ks, retrieve) ->
          MFSyntax.MapGet (
            add_plain_to_ident x,
            add_plain_to_ident m,
            List.map add_plain_to_ident ks,
            retrieve
          )
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
  
  (*******************************************************)
  (*                  Find fixpoint                      *)
  (*******************************************************)
  open MFSyntax
      
  (* Lattice implementation:
     t1 unifies with t2 if t1 and t2 are equal.
     Plain = Bottom, so anything supercedes Plain.
     Nothing else unifies, so return Top in all other cases *)
  let unify_tags t1 t2 =
    match t1, t2 with
    | x, y
      when x = y -> x
    | Plain, x
    | x, Plain   -> x
    | _, _       -> Top

  let get_id_tag id =
    match id with
    | Ident (_, (tag, _)) -> tag
      
  let update_id_tag id new_tag =
    match id with
    | Ident (v, (_, rep)) -> Ident (v, (new_tag, rep))

  let lookup_var_tag i field_env local_env =
    match AssocDictionary.lookup (get_id i) local_env with
    | Some t -> t
    | None ->
        match AssocDictionary.lookup (get_id i) field_env with
        | Some t -> t
        | None ->
            (* TODO: This indicates an identifier bound in a library
               Set to Top until libraries are handled. *)
            (* get_id_tag i *) Top
                    
  let update_var_tag i new_tag field_env local_env =
    match AssocDictionary.lookup (get_id i) local_env with
    | Some _ -> (field_env, AssocDictionary.update (get_id i) new_tag local_env)
    | None ->
        match AssocDictionary.lookup (get_id i) field_env with
        | Some _ -> (AssocDictionary.update (get_id i) new_tag field_env, local_env)
        | None -> (field_env, local_env)

  (* TODO: implement tag based on the builtin functions *)
  let builtin_tag _f _args = Map Top

  let rec mf_tag_expr erep expected_tag field_env local_env =
    let unify t = unify_tags expected_tag t in
    let (e, (tag, rep)) = erep in
    let (new_e, new_e_tag, new_field_env, new_local_env, new_changes) = 
      match e with
      | Literal _ -> (e, unify tag, field_env, local_env, false)
      | Var i ->
          let res_e_tag = unify (lookup_var_tag i field_env local_env) in
          let new_i = update_id_tag i res_e_tag in
          let (new_field_env, new_local_env) = update_var_tag i res_e_tag field_env local_env in
          (Var new_i, res_e_tag, new_field_env, new_local_env, res_e_tag <> lookup_var_tag i field_env local_env)
      | Fun (arg, t, body) ->
          let body_expected_tag =
            match expected_tag with
            | Map x -> x
            | Plain -> Plain
            | _     -> Top in
          let body_local_env =
            AssocDictionary.insert (get_id arg) (get_id_tag arg) local_env in
          let ((_, (new_body_tag, _)) as new_body, res_body_field_env, res_body_local_env, body_changes) =
            mf_tag_expr body body_expected_tag field_env body_local_env in
          let res_arg_tag =
            match AssocDictionary.lookup (get_id arg) res_body_local_env with
            | None -> Top
            | Some x -> x in
          (Fun (update_id_tag arg res_arg_tag, t, new_body),
           unify (Map new_body_tag),
           res_body_field_env,
           AssocDictionary.remove (get_id arg) res_body_local_env,
           body_changes || (get_id_tag arg <> res_arg_tag))
      | App (f, args) ->
          let new_args = List.map (fun arg -> update_id_tag arg (lookup_var_tag arg field_env local_env)) args in
          let f_tag = unify_tags (lookup_var_tag f field_env local_env) (Map expected_tag) in
          let (updated_field_env, updated_local_env) = update_var_tag f f_tag field_env local_env in
          let new_e_tag = 
            match f_tag with
            | Map t -> t
            | _     -> Top in
          (App (update_id_tag f f_tag, new_args),
           unify new_e_tag,
           updated_field_env,
           updated_local_env,
           f_tag <> get_id_tag f || args <> new_args)
      | Builtin (f, args) ->
          let new_args = List.map (fun arg -> update_id_tag arg (lookup_var_tag arg field_env local_env)) args in
          let f_tag = unify_tags (builtin_tag f new_args) (Map expected_tag) in
          let new_e_tag = 
            match f_tag with
            | Map t -> t
            | _     -> Top in
          (Builtin (update_id_tag f f_tag, new_args),
           unify new_e_tag,
           field_env,
           local_env,
           f_tag <> get_id_tag f || args <> new_args)
      | Let (i, topt, lhs, rhs) ->
          let ((_, (new_lhs_tag, _)) as new_lhs, lhs_field_env, lhs_local_env, lhs_changes) =
            mf_tag_expr lhs (get_id_tag i) field_env local_env in
          let new_i_tag = unify_tags (get_id_tag i) new_lhs_tag in
          let new_i = update_id_tag i new_i_tag in
          let updated_lhs_local_env = AssocDictionary.insert (get_id i) new_i_tag lhs_local_env in
          let ((_, (new_rhs_tag, _)) as new_rhs, rhs_field_env, rhs_local_env, rhs_changes) =
            mf_tag_expr rhs expected_tag lhs_field_env updated_lhs_local_env in
          let res_local_env = AssocDictionary.remove (get_id i) rhs_local_env in
          let new_e_tag = unify new_rhs_tag in
          (Let (new_i, topt, new_lhs, new_rhs),
           new_e_tag,
           rhs_field_env,
           res_local_env,
           lhs_changes || rhs_changes || new_i_tag <> get_id_tag i)
      | Constr (cname, ts, actuals) ->
          (* TODO: Handle ADTs properly *)
          let new_actuals = List.map (fun arg -> update_id_tag arg (lookup_var_tag arg field_env local_env)) actuals in
          (Constr (cname, ts, new_actuals),
           Top,
           field_env,
           local_env,
           false)
      | MatchExpr (x, clauses) ->
          let new_x = update_id_tag x (lookup_var_tag x field_env local_env) in
          (* TODO: ADTs not handled, so assume all bound variables in patterns are TOP *)
          let (res_clauses, res_tag, new_field_env, new_local_env, new_changes) =
            List.fold_right
              (fun (p, ep) (acc_clauses, acc_res_tag, acc_field_env, acc_local_env, acc_changes) ->
                 let (res_p, res_ep, res_ep_tag, res_field_env, res_local_env, res_changes) =
                   mf_tag_pattern p ep acc_res_tag acc_field_env acc_local_env in
                 ((res_p, res_ep) :: acc_clauses,
                  res_ep_tag,
                  res_field_env,
                  res_local_env,
                  acc_changes || res_changes))
              clauses
              ([], expected_tag, field_env, local_env, false) in
          (MatchExpr (new_x, res_clauses),
           res_tag,
           new_field_env,
           new_local_env,
           new_changes || (get_id_tag x) <> (get_id_tag new_x))
      | Fixpoint (_f, _t, _body) ->
          (e, Top, field_env, local_env, false)
(* TODO: This is almost correct, but we do need to look up f in the environment to find its tag.
         It is still unknow how to deal with identifiers bound in the libraries, and since
         fixpoints can only occur in library code, f must be bound to Top at this point.
          let f_tag = get_id_tag f in
          let body_local_env = AssocDictionary.insert (get_id f) (unify f_tag) local_env in
          let ((_, (body_tag, _)) as new_body, new_field_env, new_local_env, new_changes) =
            mf_tag_expr body (unify f_tag) field_env body_local_env in
          let new_f_tag = unify (unify_tags body_tag f_tag) in
          let new_f = update_id_tag f new_f_tag in
          let res_local_env = AssocDictionary.remove (get_id f) new_local_env in
          (Fixpoint (new_f, t, new_body),
           new_f_tag,
           new_field_env,
           res_local_env,
           f_tag <> new_f_tag || new_changes)
*)
      | TFun (_tvar, _body) ->
          (* TODO: Polymorphism not yet handled *)
          (e, Top, field_env, local_env, expected_tag <> Top)
      | TApp (_tf, _arg_types) ->
          (* TODO: Polymorphism not yet handled *)
          (e, Top, field_env, local_env, expected_tag <> Top)
      | Message bs ->
          (* Find _amount initializer and update env if necessary *)
          let ((new_field_env, new_local_env), env_changes) =
            match List.find_opt (fun (s, _) -> s = "_amount") bs with
            | None
            | Some (_, MTag _)
            | Some (_, MLit _) -> ((field_env, local_env), false)
            | Some (_, MVar x) ->
                let old_env_tag = lookup_var_tag x field_env local_env in
                let new_env_tag = unify_tags Money old_env_tag in
                (update_var_tag x new_env_tag field_env local_env, new_env_tag <> old_env_tag) in
          let updated_bs =
            List.map
              (fun (s, p) ->
                 match p with
                 | MTag _
                 | MLit _ -> (s, p)
                 | MVar x ->
                     (s, MVar (update_id_tag x (lookup_var_tag x new_field_env new_local_env)))) bs in
          (Message updated_bs,
           Plain,
           new_field_env,
           new_local_env,
           env_changes || bs <> updated_bs) in 
    ((new_e, (new_e_tag, rep)), new_field_env, new_local_env, new_changes || tag <> new_e_tag)

  and mf_tag_pattern p e expected_tag field_env local_env =
    let rec get_pattern_vars acc p =
      match p with
      | Wildcard -> acc
      | Binder x -> x :: acc
      | Constructor (_, ps) ->
          List.fold_left get_pattern_vars acc ps in
    (* TODO: ADTs not handled, so patterns variables updated to Top *)
    let rec update_pattern_vars p =
      match p with
      | Wildcard -> Wildcard
      | Binder x -> Binder (update_id_tag x Top)
      | Constructor (s, ps) ->
          Constructor (s, List.map update_pattern_vars ps) in
    let pattern_vars = get_pattern_vars [] p in
    let sub_local_env =
      List.fold_left
        (* TODO: ADTs not handled, so just tag pattern variables to Top *)
        (fun l_env x -> AssocDictionary.insert (get_id x) Top l_env) local_env pattern_vars in
    let ((_, (new_e_tag, _)) as new_e, new_field_env, new_local_env, new_changes) =
      mf_tag_expr e expected_tag field_env sub_local_env in
    let res_local_env = 
      List.fold_left
        (fun l_env x -> AssocDictionary.remove (get_id x) l_env) new_local_env pattern_vars in
    let res_pattern = update_pattern_vars p in
    (res_pattern, new_e, new_e_tag, new_field_env, res_local_env, new_changes)
(*    
  let rec mf_tag_stmt (srep : EMFR.stmt_annot) field_env local_env =
    let (s, rep) = srep in
    

    | Load of ER.rep ident * ER.rep ident
    | Store of ER.rep ident * ER.rep ident
    | Bind of ER.rep ident * expr_annot
    (* m[k1][k2][..] := v OR delete m[k1][k2][...] *)
    | MapUpdate of ER.rep ident * (ER.rep ident list) * ER.rep ident option
    (* v <- m[k1][k2][...] OR b <- exists m[k1][k2][...] *)
    (* If the bool is set, then we interpret this as value retrieve, 
       otherwise as an "exists" query. *)
    | MapGet of ER.rep ident * ER.rep ident * (ER.rep ident list) * bool
    | MatchStmt of ER.rep ident * (pattern * stmt_annot list) list
    | ReadFromBC of ER.rep ident * string
    | AcceptPayment
    | SendMsgs of ER.rep ident
    | CreateEvnt of ER.rep ident
    | Throw of ER.rep ident
*)

  (*******************************************************)
  (*                Main entry function                  *)
  (*******************************************************)

  let main cmod =
    let init_mod = mf_init_tag_module cmod in
    init_mod

end
