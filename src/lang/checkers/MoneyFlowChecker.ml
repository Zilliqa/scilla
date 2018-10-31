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
    | Bottom
    | NotMoney
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
    | Ident (n, r) -> Ident (n, (Bottom, r))

  let mk_id_address s = mk_id (R.mk_id_address s)
  let mk_id_uint128 s = mk_id (R.mk_id_uint128 s)
  let mk_id_bnum    s = mk_id (R.mk_id_bnum s)
  let mk_id_string  s = mk_id (R.mk_id_string s)
  
  let parse_rep s = (Bottom, R.parse_rep s)
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
  (*     Initial traversal: Set every tag to Bottom      *)
  (*******************************************************)
  
  (* Lift Ident (n, rep) to Ident (n, (Bottom, rep)) *)
  let add_bottom_to_ident i =
    match i with
    | Ident (name, rep) -> Ident (name, (EMFR.Bottom, rep))
  
  let rec mf_init_tag_pattern p =
    match p with
    | Wildcard -> MFSyntax.Wildcard
    | Binder x -> MFSyntax.Binder (add_bottom_to_ident x)
    | Constructor (cn, ps) ->
        MFSyntax.Constructor (
          cn,
          List.map mf_init_tag_pattern ps)

  let mf_init_tag_payload p =
    match p with
    | MTag s -> MFSyntax.MTag s
    | MLit l -> MFSyntax.MLit l
    | MVar v -> MFSyntax.MVar (add_bottom_to_ident v)
  
  let rec mf_init_tag_expr erep =
    let (e, rep) = erep in
    let res_e = 
      match e with
      | Literal l ->
          MFSyntax.Literal l
      | Var i ->
          MFSyntax.Var (add_bottom_to_ident i)
      |  Fun (arg, t, body) ->
          MFSyntax.Fun (
              add_bottom_to_ident arg,
              t,
              mf_init_tag_expr body)
      | App (f, actuals) ->
          MFSyntax.App (
              add_bottom_to_ident f, 
              List.map add_bottom_to_ident actuals)
      | Builtin (i, actuals) ->
          MFSyntax.Builtin (
              add_bottom_to_ident i,
              List.map add_bottom_to_ident actuals)
      | Let (i, topt, lhs, rhs) ->
          MFSyntax.Let (
              add_bottom_to_ident i,
              topt,
              mf_init_tag_expr lhs,
              mf_init_tag_expr rhs)
      | Constr (cname, ts, actuals) ->
          MFSyntax.Constr (
              cname,
              ts,
              List.map add_bottom_to_ident actuals)
      | MatchExpr (x, clauses) ->
          MFSyntax.MatchExpr (
              add_bottom_to_ident x,
              List.map (fun (p, e) ->
                  (mf_init_tag_pattern p, mf_init_tag_expr e)) clauses)
      | Fixpoint (f, t, body) ->
          MFSyntax.Fixpoint (
              add_bottom_to_ident f,
              t,
              mf_init_tag_expr body)
      | TFun (tvar, body) ->
          MFSyntax.TFun (
              add_bottom_to_ident tvar,
              mf_init_tag_expr body)
      | TApp (tf, arg_types) ->
          MFSyntax.TApp (
              add_bottom_to_ident tf,
              arg_types)
      | Message bs ->
          MFSyntax.Message (
              List.map (fun (s, p) -> (s, mf_init_tag_payload p)) bs) in
    (res_e, (EMFR.Bottom, rep))

  let rec mf_init_tag_stmt srep =
    let (s, rep) = srep in
    let res_s = 
      match s with
      | Load (x, y) ->
          MFSyntax.Load (
            add_bottom_to_ident x,
            add_bottom_to_ident y)
      | Store (x, y) -> 
          MFSyntax.Store (
            add_bottom_to_ident x,
            add_bottom_to_ident y)
      | Bind (x, e) ->
          MFSyntax.Bind (
            add_bottom_to_ident x,
            mf_init_tag_expr e)
      | MapUpdate (m, ks, v) ->
          MFSyntax.MapUpdate (
            add_bottom_to_ident m,
            List.map add_bottom_to_ident ks,
            match v with | None -> None | Some v' -> Some (add_bottom_to_ident v')
          )
      | MapGet (x, m, ks, retrieve) ->
          MFSyntax.MapGet (
            add_bottom_to_ident x,
            add_bottom_to_ident m,
            List.map add_bottom_to_ident ks,
            retrieve
          )
      | MatchStmt (x, pss) ->
          MFSyntax.MatchStmt (
            add_bottom_to_ident x,
            List.map (fun (p, ss) ->
                (mf_init_tag_pattern p,
                 List.map mf_init_tag_stmt ss)) pss)
      | ReadFromBC (x, s) ->
          MFSyntax.ReadFromBC (
            add_bottom_to_ident x, s)
      | AcceptPayment ->
          MFSyntax.AcceptPayment
      | SendMsgs x ->
          MFSyntax.SendMsgs (add_bottom_to_ident x)
      | CreateEvnt x ->
          MFSyntax.CreateEvnt (add_bottom_to_ident x)
      | Throw x ->
          MFSyntax.Throw (add_bottom_to_ident x) in
    (res_s, rep)

  let mf_init_tag_transition transition =
    let { tname ; tparams ; tbody } = transition in
    { MFSyntax.tname = tname;
      MFSyntax.tparams =
        List.map (fun (x, t) -> (add_bottom_to_ident x, t)) tparams;
      MFSyntax.tbody =
        List.map mf_init_tag_stmt tbody }
  
  let mf_init_tag_contract contract =
    let { cname ; cparams ; cfields ; ctrans } = contract in
    { MFSyntax.cname = cname;
      MFSyntax.cparams =
        List.map (fun (x, t) -> (add_bottom_to_ident x, t)) cparams;
      MFSyntax.cfields =
        List.map (fun (x, t, e) ->
            (add_bottom_to_ident x,
             t,
             mf_init_tag_expr e)) cfields;
      MFSyntax.ctrans =
        List.map mf_init_tag_transition ctrans }
  
  let mf_init_tag_library lib =
    let { lname ; lentries } = lib in
    { MFSyntax.lname = lname;
      MFSyntax.lentries = List.map
          (fun { lname ; lexp } ->
             { MFSyntax.lname = add_bottom_to_ident lname ;
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
  (* Strategy:                                           *)
  (*  - Expressions have an expected tag, which is       *)
  (* unified with the tag that can be extrapolated from  *)
  (* the usage.                                          *)
  (*  - Statement lists are first traversed to collect   *)
  (* all declared local variable and their tags. Then    *)
  (* they are traversed again to extrapolate new tags.   *)
  (*******************************************************)
  open MFSyntax
      
  (* Lattice implementation:
     t1 unifies with t2 if t1 and t2 are equal.
     Nothing else unifies, so return Top in all other cases *)
  let rec unify_tags t1 t2 =
    match t1, t2 with
    | Map x, Map y ->
        Map (unify_tags x y)
    | Option x, Option y ->
        Option (unify_tags x y)
    | x, y
      when x = y -> x
    | Bottom, x
    | x, Bottom   -> x
    | _, _       -> Top

  let unify_tag_with_option t1 t2 =
    match t2 with
    | Some t -> unify_tags t1 t
    | None -> t1
  
  let get_id_tag id =
    match id with
    | Ident (_, (tag, _)) -> tag
      
  let update_id_tag id new_tag =
    match id with
    | Ident (v, (_, rep)) -> Ident (v, (new_tag, rep))

  let lookup_var_tag i env =
    match AssocDictionary.lookup (get_id i) env with
    | Some t -> t
    | None -> get_id_tag i
  
  let lookup_var_tag2 i env1 env2 =
    match AssocDictionary.lookup (get_id i) env1 with
    | Some t -> t
    | None -> lookup_var_tag i env2

  let update_var_tag2 i t env1 env2 =
    match AssocDictionary.lookup (get_id i) env1 with
    | Some _ -> (AssocDictionary.update (get_id i) t env1, env2)
    | None -> (env1, AssocDictionary.update (get_id i) t env2)

  let insert_or_update_in_env x new_tag env =
    let old_tag = AssocDictionary.lookup (get_id x) env in
    match old_tag with
    | None -> AssocDictionary.insert (get_id x) new_tag env
    | Some t ->
        if new_tag = t
        then env
        else AssocDictionary.update (get_id x) new_tag env
  
  (* TODO: implement tag based on the builtin functions *)
  let builtin_tag _f _args = Map Bottom

  let rec get_pattern_vars acc p =
    match p with
    | Wildcard -> acc
    | Binder x -> x :: acc
    | Constructor (_, ps) ->
        List.fold_left get_pattern_vars acc ps

  let update_pattern_vars_tags p local_env =
    let rec walk p =
      match p with
      | Wildcard -> Wildcard
      | Binder x -> Binder (update_id_tag x (lookup_var_tag x local_env))
      | Constructor (s, ps) ->
          Constructor (s, List.map walk ps) in
    walk p

  let insert_pattern_vars_into_env p local_env =
    let pattern_vars = get_pattern_vars [] p in
    List.fold_left
      (fun l_env x ->
         AssocDictionary.insert (get_id x) (get_id_tag x) l_env) local_env pattern_vars
      
  let remove_pattern_vars_from_env p local_env =
    let pattern_vars = get_pattern_vars [] p in
    List.fold_left
      (fun l_env x -> AssocDictionary.remove (get_id x) l_env) local_env pattern_vars    

  let unify_pattern_tags ps =
    let rec walk acc_tag p =
      match p with
      | Wildcard -> acc_tag
      | Binder x -> unify_tags (get_id_tag x) acc_tag
      | Constructor (s, ps) ->
          match s with
          | "None" -> unify_tags (Option Bottom) acc_tag
          | "Some" ->
              (match acc_tag with
               | Bottom   -> Option (List.fold_left walk Bottom ps)
               | Option t -> Option (List.fold_left walk t ps)
               | _ -> Top)
          | "True"
          | "False" -> unify_tags acc_tag NotMoney
          | _ -> Top in
    List.fold_left walk Bottom ps
  
  let update_var_tag_payload p local_env =
    match p with
    | MTag s -> MFSyntax.MTag s
    | MLit l -> MFSyntax.MLit l
    | MVar v ->
        let tag =
          match AssocDictionary.lookup (get_id v) local_env with
          | None -> Top (* Should not happen *)
          | Some t -> t in
        match v with
        | Ident (name, (_, rep)) -> MFSyntax.MVar (Ident (name, (tag, rep)))

  let rec mf_tag_expr erep expected_tag field_env local_env =
    let unify t = unify_tags expected_tag t in
    let (e, (tag, rep)) = erep in
    let (new_e, new_e_tag, new_field_env, new_local_env, new_changes) = 
      match e with
      | Literal _ ->
          (* TODO: Deduce tag from type? *)
          (e, tag, field_env, local_env, false)
      | Var i ->
          let new_i_tag = unify (lookup_var_tag2 i local_env field_env) in
          let new_i = update_id_tag i new_i_tag in
          let (new_local_env, new_field_env) = update_var_tag2 i new_i_tag local_env field_env in
          (Var new_i, new_i_tag, new_field_env, new_local_env, new_i_tag <> (get_id_tag i))
      | Fun (arg, t, body) ->
          let body_expected_tag =
            match expected_tag with
            | Map x -> x
            | Bottom -> Bottom
            | _     -> Top in
          let body_local_env =
            AssocDictionary.insert (get_id arg) (get_id_tag arg) local_env in
          let ((_, (new_body_tag, _)) as new_body, res_field_env, res_body_local_env, body_changes) =
            mf_tag_expr body body_expected_tag field_env body_local_env in
          let res_arg_tag = lookup_var_tag arg res_body_local_env in
          (Fun (update_id_tag arg res_arg_tag, t, new_body),
           Map new_body_tag,
           res_field_env,
           AssocDictionary.remove (get_id arg) res_body_local_env,
           body_changes || (get_id_tag arg <> res_arg_tag))
      | App (f, args) ->
          let new_args = List.map (fun arg -> update_id_tag arg (lookup_var_tag2 arg local_env field_env)) args in
          let (args_changes, _) =
            List.fold_left
              (fun (acc_changes, new_args_acc) arg ->
                 match new_args_acc with
                 | [] -> (false, [])
                 | x :: rest -> (acc_changes || (get_id_tag x) <> (get_id_tag arg), rest))
              (false, new_args) args in
          let f_tag = unify_tags (lookup_var_tag2 f local_env field_env) (Map expected_tag) in
          let new_f = update_id_tag f f_tag in
          let (new_local_env, new_field_env) = update_var_tag2 f f_tag local_env field_env in
          let new_e_tag = 
            match f_tag with
            | Map t -> t
            | Bottom -> Bottom
            | _     -> Top in
          (App (new_f, new_args),
           new_e_tag,
           new_field_env,
           new_local_env,
           args_changes || f_tag <> get_id_tag f)
      | Builtin (f, args) ->
          let new_args = List.map (fun arg -> update_id_tag arg (lookup_var_tag2 arg local_env field_env)) args in
          let (args_changes, _) =
            List.fold_left
              (fun (acc_changes, new_args_acc) arg ->
                 match new_args_acc with
                 | [] -> (false, [])
                 | x :: rest -> (acc_changes || (get_id_tag x) <> (get_id_tag arg), rest))
              (false, new_args) args in
          let f_tag = unify_tags (builtin_tag f new_args) (Map expected_tag) in
          let new_f = update_id_tag f f_tag in
          (* Update library entry tag? *)
          let new_e_tag = 
            match f_tag with
            | Map t -> t
            | Bottom -> Bottom
            | _     -> Top in
          (Builtin (new_f, new_args),
           new_e_tag,
           field_env,
           local_env,
           args_changes || f_tag <> get_id_tag f)
      | Let (i, topt, lhs, rhs) ->
          let ((_, (new_lhs_tag, _)) as new_lhs, lhs_field_env, lhs_local_env, lhs_changes) =
            mf_tag_expr lhs (get_id_tag i) field_env local_env in
          let updated_lhs_local_env = AssocDictionary.insert (get_id i) new_lhs_tag lhs_local_env in
          let ((_, (new_rhs_tag, _)) as new_rhs, rhs_field_env, rhs_local_env, rhs_changes) =
            mf_tag_expr rhs expected_tag lhs_field_env updated_lhs_local_env in
          let new_i_tag = lookup_var_tag i rhs_local_env in
          let new_i = update_id_tag i new_i_tag in
          let res_local_env = AssocDictionary.remove (get_id i) rhs_local_env in
          (Let (new_i, topt, new_lhs, new_rhs),
           new_rhs_tag,
           rhs_field_env,
           res_local_env,
           lhs_changes || rhs_changes || new_i_tag <> get_id_tag i)
      | Constr (cname, ts, args) ->
          let new_args = List.map (fun arg -> update_id_tag arg (lookup_var_tag2 arg local_env field_env)) args in
          let (args_changes, _) =
            List.fold_left
              (fun (acc_changes, new_args_acc) arg ->
                 match new_args_acc with
                 | [] -> (false, [])
                 | x :: rest -> (acc_changes || (get_id_tag x) <> (get_id_tag arg), rest))
              (false, new_args) args in
          let tag =
            match cname with
            | "None" -> Option Bottom
            | "Some" -> Option (List.fold_left (fun _ arg -> (get_id_tag arg)) Bottom new_args)
            | "True"
            | "False" -> NotMoney
            | _ -> Top in
          (Constr (cname, ts, new_args),
           tag,
           field_env,
           local_env,
           args_changes)
      | MatchExpr (x, clauses) ->
          let (res_clauses, res_tag, new_field_env, new_local_env, clause_changes) =
            List.fold_right
              (fun (p, ep) (acc_clauses, acc_res_tag, acc_field_env, acc_local_env, acc_changes) ->
                 let sub_local_env =
                   insert_pattern_vars_into_env p acc_local_env in
                 let ((_, (new_e_tag, _)) as new_e, new_field_env, new_local_env, new_changes) =
                   mf_tag_expr ep expected_tag acc_field_env sub_local_env in
                 let new_p = update_pattern_vars_tags p new_local_env in
                 let res_local_env = remove_pattern_vars_from_env p new_local_env in
                 ((new_p, new_e) :: acc_clauses,
                  unify_tags acc_res_tag new_e_tag,
                  new_field_env,
                  res_local_env,
                  acc_changes || new_changes || p <> new_p))
              clauses
              ([], expected_tag, field_env, local_env, false) in
          let x_tag_usage = unify_pattern_tags (List.map (fun (p, _) -> p) res_clauses) in
          let new_x_tag = unify_tags (lookup_var_tag x local_env) x_tag_usage in
          let new_x = update_id_tag x new_x_tag in
          let res_local_env = AssocDictionary.update (get_id x) new_x_tag new_local_env in
          (MatchExpr (new_x, res_clauses),
           res_tag,
           new_field_env,
           res_local_env,
           clause_changes || (get_id_tag x) <> new_x_tag)
      | Fixpoint (_f, _t, _body) ->
          (* TODO: Library functions and polymorphism not yet handled. *)
          (e, Top, field_env, local_env, false)
      | TFun (_tvar, _body) ->
          (* TODO: Polymorphism not yet handled *)
          (e, Top, field_env, local_env, false)
      | TApp (_tf, _arg_types) ->
          (* TODO: Polymorphism not yet handled *)
          (e, Top, field_env, local_env, false)
      | Message bs ->
          (* Find _amount initializer and update env if necessary *)
          let (new_field_env, new_local_env) =
            match List.find_opt (fun (s, _) -> s = "_amount") bs with
            | None
            | Some (_, MTag _)
            | Some (_, MLit _) -> (field_env, local_env)
            | Some (_, MVar x) ->
                let old_env_tag = lookup_var_tag x local_env in
                let new_env_tag = unify_tags Money old_env_tag in
                let (new_local_env, new_field_env) = update_var_tag2 x new_env_tag local_env field_env in
                (new_field_env, new_local_env) in
          let updated_bs =
            List.map
              (fun (s, p) ->
                 match p with
                 | MTag _
                 | MLit _ -> (s, p)
                 | MVar x ->
                     (s, MVar (update_id_tag x (lookup_var_tag2 x new_local_env new_field_env)))) bs in
          (Message updated_bs,
           NotMoney,
           new_field_env,
           new_local_env,
           bs <> updated_bs) in
    let e_tag = unify new_e_tag in
    ((new_e, (e_tag, rep)), new_field_env, new_local_env, new_changes || tag <> e_tag)
    
  let mf_update_tag_for_field_assignment f x field_env local_env =
    let x_tag = lookup_var_tag x local_env in
    let f_tag = lookup_var_tag f field_env in
    let new_tag = unify_tags x_tag f_tag in
    let new_x = update_id_tag x new_tag in
    let new_f = update_id_tag f new_tag in
    let new_field_env = AssocDictionary.update (get_id f) new_tag field_env in
    let new_local_env = AssocDictionary.update (get_id x) new_tag local_env in
    (new_f, new_x, new_field_env, new_local_env)

  let update_ids_tags ids env =
    List.map
      (fun i ->
         let i_tag = lookup_var_tag i env in
         update_id_tag i i_tag) ids
  
  let rec mf_tag_stmt (srep : MFSyntax.stmt_annot) field_env local_env =
    let (s, rep) = srep in
    let (new_s, new_field_env, new_local_env, changes) =
      match s with
      | Load (x, f) ->
          let (new_f, new_x, new_field_env, tmp_local_env) =
            mf_update_tag_for_field_assignment f x field_env local_env in
          (* x is no longer in scope, so remove from local_env *)
          let new_local_env = AssocDictionary.remove (get_id x) tmp_local_env in
          (Load (new_x, new_f),
           new_field_env,
           new_local_env,
           (get_id_tag new_x) <> (get_id_tag x) || (get_id_tag new_f) <> (get_id_tag f))
      | Store (f, x) ->
          let (new_f, new_x, new_field_env, new_local_env) =
            mf_update_tag_for_field_assignment f x field_env local_env in
          (Store (new_f, new_x),
           new_field_env,
           new_local_env,
           (get_id_tag new_x) <> (get_id_tag x) || (get_id_tag new_f) <> (get_id_tag f))
      | Bind (x, e) ->
          let x_tag = lookup_var_tag x local_env in
          let e_local_env = AssocDictionary.remove (get_id x) local_env in
          let ((_, (new_e_tag, _)) as new_e, new_field_env, new_local_env, e_changes) =
            mf_tag_expr e x_tag field_env e_local_env in
          let new_x_tag = unify_tags x_tag new_e_tag in
          let new_x = update_id_tag x new_x_tag in
          (Bind (new_x, new_e),
           new_field_env,
           new_local_env,
           e_changes || (get_id_tag x) <> new_x_tag)
      | MapUpdate (m, ks, v_opt) ->
          let v_tag =
            match v_opt with
            | None -> Bottom
            | Some v -> lookup_var_tag v local_env in
          let m_tag_usage = List.fold_left (fun acc _ -> Map acc) v_tag ks in
          let m_tag = unify_tags m_tag_usage (lookup_var_tag m field_env) in
          let new_m = update_id_tag m m_tag in
          let new_field_env = AssocDictionary.update (get_id m) m_tag field_env in
          let new_ks = update_ids_tags ks local_env in
          let (new_v_opt, new_local_env) =
            match v_opt with
            | None -> (None, local_env)
            | Some v -> 
                let v_tag_usage =
                  List.fold_left
                    (fun acc_tag _ ->
                       match acc_tag with
                       | Map t -> t
                       | _ -> Top) m_tag ks in
                let new_v_tag = unify_tags v_tag_usage v_tag in
                let new_v = update_id_tag v new_v_tag in
                let new_local_env =
                  AssocDictionary.update (get_id v) new_v_tag local_env in
                (Some new_v, new_local_env) in
          (MapUpdate (new_m, new_ks, new_v_opt),
           new_field_env,
           new_local_env,
           (get_id_tag m) <> m_tag || new_v_opt <> v_opt || new_ks <> ks)
      | MapGet (x, m, ks, fetch) ->
          let x_tag = lookup_var_tag x local_env in
          let val_tag = 
            if fetch
            then
              match x_tag with
              | Option t -> t
              | Bottom -> Bottom
              | _ -> Top
            else
              Bottom in
          let m_tag_usage =
            List.fold_left (fun acc _ -> Map acc) val_tag ks in
          let m_tag = unify_tags m_tag_usage (lookup_var_tag m field_env) in
          let new_m = update_id_tag m m_tag in
          let new_field_env = AssocDictionary.update (get_id m) m_tag field_env in
          let new_local_env = AssocDictionary.remove (get_id x) local_env in
          let new_ks = update_ids_tags ks new_local_env in
          let new_x_tag =
            if fetch
            then
              unify_tags x_tag (Option val_tag)
            else
              NotMoney (* Bool *) in
          let new_x = update_id_tag x new_x_tag in
          (MapGet (new_x, new_m, new_ks, fetch),
           new_field_env,
           new_local_env,
           (get_id_tag x) <> new_x_tag || (get_id_tag m) <> m_tag || new_ks <> ks)
      | MatchStmt (x, clauses) -> 
          let (res_clauses, new_field_env, new_local_env, clause_changes) =
            List.fold_right
              (fun (p, sp) (acc_clauses, acc_field_env, acc_local_env, acc_changes) ->
                 let sub_local_env =
                   insert_pattern_vars_into_env p acc_local_env in
                 let (new_stmts, new_field_env, s_local_env, s_changes) =
                   mf_tag_stmts sp acc_field_env sub_local_env in
                 let new_p = update_pattern_vars_tags p s_local_env in
                 let new_local_env = remove_pattern_vars_from_env p s_local_env in
                 ((new_p, new_stmts) :: acc_clauses,
                  new_field_env,
                  new_local_env,
                  acc_changes || s_changes || p <> new_p))
              clauses
              ([], field_env, local_env, false) in
          let x_tag_usage = unify_pattern_tags (List.map (fun (p, _) -> p) res_clauses) in
          let new_x_tag = unify_tags (lookup_var_tag x local_env) x_tag_usage in
          let new_x = update_id_tag x new_x_tag in
          let res_local_env = AssocDictionary.update (get_id x) new_x_tag new_local_env in
          (MatchStmt (new_x, res_clauses),
           new_field_env,
           res_local_env,
           clause_changes || (get_id_tag x) <> new_x_tag)
      | ReadFromBC (x, s) ->
          let x_tag = unify_tags NotMoney (lookup_var_tag x local_env) in
          let new_x = update_id_tag x x_tag in
          let new_local_env = AssocDictionary.remove (get_id x) local_env in
          (ReadFromBC (new_x, s),
           field_env,
           new_local_env,
           (get_id_tag x) <> x_tag)
      | AcceptPayment -> (AcceptPayment, field_env, local_env, false)
      | SendMsgs m ->
          let m_tag = unify_tags NotMoney (lookup_var_tag m local_env) in
          let new_m = update_id_tag m m_tag in
          let new_local_env = AssocDictionary.update (get_id m) m_tag local_env in
          (SendMsgs new_m,
           field_env,
           new_local_env,
           (get_id_tag m) <> m_tag)
      | CreateEvnt e ->
          let e_tag = unify_tags NotMoney (lookup_var_tag e local_env) in
          let new_e = update_id_tag e e_tag in
          let new_local_env = AssocDictionary.update (get_id e) e_tag local_env in
          (CreateEvnt new_e,
           field_env,
           new_local_env,
           (get_id_tag e) <> e_tag)
      | Throw x ->
          let x_tag = unify_tags NotMoney (lookup_var_tag x local_env) in
          let new_x = update_id_tag x x_tag in
          let new_local_env = AssocDictionary.update (get_id x) x_tag local_env in
          (Throw new_x,
           field_env,
           new_local_env,
           (get_id_tag x) <> x_tag) in
    ((new_s, rep), new_field_env, new_local_env, changes)

    and mf_tag_stmts ss field_env local_env =
      let init_local_env =
        List.fold_left
          (fun acc_env srep ->
             let (s, _) = srep in
             match s with
             | Load (x, _)
             | Bind (x, _)
             | MapGet (x, _, _, _)
             | ReadFromBC (x, _)
               -> AssocDictionary.insert (get_id x) (get_id_tag x) acc_env
             | _ -> acc_env) local_env ss in
      List.fold_right
        (fun s (acc_ss, acc_field_env, acc_local_env, acc_changes) ->
           let (new_s, new_field_env, new_local_env, new_changes) =
             mf_tag_stmt s acc_field_env acc_local_env in
           (new_s :: acc_ss,
            new_field_env,
            new_local_env,
            new_changes || acc_changes))
        ss
        ([], field_env, init_local_env, false)

    let mf_tag_transition t field_env =
      let { tname ; tparams ; tbody } = t in
      let empty_local_env = AssocDictionary.make_dict() in
      let implicit_local_env =
        AssocDictionary.insert "_amount" Money 
          (AssocDictionary.insert "_recipient" NotMoney
             (AssocDictionary.insert "_tag" NotMoney empty_local_env)) in
      let param_local_env =
        List.fold_left
          (fun acc_env (p, _) ->
             AssocDictionary.insert (get_id p) (get_id_tag p) acc_env)
          implicit_local_env
          tparams in
      let (new_tbody, new_field_env, new_local_env, body_changes) =
        mf_tag_stmts tbody field_env param_local_env in
      let (new_params, new_changes) =
        List.fold_right
          (fun (p, typ) (acc_ps, acc_changes) ->
             let new_tag = lookup_var_tag p new_local_env in
             ((update_id_tag p new_tag, typ) :: acc_ps,
              acc_changes || (get_id_tag p) <> new_tag))
               tparams ([], body_changes) in
      ({ tname = tname ; tparams = new_params ; tbody = new_tbody },
       new_field_env,
       new_changes)

    let mf_tag_contract c =
      let { cname ; cparams ; cfields ; ctrans } = c in
      let empty_field_env = AssocDictionary.make_dict () in
      let implicit_field_env = AssocDictionary.insert "_balance" Money empty_field_env in
      let param_field_env =
        List.fold_left
          (fun acc_env (p, _) ->
             AssocDictionary.insert (get_id p) (get_id_tag p) acc_env)
          implicit_field_env
          cparams in
      let init_field_env =
        List.fold_left
          (fun acc_env (f, _, e) ->
             let ((_, (e_tag, _)), _, _, _) =
                  mf_tag_expr e Bottom (AssocDictionary.make_dict ()) (AssocDictionary.make_dict ()) in
             AssocDictionary.insert (get_id f) e_tag acc_env)
          param_field_env
          cfields in
      let rec tagger transitions field_env =
        let (new_ts, new_field_env, ctrans_changes) =
          List.fold_right
            (fun t (acc_ts, acc_field_env, acc_changes) ->
               let (new_t, new_field_env, t_changes) =
                 mf_tag_transition t acc_field_env in
               (new_t :: acc_ts, new_field_env, acc_changes || t_changes))
            transitions ([], field_env, false) in
        if ctrans_changes
        then
          tagger new_ts new_field_env
        else (new_ts, new_field_env) in
      let (new_ctrans, new_field_env) = tagger ctrans init_field_env in
      let new_fields =
        List.fold_right
          (fun (f, t, e) acc_fields ->
             let new_tag = lookup_var_tag f new_field_env in
             (update_id_tag f new_tag, t, e) :: acc_fields)
          cfields [] in
      let new_params =
        List.fold_right
          (fun (p, t) acc_params ->
             let new_tag = lookup_var_tag p new_field_env in
             (update_id_tag p new_tag, t) :: acc_params)
          cparams [] in
      { cname = cname ;
        cparams = new_params ;
        cfields = new_fields ;
        ctrans = new_ctrans }

    let mf_tag_module m =
      let { cname ; libs ; elibs ; contr } = m in
      let new_contr = mf_tag_contract contr in
      { cname = cname ;
        libs = libs ;
        elibs = elibs ;
        contr = new_contr }
    
  (*******************************************************)
  (*                Main entry function                  *)
  (*******************************************************)

  let main cmod =
    let init_mod = mf_init_tag_module cmod in
    let new_mod = mf_tag_module init_mod in
    (List.map (fun (p, _) -> (get_id p, get_id_tag p)) new_mod.contr.cparams)
    @
    (List.map (fun (f, _, _) -> (get_id f, get_id_tag f)) new_mod.contr.cfields)

end
