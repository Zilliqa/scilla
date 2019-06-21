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

open Core
open Sexplib.Std
open Syntax
open Datatypes
open TypeUtil
open Utils

module CashflowRep (R : Rep) = struct
  type money_tag =
    | NoInfo
    | NotMoney
    | Money
    | Map of money_tag
    | Adt of string * money_tag list (* name of adt paired with tags of type params *)
    | Inconsistent
  [@@deriving sexp]

  let rec money_tag_to_string tag =
    match tag with
    | Adt (n, ts) -> "(" ^ n ^ " " ^ (String.concat ~sep:" " (List.map ~f:money_tag_to_string ts)) ^ ")"
    | Map t -> "(Map " ^ (money_tag_to_string t) ^ ")"
    | _ -> sexp_of_money_tag tag |> Sexplib.Sexp.to_string


  type rep = money_tag * R.rep
  [@@deriving sexp]

  let get_loc r = match r with | (_, rr) -> R.get_loc rr

  let add_tag_to_id s tag =
    match s with
    | Ident (n, r) -> Ident (n, (tag, r))

  let mk_id s =
    add_tag_to_id s NoInfo

  let mk_id_address s = mk_id (R.mk_id_address s)
  let mk_id_uint128 s = mk_id (R.mk_id_uint128 s)
  let mk_id_uint32 s = mk_id (R.mk_id_uint32 s)
  let mk_id_bnum    s = mk_id (R.mk_id_bnum s)
  let mk_id_string  s = mk_id (R.mk_id_string s)
  
  let parse_rep s = (NoInfo, R.parse_rep s)
  let get_rep_str r = match r with | (_, rr) -> R.get_rep_str rr
end


module ScillaCashflowChecker
    (SR : Rep)
    (ER : sig
       include Rep
       val [@warning "-32"] get_type : rep -> PlainTypes.t inferred_type
     end) = struct

  module SCFR = SR
  module ECFR = CashflowRep (ER)
  module TypedSyntax = ScillaSyntax (SR) (ER)
  module CFSyntax = ScillaSyntax (SCFR) (ECFR)

  open TypedSyntax
  open ECFR

  (*******************************************************)
  (*     Initial traversal: Set every tag to NoInfo      *)
  (*******************************************************)
  
  (* Lift Ident (n, rep) to Ident (n, (NoInfo, rep)) *)
  let add_noinfo_to_ident i = ECFR.mk_id i

  let add_money_or_mapmoney_to_ident i typ =
    let rec create_money_tag typ =
      match typ with
      | MapType (_, vtyp) ->
          ECFR.Map (create_money_tag vtyp)
      | _ -> ECFR.Money in
    ECFR.add_tag_to_id i (create_money_tag typ)
  let add_noinfo_to_builtin (op, rep) = (op, (ECFR.NoInfo, rep))
  
  let rec cf_init_tag_pattern p =
    match p with
    | Wildcard -> CFSyntax.Wildcard
    | Binder x -> CFSyntax.Binder (add_noinfo_to_ident x)
    | Constructor (cn, ps) ->
        CFSyntax.Constructor (
          cn,
          List.map ~f:cf_init_tag_pattern ps)

  let cf_init_tag_payload p =
    match p with
    | MLit l -> CFSyntax.MLit l
    | MVar v -> CFSyntax.MVar (add_noinfo_to_ident v)
  
  let rec cf_init_tag_expr erep =
    let (e, rep) = erep in
    let res_e = 
      match e with
      | Literal l ->
          CFSyntax.Literal l
      | Var i ->
          CFSyntax.Var (add_noinfo_to_ident i)
      |  Fun (arg, t, body) ->
          CFSyntax.Fun (
              add_noinfo_to_ident arg,
              t,
              cf_init_tag_expr body)
      | App (f, actuals) ->
          CFSyntax.App (
              add_noinfo_to_ident f, 
              List.map ~f:add_noinfo_to_ident actuals)
      | Builtin (op, actuals) ->
          CFSyntax.Builtin (
              add_noinfo_to_builtin op,
              List.map ~f:add_noinfo_to_ident actuals)
      | Let (i, topt, lhs, rhs) ->
          CFSyntax.Let (
              add_noinfo_to_ident i,
              topt,
              cf_init_tag_expr lhs,
              cf_init_tag_expr rhs)
      | Constr (cname, ts, actuals) ->
          CFSyntax.Constr (
              cname,
              ts,
              List.map ~f:add_noinfo_to_ident actuals)
      | MatchExpr (x, clauses) ->
          CFSyntax.MatchExpr (
              add_noinfo_to_ident x,
              List.map ~f:(fun (p, e) ->
                  (cf_init_tag_pattern p, cf_init_tag_expr e)) clauses)
      | Fixpoint (f, t, body) ->
          CFSyntax.Fixpoint (
              add_noinfo_to_ident f,
              t,
              cf_init_tag_expr body)
      | TFun (tvar, body) ->
          CFSyntax.TFun (
              add_noinfo_to_ident tvar,
              cf_init_tag_expr body)
      | TApp (tf, arg_types) ->
          CFSyntax.TApp (
              add_noinfo_to_ident tf,
              arg_types)
      | Message bs ->
          CFSyntax.Message (
              List.map ~f:(fun (s, p) -> (s, cf_init_tag_payload p)) bs) in
    (res_e, (ECFR.NoInfo, rep))

  let rec cf_init_tag_stmt srep =
    let (s, rep) = srep in
    let res_s = 
      match s with
      | Load (x, y) ->
          CFSyntax.Load (
            add_noinfo_to_ident x,
            add_noinfo_to_ident y)
      | Store (x, y) -> 
          CFSyntax.Store (
            add_noinfo_to_ident x,
            add_noinfo_to_ident y)
      | Bind (x, e) ->
          CFSyntax.Bind (
            add_noinfo_to_ident x,
            cf_init_tag_expr e)
      | MapUpdate (m, ks, v) ->
          CFSyntax.MapUpdate (
            add_noinfo_to_ident m,
            List.map ~f:add_noinfo_to_ident ks,
            match v with | None -> None | Some v' -> Some (add_noinfo_to_ident v')
          )
      | MapGet (x, m, ks, retrieve) ->
          CFSyntax.MapGet (
            add_noinfo_to_ident x,
            add_noinfo_to_ident m,
            List.map ~f:add_noinfo_to_ident ks,
            retrieve
          )
      | MatchStmt (x, pss) ->
          CFSyntax.MatchStmt (
            add_noinfo_to_ident x,
            List.map ~f:(fun (p, ss) ->
                (cf_init_tag_pattern p,
                 List.map ~f:cf_init_tag_stmt ss)) pss)
      | ReadFromBC (x, s) ->
          CFSyntax.ReadFromBC (
            add_noinfo_to_ident x, s)
      | AcceptPayment ->
          CFSyntax.AcceptPayment
      | SendMsgs x ->
          CFSyntax.SendMsgs (add_noinfo_to_ident x)
      | CreateEvnt x ->
          CFSyntax.CreateEvnt (add_noinfo_to_ident x)
      | CallProc (p, args) ->
          CFSyntax.CallProc (p, List.map args ~f:add_noinfo_to_ident)
      | Throw xopt ->
          (match xopt with
          | Some x -> CFSyntax.Throw (Some (add_noinfo_to_ident x))
          | None -> CFSyntax.Throw (None)
          )
      in
    (res_s, rep)

  let cf_init_tag_component component =
    let { comp_type; comp_name ; comp_params ; comp_body } = component in
    { CFSyntax.comp_type = comp_type;
      CFSyntax.comp_name = comp_name;
      CFSyntax.comp_params =
        List.map ~f:(fun (x, t) -> (add_noinfo_to_ident x, t)) comp_params;
      CFSyntax.comp_body =
        List.map ~f:cf_init_tag_stmt comp_body }
  
  let cf_init_tag_contract contract token_fields =
    let { cname ; cparams ; cfields ; ccomps } = contract in
    let token_fields_contains x =
      List.exists ~f:(fun token_field -> get_id x = token_field) token_fields in
    { CFSyntax.cname = cname;
      CFSyntax.cparams =
        List.map ~f:(fun (x, t) ->
            (if token_fields_contains x
             then add_money_or_mapmoney_to_ident x t
             else add_noinfo_to_ident x),
            t) cparams;
      CFSyntax.cfields =
        List.map ~f:(fun (x, t, e) ->
            ((if token_fields_contains x
             then add_money_or_mapmoney_to_ident x t
             else add_noinfo_to_ident x),
             t,
             cf_init_tag_expr e)) cfields;
      CFSyntax.ccomps =
        List.map ~f:cf_init_tag_component ccomps }
    
  let cf_init_tag_type_def tdef =
    let { cname ; c_arg_types } = tdef in
    { CFSyntax.cname = add_noinfo_to_ident cname ; CFSyntax.c_arg_types = c_arg_types }
  
  let cf_init_tag_library lib =
    let { lname ; lentries } = lib in
    let init_tag_entry entry =
      match entry with
      | LibVar (lname, ltype, lexp) ->
          CFSyntax.LibVar (add_noinfo_to_ident lname, ltype, cf_init_tag_expr lexp)
      | LibTyp (lname, type_defs) ->
          CFSyntax.LibTyp (add_noinfo_to_ident lname,
                           List.map ~f:cf_init_tag_type_def type_defs) in
    { CFSyntax.lname = lname;
      CFSyntax.lentries = List.map ~f:init_tag_entry lentries }
  
  let cf_init_tag_module cmod token_fields =
    let { smver; cname; libs; elibs; contr } = cmod in
    let res_libs =
      match libs with
      | None -> None
      | Some l -> Some (cf_init_tag_library l) in
    { CFSyntax.smver = smver;
      CFSyntax.cname = cname;
      CFSyntax.libs = res_libs;
      CFSyntax.elibs = elibs;
      CFSyntax.contr = cf_init_tag_contract contr token_fields }
  
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
  open CFSyntax
      
  (* Least upper bound in the money_tag lattice. *)
  let rec lub_tags t1 t2 =
    match t1, t2 with
    | Inconsistent , _
    | _            , Inconsistent  -> Inconsistent
    | NoInfo       , x
    | x            , NoInfo        -> x
    | Map x        , Map y         -> Map (lub_tags x y)
    | Adt (n1, ts1), Adt (n2, ts2)
      when n1 = n2                 ->
        (match List.map2 ts1 ts2 ~f:lub_tags with
         | Ok res -> Adt (n1, res)
         | Unequal_lengths -> Inconsistent)
    | Money        , Money         -> Money
    | NotMoney     , NotMoney      -> NotMoney
    | _            , _             -> Inconsistent

  (* Greatest lower bound in the money_tag lattice. *)
  let rec glb_tags t1 t2 =
    match t1, t2 with
    | Inconsistent , x
    | x            , Inconsistent  -> x
    | NoInfo       , _
    | _            , NoInfo        -> NoInfo
    | Map x        , Map y         -> Map (glb_tags x y)
    | Adt (n1, ts1), Adt (n2, ts2)
      when n1 = n2                 ->
        (match List.map2 ts1 ts2 ~f:glb_tags with
         | Ok res -> Adt (n1, res)
         | Unequal_lengths -> Inconsistent)
    | Money        , Money         -> Money
    | NotMoney     , NotMoney      -> NotMoney
    | _            , _             -> NoInfo

  (*******************************************************)
  (*           Helper functions for ADTs                 *)
  (*******************************************************)

  let ctr_to_adt_tag ctr_name arg_tags =
    let open DataTypeDictionary in
    match lookup_constructor ctr_name with
    | Error _       ->
        (* We don't allow failures at this stage of the checker *)
        Inconsistent
    | Ok (adt, _) ->
        match adt.tparams with
        | [] ->
            (* No type parameters - check for special cases *)
            (* Case 1 (Bool case): No constructor takes arguments : NotMoney *)
            if List.for_all adt.tmap
                ~f:(fun (_, arg_typs) ->
                    match arg_typs with
                    | [] -> true
                    | _ -> false)
            then NotMoney
            (* Case 2 (Nat case): 2 constructors. One constructor takes 
               1 argument of same type, other constructor
               takes no argument : NoInfo *)
            else if List.length adt.tconstr = 2 &&
                    List.exists adt.tmap
                      ~f:(fun (_, arg_typs) ->
                          match arg_typs with
                          | [] -> true
                          | _ -> false) &&
                    List.exists adt.tmap
                      ~f:(fun (_, arg_typs) ->
                          match arg_typs with
                          | [ADT (arg_typ_name, _)] ->
                              arg_typ_name = adt.tname
                          | _ -> false)
            then NoInfo
            else Adt (adt.tname, [])
        | _ ->
            (* Deduce mapping from type arguments to tags based on
               constructor argument tags *)
            match constr_tmap adt ctr_name with
            | None ->
                (* No tmap for constructor = doesn't take arguments *)
                Adt (adt.tname, List.map adt.tparams ~f:(fun _ -> NoInfo))
            | Some arg_typs ->
                (* Default mapping : 'A -> NoInfo *)
                let init_targ_to_tag_map =
                  List.map adt.tparams ~f:(fun tparam -> (tparam, NoInfo)) in
                let update_targ_tag targ new_tag map =
                  let current_tag =
                    match List.Assoc.find map ~equal:(=) targ with
                    | None -> Inconsistent
                    | Some t -> t in
                  List.Assoc.add map ~equal:(=) targ (lub_tags current_tag new_tag) in
                let rec match_arg_tag_with_typ arg_typ arg_tag targ_tag_map =
                  match arg_typ with
                  | TypeVar v           -> update_targ_tag v arg_tag targ_tag_map
                  | MapType (_, vt)
                  | FunType (_, vt)     ->
                      (match arg_tag with
                       | Map vtag -> match_arg_tag_with_typ vt vtag targ_tag_map
                       | NoInfo   -> match_arg_tag_with_typ vt NoInfo targ_tag_map
                       | _        -> targ_tag_map)
                  | ADT (_, adt_params) ->
                      (match arg_tag with
                       | Adt (_, adt_param_tags) ->
                           let (new_map, _) = 
                             List.fold_left adt_params ~init:(targ_tag_map, adt_param_tags)
                               ~f:(fun (map, adt_param_tags) adt_param ->
                                   match adt_param_tags with
                                   | []      -> (map, [])
                                   | t :: ts -> (match_arg_tag_with_typ adt_param t map, ts)) in
                           new_map
                       | _                       -> targ_tag_map)
                  | PrimType _
                  | PolyFun (_, _)
                  | Unit                -> targ_tag_map in
                let (tvar_tag_map, _) =
                  List.fold_left arg_typs ~init:(init_targ_to_tag_map, arg_tags)
                    ~f:(fun (map, arg_tags) arg_typ ->
                        match arg_tags with
                        | []      -> (map, [])
                        | t :: ts -> (match_arg_tag_with_typ arg_typ t map, ts)) in
                let final_adt_arg_tags = 
                  List.map adt.tparams
                    ~f:(fun tparam ->
                        match List.Assoc.find tvar_tag_map ~equal:(=) tparam with
                        | None   -> Inconsistent
                        | Some t -> t) in
                Adt (adt.tname, final_adt_arg_tags)

  let ctr_pattern_to_subtags ctr_name expected_tag =
    let open DataTypeDictionary in
    match lookup_constructor ctr_name with
    | Error _ ->
        (* Catch errors as We don't allow failures at this stage of the checker *)
        None
    | Ok (adt, _) ->
        match constr_tmap adt ctr_name with
        | None ->
            (* No tmap = No constructor arguments *)
            Some []
        | Some tmap ->
            let tvar_tag_map =
              let zipped_tvar_tags =
                match expected_tag with
                | Adt (exp_typ_name, arg_tags)
                  when exp_typ_name = adt.tname ->
                    List.zip adt.tparams arg_tags
                | NoInfo (* Nothing known *)
                | Money (* Nat case *)
                | NotMoney (* Nat case *)
                  -> Some (List.map adt.tparams ~f:(fun tparam -> (tparam, expected_tag)))
                | _ ->
                    (* Don't let inconsistent tags infect subpatterns *)
                    Some (List.map adt.tparams ~f:(fun tparam -> (tparam, NoInfo))) in
              match zipped_tvar_tags with
              | None ->
                  (* Can only happen if arg_tags in Adt case has wrong length *)
                  List.map adt.tparams ~f:(fun tparam -> (tparam, Inconsistent))
              | Some map -> map in
            let rec tag_tmap t =
              match t with
              | PrimType _ ->
                  (* TODO: Fixed constructor argument types
                           should be analysed *)
                  NoInfo
              | MapType (_, vt)
              | FunType (_, vt) ->
                  Map (tag_tmap vt)
              | ADT (adt_name, arg_typs) ->
                  Adt (adt_name, List.map arg_typs ~f:tag_tmap)
              | TypeVar tvar ->
                  (match List.Assoc.find tvar_tag_map ~equal:(=) tvar with
                   | Some tag -> tag
                   | None -> Inconsistent)
              | _ -> Inconsistent in
            Some (List.map tmap ~f:tag_tmap)

  let ctr_arg_filter targ =
    match targ with
    | PrimType _
    | MapType _
    | FunType _ -> true
    | ADT _     (* TODO: Detect induction, and ignore only when inductive *)
    | TypeVar _ (* TypeVars tagged at type level *)
    | PolyFun _ (* Ignore *)
    | Unit ->   (* Ignore *)
        false
          
  let init_ctr_tag_map () =
    let open DataTypeDictionary in
    let all_ctrs = get_all_ctrs () in
    List.fold_left all_ctrs ~init:[]
      ~f:(fun acc (adt, ctr) ->
          match constr_tmap adt ctr.cname with
          | None ->
              (* No constructor arguments - ignore *)
              acc
          | Some targs ->
              let tag_list = List.map targs
                  ~f:(fun t -> if ctr_arg_filter t then Some NoInfo else None) in
              if List.exists tag_list ~f:Option.is_some
              then (ctr.cname, tag_list) :: acc
              else acc)

  let update_ctr_tag_map ctr_tag_map ctr_name arg_tags =
    match List.Assoc.find ctr_tag_map ~equal:(=) ctr_name with
    | None ->
        (* Ignored constructor *)
        None
    | Some arg_map_tags ->
        match List.map2 arg_map_tags arg_tags
                ~f:(fun arg_map_tag arg_tag ->
                    Option.map arg_map_tag ~f:(lub_tags arg_tag)) with
        | Ok new_tags
          when new_tags <> arg_map_tags ->
            Some (List.Assoc.add ctr_tag_map ~equal:(=) ctr_name new_tags)
        | _ ->
            None
              
  (*******************************************************)
  (*      Helper functions for local variables           *)
  (*******************************************************)

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
  
  let update_ids_tags ids env =
    List.map
      ~f:(fun i ->
         let i_tag = lookup_var_tag i env in
         update_id_tag i i_tag) ids

  let lookup_var_tag2 i env1 env2 =
    match AssocDictionary.lookup (get_id i) env1 with
    | Some t -> t
    | None -> lookup_var_tag i env2

  let update_var_tag2 i t env1 env2 =
    match AssocDictionary.lookup (get_id i) env1 with
    | Some _ -> (AssocDictionary.update (get_id i) t env1, env2)
    | None -> (env1, AssocDictionary.update (get_id i) t env2)

  (*******************************************************)
  (*      Helper functions for builtin functions         *)
  (*******************************************************)

  (* Calculate the signature of a builtin function.

     Step 1: Calculate candidate signatures based on the 
     desired result tag and each argument tag.

     - For each tag t, pick every least upper bound of that 
     tag that makes sense for that result/argument. 
     Call these bounds b_t.

     - For each b_t, find all sets of tags satisfying that 
     the use of those tags in the other argument/result positions 
     is the greatest lower bound of a consistent use of tags 
     satisfying b_t. These sets along with b_t are considered 
     the candidate sigantures for t, called C_t.

     Step 2: Consider the elements of C_t1 x C_t2 x ..., i.e., 
     the cartesian product of the candidate signature sets for 
     each t.

     For each element, calculate the least upper bound of all 
     the tags in the signatures of the element. Call the 
     resulting set of candidate signatures C.

     Step 3: Calculate the greatest lower bound of C. *)
  let builtin_signature f res_tag args_tags =
    let lub_sigs c_rs c_ass =
      let c =
        List.fold_left c_ass ~init:c_rs 
          ~f:(fun partial_c c_t ->
             List.fold_left partial_c ~init:[]
               ~f:(fun acc_partial_c (partial_c_res_tag, partial_c_args_tags) ->
                  List.fold_left c_t ~init:acc_partial_c
                    ~f:(fun acc_lub_sigs (c_t_res_tag, c_t_args_tags) ->
                        let res_sigs =
                          match List.map2 ~f:lub_tags partial_c_args_tags c_t_args_tags with
                          | Ok res          -> (lub_tags partial_c_res_tag c_t_res_tag, res)
                          | Unequal_lengths -> (Inconsistent, [NoInfo]) in
                        
                        res_sigs :: acc_lub_sigs))) in
      List.fold_left c ~init:( Inconsistent , List.map ~f:(fun _ -> Inconsistent ) args_tags )
        ~f:(fun (glb_res_tag, glb_args_tags) (c_res_tag, c_args_tags) ->
            ( glb_tags glb_res_tag c_res_tag,
              match List.map2 ~f:glb_tags glb_args_tags c_args_tags with
              | Ok res -> res
              | Unequal_lengths -> [Inconsistent]))
        in
    let (c_r, c_as) =
      match fst f with
      | Builtin_put ->
          let c_r_sigs =
            match res_tag with
            | Map t  -> [ ( Map t        , [ Map t      ; NotMoney ; t      ] ) ]
            | NoInfo -> [ ( Map NoInfo   , [ Map NoInfo ; NotMoney ; NoInfo ] ) ]
            | _      -> [ ( Inconsistent , [ Map NoInfo ; NotMoney ; NoInfo ] ) ] in
          let c_as_sigs =
            match args_tags with
            | [ m ; k ; v ] ->
                let m_sig =
                  match m with
                  | Map t  -> [ ( Map t      , [ Map t        ; NotMoney ; t      ] ) ] 
                  | NoInfo -> [ ( Map NoInfo , [ Map NoInfo   ; NotMoney ; NoInfo ] ) ] 
                  | _      -> [ ( Map NoInfo , [ Inconsistent ; NotMoney ; NoInfo ] ) ] in
                let k_sig =
                  match k with
                  | NotMoney
                  | NoInfo   -> [ ( Map NoInfo , [ Map NoInfo ; NotMoney     ; NoInfo ] ) ]
                  | _        -> [ ( Map NoInfo , [ Map NoInfo ; Inconsistent ; NoInfo ] ) ] in
                let v_sig =
                  match v with
                  | _        -> [ ( Map v , [ Map v ; NotMoney ; v ] ) ] in
                [ m_sig ; k_sig ; v_sig ]
            | _             ->
                (* Error *)
                [[ ( Inconsistent , List.map ~f:(fun _ -> Inconsistent) args_tags ) ]] in
          (c_r_sigs, c_as_sigs)
      | Builtin_remove ->
          let c_r_sigs =
            match res_tag with
            | Map t  -> [ ( Map t        , [ Map t      ; NotMoney ] ) ]
            | NoInfo -> [ ( Map NoInfo   , [ Map NoInfo ; NotMoney ] ) ]
            | _      -> [ ( Inconsistent , [ Map NoInfo ; NotMoney ] ) ] in
          let c_as_sigs =
            match args_tags with
            | [ m ; k ] ->
                let m_sig =
                  match m with
                  | Map t  -> [ ( Map t      , [ Map t        ; NotMoney ] ) ] 
                  | NoInfo -> [ ( Map NoInfo , [ Map NoInfo   ; NotMoney ] ) ] 
                  | _      -> [ ( Map NoInfo , [ Inconsistent ; NotMoney ] ) ] in
                let k_sig =
                  match k with
                  | NotMoney
                  | NoInfo   -> [ ( Map NoInfo , [ Map NoInfo ; NotMoney     ] ) ]
                  | _        -> [ ( Map NoInfo , [ Map NoInfo ; Inconsistent ] ) ] in
                [ m_sig ; k_sig ]
            | _             ->
                (* Error *)
                [[ ( Inconsistent , List.map ~f:(fun _ -> Inconsistent) args_tags ) ]] in
          (c_r_sigs, c_as_sigs)
      | Builtin_get ->
          let c_r_sigs =
            match res_tag with
            | Adt ("Option", [t]) -> [ ( Adt ("Option", [t]      ) , [ Map t      ; NotMoney ] ) ]
            | NoInfo              -> [ ( Adt ("Option", [NoInfo] ) , [ Map NoInfo ; NotMoney ] ) ]
            | _                   -> [ ( Inconsistent  , [ Map NoInfo ; NotMoney ] ) ] in
          let c_as_sigs =
            match args_tags with
            | [ m ; k ] ->
                let m_sig =
                  match m with
                  | Map t  -> [ ( Adt ("Option", [t     ] ) , [ Map t        ; NotMoney ] ) ] 
                  | NoInfo -> [ ( Adt ("Option", [NoInfo] ) , [ Map NoInfo   ; NotMoney ] ) ] 
                  | _      -> [ ( Adt ("Option", [NoInfo] ) , [ Inconsistent ; NotMoney ] ) ] in
                let k_sig =
                  match k with
                  | NotMoney
                  | NoInfo   -> [ ( Adt ("Option", [NoInfo] ) , [ Map NoInfo ; NotMoney     ] ) ]
                  | _        -> [ ( Adt ("Option", [NoInfo] ) , [ Map NoInfo ; Inconsistent ] ) ] in
                [ m_sig ; k_sig ]
            | _             ->
                (* Error *)
                [[ ( Inconsistent , List.map ~f:(fun _ -> Inconsistent) args_tags ) ]] in
          (c_r_sigs, c_as_sigs)
      | Builtin_contains ->
          let c_r_sigs =
            match res_tag with
            | NotMoney
            | NoInfo   -> [ ( NotMoney     , [ Map NoInfo ; NotMoney ] ) ]
            | _        -> [ ( Inconsistent , [ Map NoInfo ; NotMoney ] ) ] in
          let c_as_sigs =
            match args_tags with
            | [ m ; k ] ->
                let m_sig =
                  match m with
                  | Map t  -> [ ( NotMoney , [ Map t        ; NotMoney ] ) ] 
                  | NoInfo -> [ ( NotMoney , [ Map NoInfo   ; NotMoney ] ) ] 
                  | _      -> [ ( NotMoney , [ Inconsistent ; NotMoney ] ) ] in
                let k_sig =
                  match k with
                  | NotMoney
                  | NoInfo   -> [ ( NotMoney , [ Map NoInfo ; NotMoney     ] ) ]
                  | _        -> [ ( NotMoney , [ Map NoInfo ; Inconsistent ] ) ] in
                [ m_sig ; k_sig ]
            | _             ->
                (* Error *)
                [[ ( Inconsistent , List.map ~f:(fun _ -> Inconsistent) args_tags ) ]] in
          (c_r_sigs, c_as_sigs)
      | Builtin_to_list ->
          (* Lists not supported, so use Inconsistent *)
          let c_r_sigs =
            [ ( Inconsistent , [ Map NoInfo ] ) ] in
          let c_as_sigs =
            match args_tags with
            | [ m ] ->
                let m_sig =
                  match m with
                  | Map t  -> [ ( Inconsistent , [ Map t        ] ) ] 
                  | NoInfo -> [ ( Inconsistent , [ Map NoInfo   ] ) ] 
                  | _      -> [ ( Inconsistent , [ Inconsistent ] ) ] in
                [ m_sig ]
            | _             ->
                (* Error *)
                [[ ( Inconsistent , List.map ~f:(fun _ -> Inconsistent) args_tags ) ]] in
          (c_r_sigs, c_as_sigs)
      | Builtin_size ->
          let c_r_sigs =
            match res_tag with
            | NotMoney
            | NoInfo   -> [ ( NotMoney     , [ Map NoInfo ] ) ]
            | _        -> [ ( Inconsistent , [ Map NoInfo ] ) ] in
          let c_as_sigs =
            match args_tags with
            | [ m ] ->
                let m_sig =
                  match m with
                  | Map t  -> [ ( NotMoney , [ Map t        ] ) ] 
                  | NoInfo -> [ ( NotMoney , [ Map NoInfo   ] ) ] 
                  | _      -> [ ( NotMoney , [ Inconsistent ] ) ] in
                [ m_sig ]
            | _             ->
                (* Error *)
                [[ ( Inconsistent , List.map ~f:(fun _ -> Inconsistent) args_tags ) ]] in
          (c_r_sigs, c_as_sigs)
      | Builtin_eq
      | Builtin_lt ->
          let c_r_sigs =
            match res_tag with
            | NotMoney
            | NoInfo   -> [ ( NotMoney     , [ NotMoney ; NotMoney ] ) ;
                            ( NotMoney     , [ Money    ; Money    ] ) ]
            | _        -> [ ( Inconsistent , [ NotMoney ; NotMoney ] ) ;
                            ( Inconsistent , [ Money    ; Money    ] ) ] in
          let c_as_sigs =
            match args_tags with
            | [ v1 ; v2 ] ->
                let v1_sig =
                  match v1 with
                  | Money    -> [ ( NotMoney , [ Money        ; Money    ] ) ] 
                  | NotMoney -> [ ( NotMoney , [ NotMoney     ; NotMoney ] ) ]
                  | NoInfo   -> [ ( NotMoney , [ NotMoney     ; NotMoney ] ) ;
                                  ( NotMoney , [ Money        ; Money    ] ) ]
                  | _        -> [ ( NotMoney , [ Inconsistent ; NoInfo   ] ) ] in
                let v2_sig =
                  match v2 with
                  | Money    -> [ ( NotMoney , [ Money    ; Money        ] ) ] 
                  | NotMoney -> [ ( NotMoney , [ NotMoney ; NotMoney     ] ) ]
                  | NoInfo   -> [ ( NotMoney , [ NotMoney ; NotMoney     ] ) ;
                                  ( NotMoney , [ Money    ; Money        ] ) ]
                  | _        -> [ ( NotMoney , [ NoInfo   ; Inconsistent ] ) ] in
                [ v1_sig ; v2_sig ]
            | _             ->
                (* Error *)
                [[ ( Inconsistent , List.map ~f:(fun _ -> Inconsistent) args_tags ) ]] in
          (c_r_sigs, c_as_sigs)
      | Builtin_add
      | Builtin_sub ->
          let c_r_sigs =
            match res_tag with
            | NotMoney -> [ ( NotMoney     , [ NotMoney ; NotMoney ] ) ]
            | Money    -> [ ( Money        , [ Money    ; Money    ] ) ]
            | NoInfo   -> [ ( NotMoney     , [ NotMoney ; NotMoney ] ) ;
                            ( Money        , [ Money    ; Money    ] ) ]
            | _        -> [ ( Inconsistent , [ NotMoney ; NotMoney ] ) ;
                            ( Inconsistent , [ Money    ; Money    ] ) ] in
          let c_as_sigs =
            match args_tags with
            | [ v1 ; v2 ] ->
                let v1_sig =
                  match v1 with
                  | Money    -> [ ( Money    , [ Money        ; Money    ] ) ] 
                  | NotMoney -> [ ( NotMoney , [ NotMoney     ; NotMoney ] ) ]
                  | NoInfo   -> [ ( NotMoney , [ NotMoney     ; NotMoney ] ) ;
                                  ( Money    , [ Money        ; Money    ] ) ]
                  | _        -> [ ( NotMoney , [ Inconsistent ; NotMoney ] ) ;
                                  ( Money    , [ Inconsistent ; Money    ] ) ] in
                let v2_sig =
                  match v2 with
                  | Money    -> [ ( Money    , [ Money    ; Money        ] ) ] 
                  | NotMoney -> [ ( NotMoney , [ NotMoney ; NotMoney     ] ) ]
                  | NoInfo   -> [ ( NotMoney , [ NotMoney ; NotMoney     ] ) ;
                                  ( Money    , [ Money    ; Money        ] ) ]
                  | _        -> [ ( NotMoney , [ NotMoney ; Inconsistent ] ) ;
                                  ( Money    , [ Money    ; Inconsistent ] ) ] in
                [ v1_sig ; v2_sig ]
            | _             ->
                (* Error *)
                [[ ( Inconsistent , List.map ~f:(fun _ -> Inconsistent) args_tags ) ]] in
          (c_r_sigs, c_as_sigs)
      | Builtin_mul ->
          let c_r_sigs =
            match res_tag with
            | NotMoney -> [ ( NotMoney     , [ NotMoney ; NotMoney ] ) ]
            | Money    -> [ ( Money        , [ NotMoney ; Money    ] ) ;
                            ( Money        , [ Money    ; NotMoney ] ) ]
            | NoInfo   -> [ ( NotMoney     , [ NotMoney ; NotMoney ] ) ;
                            ( Money        , [ NotMoney ; Money    ] ) ;
                            ( Money        , [ Money    ; NotMoney ] ) ]
            | _        -> [ ( Inconsistent , [ NotMoney ; NotMoney ] ) ;
                            ( Inconsistent , [ NotMoney ; Money    ] ) ;
                            ( Inconsistent , [ Money    ; NotMoney ] ) ] in
          let c_as_sigs =
            match args_tags with
            | [ v1 ; v2 ] ->
                let v1_sig =
                  match v1 with
                  | Money    -> [ ( Money    , [ Money        ; NotMoney ] ) ] 
                  | NotMoney -> [ ( NotMoney , [ NotMoney     ; NotMoney ] ) ;
                                  ( Money    , [ NotMoney     ; Money    ] ) ]
                  | NoInfo   -> [ ( NotMoney , [ NotMoney     ; NotMoney ] ) ;
                                  ( Money    , [ NotMoney     ; Money    ] ) ;
                                  ( Money    , [ Money        ; NotMoney ] ) ]
                  | _        -> [ ( Money    , [ Inconsistent ; Money    ] ) ;
                                  ( Money    , [ Inconsistent ; NotMoney ] ) ;
                                  ( NotMoney , [ Inconsistent ; NotMoney ] ) ] in
                let v2_sig =
                  match v2 with
                  | Money    -> [ ( Money    , [ NotMoney ; Money        ] ) ] 
                  | NotMoney -> [ ( NotMoney , [ NotMoney ; NotMoney     ] ) ;
                                  ( Money    , [ Money    ; NotMoney     ] ) ]
                  | NoInfo   -> [ ( NotMoney , [ NotMoney ; NotMoney     ] ) ;
                                  ( Money    , [ NotMoney ; Money        ] ) ;
                                  ( Money    , [ Money    ; NotMoney     ] ) ]
                  | _        -> [ ( Money    , [ Money    ; Inconsistent ] ) ;
                                  ( Money    , [ NotMoney ; Inconsistent ] ) ;
                                  ( NotMoney , [ NotMoney ; Inconsistent ] ) ] in
                [ v1_sig ; v2_sig ]
            | _             ->
                (* Error *)
                [[ ( Inconsistent , List.map ~f:(fun _ -> Inconsistent) args_tags ) ]] in
          (c_r_sigs, c_as_sigs)
      | Builtin_pow ->
          let c_r_sigs =
            match res_tag with
            | NotMoney
            | NoInfo   -> [ ( NotMoney     , [ NotMoney ; NotMoney ] ) ]
            | _        -> [ ( Inconsistent , [ NotMoney ; NotMoney ] ) ] in
          let c_as_sigs =
            match args_tags with
            | [ v1 ; v2 ] ->
                let v1_sig =
                  match v1 with
                  | NotMoney
                  | NoInfo   -> [ ( NotMoney , [ NotMoney     ; NotMoney ] ) ]
                  | _        -> [ ( NotMoney , [ Inconsistent ; NotMoney ] ) ] in
                let v2_sig =
                  match v2 with
                  | NotMoney
                  | NoInfo   -> [ ( NotMoney , [ NotMoney ; NotMoney     ] ) ]
                  | _        -> [ ( NotMoney , [ NotMoney ; Inconsistent ] ) ] in
                [ v1_sig ; v2_sig ]
            | _             ->
                (* Error *)
                [[ ( Inconsistent , List.map ~f:(fun _ -> Inconsistent) args_tags ) ]] in
          (c_r_sigs, c_as_sigs)          
      | Builtin_div
      | Builtin_rem ->
          let c_r_sigs =
            match res_tag with
            | NotMoney -> [ ( NotMoney     , [ NotMoney ; NotMoney ] ) ]
            | Money    -> [ ( Money        , [ Money    ; NotMoney ] ) ]
            | NoInfo   -> [ ( NotMoney     , [ NotMoney ; NotMoney ] ) ;
                            ( Money        , [ Money    ; NotMoney ] ) ]
            | _        -> [ ( Inconsistent , [ NotMoney ; NotMoney ] ) ;
                            ( Inconsistent , [ Money    ; NotMoney ] ) ] in
          let c_as_sigs =
            match args_tags with
            | [ v1 ; v2 ] ->
                let v1_sig =
                  match v1 with
                  | Money    -> [ ( Money    , [ Money        ; NotMoney ] ) ] 
                  | NotMoney -> [ ( NotMoney , [ NotMoney     ; NotMoney ] ) ]
                  | NoInfo   -> [ ( NotMoney , [ NotMoney     ; NotMoney ] ) ;
                                  ( Money    , [ Money        ; NotMoney ] ) ]
                  | _        -> [ ( Money    , [ Inconsistent ; NotMoney ] ) ;
                                  ( NotMoney , [ Inconsistent ; NotMoney ] ) ] in
                let v2_sig =
                  match v2 with
                  | NotMoney
                  | NoInfo   -> [ ( NotMoney , [ NotMoney ; NotMoney     ] ) ;
                                  ( Money    , [ Money    ; NotMoney     ] ) ]
                  | _        -> [ ( Money    , [ Money    ; Inconsistent ] ) ;
                                  ( NotMoney , [ NotMoney ; Inconsistent ] ) ] in
                [ v1_sig ; v2_sig ]
            | _             ->
                (* Error *)
                [[ ( Inconsistent , List.map ~f:(fun _ -> Inconsistent) args_tags ) ]] in
          (c_r_sigs, c_as_sigs)
      | Builtin_to_int32
      | Builtin_to_int64
      | Builtin_to_int128
      | Builtin_to_int256
      | Builtin_to_string
      | Builtin_to_nat    ->
          let c_r_sigs =
            match res_tag with
            | NotMoney -> [ ( NotMoney     , [ NotMoney ] ) ]
            | Money    -> [ ( Money        , [ Money    ] ) ]
            | NoInfo   -> [ ( NotMoney     , [ NotMoney ] ) ;
                            ( Money        , [ Money    ] ) ]
            | _        -> [ ( Inconsistent , [ NotMoney ] ) ;
                            ( Inconsistent , [ Money    ] ) ] in
          let c_as_sigs =
            match args_tags with
            | [ v1 ] ->
                let v1_sig =
                  match v1 with
                  | Money    -> [ ( Money    , [ Money        ] ) ] 
                  | NotMoney -> [ ( NotMoney , [ NotMoney     ] ) ]
                  | NoInfo   -> [ ( NotMoney , [ NotMoney     ] ) ;
                                  ( Money    , [ Money        ] ) ]
                  | _        -> [ ( Money    , [ Inconsistent ] ) ;
                                  ( NotMoney , [ Inconsistent ] ) ] in
                [ v1_sig ]
            | _             ->
                (* Error *)
                [[ ( Inconsistent , List.map ~f:(fun _ -> Inconsistent) args_tags ) ]] in
          (c_r_sigs, c_as_sigs)
      | Builtin_sha256hash
      | Builtin_keccak256hash
      | Builtin_ripemd160hash
      | Builtin_schnorr_get_address
      | Builtin_strlen
      | Builtin_to_bystr ->
          let c_r_sigs =
            match res_tag with
            | NotMoney
            | NoInfo   -> [ ( NotMoney     , [ NotMoney ] ) ]
            | _        -> [ ( Inconsistent , [ NotMoney ] ) ] in
          let c_as_sigs = 
            match args_tags with
            | [ v1 ] ->
                let v1_sig =
                  match v1 with
                  | NotMoney
                  | NoInfo   -> [ ( NotMoney , [ NotMoney     ] ) ]
                  | _        -> [ ( NotMoney , [ Inconsistent ] ) ] in
                [ v1_sig ]
            | _             ->
                (* Error *)
                [[ ( Inconsistent , List.map ~f:(fun _ -> Inconsistent) args_tags ) ]] in
          (c_r_sigs, c_as_sigs)
      | Builtin_concat
      | Builtin_bech32_to_bystr20
      | Builtin_bystr20_to_bech32
      | Builtin_blt
      | Builtin_badd
      | Builtin_bsub ->
          let c_r_sigs =
            match res_tag with
            | NotMoney
            | NoInfo   -> [ ( NotMoney     , [ NotMoney ; NotMoney ] ) ]
            | _        -> [ ( Inconsistent , [ NotMoney ; NotMoney ] ) ] in
          let c_as_sigs =
            match args_tags with
            | [ v1 ; v2 ] ->
                let v1_sig =
                  match v1 with
                  | NotMoney
                  | NoInfo   -> [ ( NotMoney , [ NotMoney     ; NotMoney ] ) ]
                  | _        -> [ ( NotMoney , [ Inconsistent ; NotMoney ] ) ] in
                let v2_sig =
                  match v2 with
                  | NotMoney
                  | NoInfo   -> [ ( NotMoney , [ NotMoney ; NotMoney     ] ) ]
                  | _        -> [ ( NotMoney , [ NotMoney ; Inconsistent ] ) ] in
                [ v1_sig ; v2_sig ]
            | _             ->
                (* Error *)
                [[ ( Inconsistent , List.map ~f:(fun _ -> Inconsistent) args_tags ) ]] in
          (c_r_sigs, c_as_sigs)
      | Builtin_substr
      | Builtin_schnorr_verify ->
          let c_r_sigs =
            match res_tag with
            | NotMoney
            | NoInfo   -> [ ( NotMoney     , [ NotMoney ; NotMoney ; NotMoney ] ) ]
            | _        -> [ ( Inconsistent , [ NotMoney ; NotMoney ; NotMoney ] ) ] in
          let c_as_sigs =
            match args_tags with
            | [ v1 ; v2 ; v3 ] ->
                let v1_sig =
                  match v1 with
                  | NotMoney
                  | NoInfo   -> [ ( NotMoney , [ NotMoney     ; NotMoney ; NotMoney ] ) ]
                  | _        -> [ ( NotMoney , [ Inconsistent ; NotMoney ; NotMoney ] ) ] in
                let v2_sig =
                  match v2 with
                  | NotMoney
                  | NoInfo   -> [ ( NotMoney , [ NotMoney ; NotMoney     ; NotMoney ] ) ]
                  | _        -> [ ( NotMoney , [ NotMoney ; Inconsistent ; NotMoney ] ) ] in
                let v3_sig =
                  match v3 with
                  | NotMoney
                  | NoInfo   -> [ ( NotMoney , [ NotMoney ; NotMoney ; NotMoney     ] ) ]
                  | _        -> [ ( NotMoney , [ NotMoney ; NotMoney ; Inconsistent ] ) ] in
                [ v1_sig ; v2_sig ; v3_sig ]
            | _             ->
                (* Error *)
                [[ ( Inconsistent , List.map ~f:(fun _ -> Inconsistent) args_tags ) ]] in
          (c_r_sigs, c_as_sigs)
      | _ -> 
          (* Error *)
          let c_r_sigs =
            [ ( Inconsistent , List.map ~f:(fun _ -> Inconsistent) args_tags ) ] in
          let c_as_sigs =
            [ [ ( Inconsistent , List.map ~f:(fun _ -> Inconsistent) args_tags ) ] ] in
          (c_r_sigs, c_as_sigs) in
    lub_sigs c_r c_as

  (*******************************************************)
  (*            Helper functions for patterns            *)
  (*******************************************************)

  let rec get_pattern_vars acc p =
    match p with
    | Wildcard -> acc
    | Binder x -> x :: acc
    | Constructor (_, ps) ->
        List.fold_left ps ~init:acc ~f:get_pattern_vars

  let update_pattern_vars_tags_from_usage p local_env ctr_tag_map =
    let rec walk p ctr_tag_map =
      match p with
      | Wildcard -> (Wildcard, NoInfo, ctr_tag_map, false)
      | Binder x ->
          let new_x_tag = lookup_var_tag x local_env in
          let new_x = update_id_tag x new_x_tag in
          (Binder new_x, new_x_tag, ctr_tag_map, get_id_tag x <> get_id_tag new_x)
      | Constructor (s, ps) ->
          let (new_ps, new_ps_tags, ps_ctr_tag_map, ps_changes) =
            List.fold_right ps ~init:([], [], ctr_tag_map, false)
              ~f:(fun p (acc_ps, acc_ps_tags, acc_ctr_tag_map, acc_changes) ->
                 let (new_p, new_p_tag, p_ctr_tag_map, p_changes) = walk p acc_ctr_tag_map in
                 (new_p :: acc_ps, new_p_tag :: acc_ps_tags, p_ctr_tag_map, acc_changes || p_changes)) in
          let (new_ctr_tag_map, ctr_tag_map_changes) =
            update_ctr_tag_map ps_ctr_tag_map s new_ps_tags |>
            Option.value_map ~default:(ps_ctr_tag_map, false) ~f:(fun map -> (map, true)) in
          let ctr_tag = ctr_to_adt_tag s new_ps_tags in
          (Constructor (s, new_ps), ctr_tag, new_ctr_tag_map, ctr_tag_map_changes || ps_changes) in
    let (new_p, _, new_ctr_tag_map, changes) = walk p ctr_tag_map in
    (new_p, new_ctr_tag_map, changes)

  let update_pattern_vars_tags_from_scrutinee p scrutinee_tag =
    let rec walk p expected_tag =
      match p with
      | Wildcard -> (Wildcard, false)
      | Binder x ->
          let new_x_tag = lub_tags expected_tag (get_id_tag x) in
          let new_x = update_id_tag x new_x_tag in
          (Binder new_x, new_x_tag <> (get_id_tag x))
      | Constructor (s, ps) ->
          let expected_subtags =
            match ctr_pattern_to_subtags s expected_tag with
            | Some ts -> ts
            | None -> List.map ps ~f:(fun _ -> NoInfo) in
          let new_subpatterns_with_tags =
            List.zip ps expected_subtags |> Option.value ~default:[] in
          let (new_ps, changes) =
            List.fold_right new_subpatterns_with_tags
              ~init:([], false)
              ~f:(fun (p, exp_tag) (acc_ps, acc_changes) ->
                  let (new_p, change) = walk p exp_tag in
                  (new_p :: acc_ps, acc_changes || change)) in
          (Constructor (s, new_ps), changes) in
    walk p scrutinee_tag
              
  let insert_pattern_vars_into_env p local_env =
    let pattern_vars = get_pattern_vars [] p in
    List.fold_left pattern_vars ~init:local_env
      ~f:(fun l_env x ->
         AssocDictionary.insert (get_id x) (get_id_tag x) l_env)
      
  let remove_pattern_vars_from_env p local_env =
    let pattern_vars = get_pattern_vars [] p in
    List.fold_left pattern_vars ~init:local_env
      ~f:(fun l_env x -> AssocDictionary.remove (get_id x) l_env)

  (* Find least upper bound of scrutinee based on patterns and pattern tags *)
  let lub_pattern_tags ps =
    let rec walk acc_tag p =
      match p with
      | Wildcard -> acc_tag
      | Binder x -> lub_tags (get_id_tag x) acc_tag
      | Constructor (s, ps) ->
          let expected_subtags =
            match ctr_pattern_to_subtags s acc_tag with
            | Some ts -> ts
            | None -> List.map ps ~f:(fun _ -> NoInfo) in
          let subpattern_tags =
            match List.map2 expected_subtags ps ~f:walk with
            | Ok tps -> tps
            | Unequal_lengths -> [] in
          let new_tag = ctr_to_adt_tag s subpattern_tags in
          lub_tags new_tag acc_tag in
    List.fold_left ps ~init:NoInfo ~f:walk
            
  (*******************************************************)
  (*            Main cashflow analyzer                   *)
  (*******************************************************)

  let update_var_tag_payload p local_env =
    match p with
    | MLit l -> CFSyntax.MLit l
    | MVar v ->
        let tag =
          match AssocDictionary.lookup (get_id v) local_env with
          | None -> Inconsistent (* Should not happen *)
          | Some t -> t in
        match v with
        | Ident (name, (_, rep)) -> CFSyntax.MVar (Ident (name, (tag, rep)))

  let rec cf_tag_expr erep expected_tag field_env local_env ctr_tag_map =
    let lub t = lub_tags expected_tag t in
    let (e, (tag, rep)) = erep in
    let (new_e, new_e_tag, new_field_env, new_local_env, new_ctr_tag_map, new_changes) = 
      match e with
      | Literal _ ->
          (* No need to deduce tag from type. 
             If the literal is a number, then nothing can be deduced, 
             and if it is not a number, the tag of relevant variables 
             will be deduced from their usage. *)
          (e, tag, field_env, local_env, ctr_tag_map, false)
      | Var i ->
          let new_i_tag = lub (lookup_var_tag2 i local_env field_env) in
          let new_i = update_id_tag i new_i_tag in
          let (new_local_env, new_field_env) = update_var_tag2 i new_i_tag local_env field_env in
          (Var new_i, new_i_tag, new_field_env, new_local_env, ctr_tag_map, new_i_tag <> (get_id_tag i))
      | Fun (arg, t, body) ->
          (* Using Map tag to represent functions as well as maps *)
          let body_expected_tag =
            match expected_tag with
            | Map x -> x
            | NoInfo -> NoInfo
            | _     -> Inconsistent in
          let body_local_env =
            AssocDictionary.insert (get_id arg) (get_id_tag arg) local_env in
          let ((_, (new_body_tag, _)) as new_body, res_field_env, res_body_local_env, res_ctr_tag_map, body_changes) =
            cf_tag_expr body body_expected_tag field_env body_local_env ctr_tag_map in
          let res_arg_tag = lookup_var_tag arg res_body_local_env in
          let new_local_env = AssocDictionary.remove (get_id arg) res_body_local_env in
          (Fun (update_id_tag arg res_arg_tag, t, new_body),
           Map new_body_tag,
           res_field_env,
           new_local_env,
           res_ctr_tag_map,
           body_changes || (get_id_tag arg <> res_arg_tag))
      | App (f, args) ->
          let new_args = List.map ~f:(fun arg -> update_id_tag arg (lookup_var_tag2 arg local_env field_env)) args in
          let args_changes =
            match List.exists2 ~f:(fun arg new_arg -> (get_id_tag arg) <> (get_id_tag new_arg)) args new_args with
            | Ok res          -> res
            | Unequal_lengths -> false
          in
          let f_tag = lub_tags (lookup_var_tag2 f local_env field_env) (Map expected_tag) in
          let new_f = update_id_tag f f_tag in
          let (new_local_env, new_field_env) = update_var_tag2 f f_tag local_env field_env in
          let new_e_tag = 
            match f_tag with
            | Map t -> t
            | NoInfo -> NoInfo
            | _     -> Inconsistent in
          (App (new_f, new_args),
           new_e_tag,
           new_field_env,
           new_local_env,
           ctr_tag_map,
           args_changes || f_tag <> get_id_tag f)
      | Builtin (f, args) ->
          let args_tags = List.map ~f:(fun arg -> lookup_var_tag2 arg local_env field_env) args in
          let (res_tag, args_tags_usage) = builtin_signature f expected_tag args_tags in
          let (final_args, final_field_env, final_local_env, changes) =
            let tags_list =
              match List.zip args args_tags_usage with
              | None -> []
              | Some res -> res in
            List.fold_right tags_list ~init:([], field_env, local_env, false) 
              ~f:(fun (arg, arg_tag) (acc_args, acc_field_env, acc_local_env, acc_changes) ->
                 let (new_local_env, new_field_env) =
                   update_var_tag2 arg arg_tag acc_local_env acc_field_env in
                 ((update_id_tag arg arg_tag) :: acc_args,
                  new_field_env,
                  new_local_env,
                  acc_changes || (get_id_tag arg) <> arg_tag))
              in
          let f_tag = lub_tags (Map res_tag) (Map expected_tag) in

          let (op, (tag, r)) = f in
          let new_f = (op, (f_tag, r)) in

          (Builtin (new_f, final_args),
           res_tag,
           final_field_env,
           final_local_env,
           ctr_tag_map,
           changes || f_tag <> tag)
      | Let (i, topt, lhs, rhs) ->
          let ((_, (new_lhs_tag, _)) as new_lhs, lhs_field_env, lhs_local_env, lhs_ctr_tag_map, lhs_changes) =
            cf_tag_expr lhs (get_id_tag i) field_env local_env ctr_tag_map in
          let updated_lhs_local_env = AssocDictionary.insert (get_id i) new_lhs_tag lhs_local_env in
          let ((_, (new_rhs_tag, _)) as new_rhs, rhs_field_env, rhs_local_env, rhs_ctr_tag_map, rhs_changes) =
            cf_tag_expr rhs expected_tag lhs_field_env updated_lhs_local_env lhs_ctr_tag_map in
          let new_i_tag = lookup_var_tag i rhs_local_env in
          let new_i = update_id_tag i new_i_tag in
          let res_local_env = AssocDictionary.remove (get_id i) rhs_local_env in
          (Let (new_i, topt, new_lhs, new_rhs),
           new_rhs_tag,
           rhs_field_env,
           res_local_env,
           rhs_ctr_tag_map,
           lhs_changes || rhs_changes || new_i_tag <> get_id_tag i)
      | Constr (cname, ts, args) ->
          let new_args = List.map ~f:(fun arg -> update_id_tag arg (lookup_var_tag2 arg local_env field_env)) args in
          let args_changes =
            match List.exists2 ~f:(fun arg new_arg -> (get_id_tag arg) <> (get_id_tag new_arg)) args new_args with
            | Ok res -> res
            | Unequal_lengths -> false
          in
          let (new_ctr_tag_map, ctr_tag_map_changes) = 
            update_ctr_tag_map ctr_tag_map cname (List.map new_args ~f:get_id_tag) |>
            Option.value_map ~default:(ctr_tag_map, false) ~f:(fun map -> (map, true)) in
          let tag = ctr_to_adt_tag cname (List.map new_args ~f:get_id_tag) in
          (Constr (cname, ts, new_args),
           tag,
           field_env,
           local_env,
           new_ctr_tag_map,
           ctr_tag_map_changes || args_changes)
      | MatchExpr (x, clauses) ->
          let (res_clauses, res_tag, new_field_env, new_local_env, new_ctr_tag_map, res_clause_changes) =
            List.fold_right clauses
              ~init:([], expected_tag, field_env, local_env, ctr_tag_map, false) 
              ~f:(fun (p, ep) (acc_clauses, acc_res_tag, acc_field_env, acc_local_env, acc_ctr_tag_map, acc_changes) ->
                 let sub_local_env =
                   insert_pattern_vars_into_env p acc_local_env in
                 let ((_, (new_e_tag, _)) as new_e, new_field_env, new_local_env, e_ctr_tag_map, new_changes) =
                   cf_tag_expr ep expected_tag acc_field_env sub_local_env acc_ctr_tag_map in
                 let (new_p, new_ctr_tag_map, p_changes) = update_pattern_vars_tags_from_usage p new_local_env e_ctr_tag_map in
                 let res_local_env = remove_pattern_vars_from_env p new_local_env in
                 ((new_p, new_e) :: acc_clauses,
                  lub_tags acc_res_tag new_e_tag,
                  new_field_env,
                  res_local_env,
                  new_ctr_tag_map,
                  acc_changes || new_changes || p_changes)) in
          let (x_res_clauses, clause_changes) =
            List.fold_right res_clauses
              ~init:([], res_clause_changes)
              ~f:(fun (p, e) (acc_ps, acc_changes) ->
                  let (new_p, p_change) =
                    update_pattern_vars_tags_from_scrutinee p (get_id_tag x) in
                  ((new_p, e) :: acc_ps, p_change || acc_changes)) in
          let x_tag_usage = lub_pattern_tags (List.map ~f:(fun (p, _) -> p) x_res_clauses) in
          let new_x_tag = lub_tags (lookup_var_tag x local_env) x_tag_usage in
          let new_x = update_id_tag x new_x_tag in
          let res_local_env = AssocDictionary.update (get_id x) new_x_tag new_local_env in
          (MatchExpr (new_x, x_res_clauses),
           res_tag,
           new_field_env,
           res_local_env,
           new_ctr_tag_map,
           clause_changes || (get_id_tag x) <> new_x_tag)
      | Fixpoint (_f, _t, _body) ->
          (* Library functions not handled. *)
          (e, Inconsistent, field_env, local_env, ctr_tag_map, false)
      | TFun (tvar, body) ->
          (* Ignore polymorphism. *)
          let ((_, (new_body_tag, _)) as new_body, new_field_env, new_local_env, new_ctr_tag_map, changes) =
            cf_tag_expr body expected_tag field_env local_env ctr_tag_map in
          (TFun (tvar, new_body), new_body_tag, new_field_env, new_local_env, new_ctr_tag_map, changes)
      | TApp (tf, arg_types) ->
          let new_tf_tag = lookup_var_tag2 tf local_env field_env in
          let new_tf = update_id_tag tf new_tf_tag in
          (TApp (new_tf, arg_types), new_tf_tag, field_env, local_env, ctr_tag_map, false)
      | Message bs ->
          (* Find initializers and update env as appropriate *)
          let (new_bs, new_field_env, new_local_env, changes) =
            List.fold_right bs
              ~init:([], field_env, local_env, false) 
              ~f:(fun (s, p) (acc_bs, acc_field_env, acc_local_env, acc_changes) ->
                 match p with
                 | MLit _ -> ((s, p) :: acc_bs, acc_field_env, acc_local_env, acc_changes)
                 | MVar x ->
                     let usage_tag =
                       match s with
                       | "_amount" -> Money
                       | "_tag"
                       | "_recipient" -> NotMoney
                       | _ -> NoInfo in
                     let old_env_tag = lookup_var_tag2 x acc_local_env acc_field_env in
                     let new_x_tag = lub_tags usage_tag old_env_tag in
                     let new_x = update_id_tag x new_x_tag in
                     let (new_local_env, new_field_env) = update_var_tag2 x new_x_tag acc_local_env acc_field_env in
                     ((s, MVar new_x) :: acc_bs, new_field_env, new_local_env, acc_changes || get_id_tag x <> new_x_tag))
              in
          (Message new_bs,
           NotMoney,
           new_field_env,
           new_local_env,
           ctr_tag_map,
           changes) in
    let e_tag = lub new_e_tag in
    ((new_e, (e_tag, rep)), new_field_env, new_local_env, new_ctr_tag_map, new_changes || tag <> e_tag)

  (* Helper function for Load and Store - ensure field and local have same tag *)
  let cf_update_tag_for_field_assignment f x field_env local_env =
    let x_tag = lookup_var_tag x local_env in
    let f_tag = lookup_var_tag f field_env in
    let new_tag = lub_tags x_tag f_tag in
    let new_x = update_id_tag x new_tag in
    let new_f = update_id_tag f new_tag in
    let new_field_env = AssocDictionary.update (get_id f) new_tag field_env in
    let new_local_env = AssocDictionary.update (get_id x) new_tag local_env in
    (new_f, new_x, new_field_env, new_local_env)

  let rec cf_tag_stmt (srep : CFSyntax.stmt_annot) field_env local_env ctr_tag_map =
    let (s, rep) = srep in
    let (new_s, new_field_env, new_local_env, new_ctr_tag_map, changes) =
      match s with
      | Load (x, f) ->
          let (new_f, new_x, new_field_env, tmp_local_env) =
            cf_update_tag_for_field_assignment f x field_env local_env in
          (* x is no longer in scope, so remove from local_env *)
          let new_local_env = AssocDictionary.remove (get_id x) tmp_local_env in
          (Load (new_x, new_f),
           new_field_env,
           new_local_env,
           ctr_tag_map,
           (get_id_tag new_x) <> (get_id_tag x) || (get_id_tag new_f) <> (get_id_tag f))
      | Store (f, x) ->
          let (new_f, new_x, new_field_env, new_local_env) =
            cf_update_tag_for_field_assignment f x field_env local_env in
          (Store (new_f, new_x),
           new_field_env,
           new_local_env,
           ctr_tag_map,
           (get_id_tag new_x) <> (get_id_tag x) || (get_id_tag new_f) <> (get_id_tag f))
      | Bind (x, e) ->
          let x_tag = lookup_var_tag x local_env in
          let e_local_env = AssocDictionary.remove (get_id x) local_env in
          let ((_, (new_e_tag, _)) as new_e, new_field_env, new_local_env, new_ctr_tag_map, e_changes) =
            cf_tag_expr e x_tag field_env e_local_env ctr_tag_map in
          let new_x_tag = lub_tags x_tag new_e_tag in
          let new_x = update_id_tag x new_x_tag in
          (Bind (new_x, new_e),
           new_field_env,
           new_local_env,
           new_ctr_tag_map,
           e_changes || (get_id_tag x) <> new_x_tag)
      | MapUpdate (m, ks, v_opt) ->
          let v_tag =
            match v_opt with
            | None -> NoInfo
            | Some v -> lookup_var_tag v local_env in
          let m_tag_usage = List.fold_left ks ~init:v_tag ~f:(fun acc _ -> Map acc) in
          let m_tag = lub_tags m_tag_usage (lookup_var_tag m field_env) in
          let new_m = update_id_tag m m_tag in
          let new_field_env = AssocDictionary.update (get_id m) m_tag field_env in
          let new_ks = update_ids_tags ks local_env in
          let (new_v_opt, new_local_env) =
            match v_opt with
            | None -> (None, local_env)
            | Some v -> 
                let v_tag_usage =
                  List.fold_left ks ~init:m_tag
                    ~f:(fun acc_tag _ ->
                       match acc_tag with
                       | Map t -> t
                       | _ -> Inconsistent) in
                let new_v_tag = lub_tags v_tag_usage v_tag in
                let new_v = update_id_tag v new_v_tag in
                let new_local_env =
                  AssocDictionary.update (get_id v) new_v_tag local_env in
                (Some new_v, new_local_env) in
          (MapUpdate (new_m, new_ks, new_v_opt),
           new_field_env,
           new_local_env,
           ctr_tag_map,
           (get_id_tag m) <> m_tag || new_v_opt <> v_opt || new_ks <> ks)
      | MapGet (x, m, ks, fetch) ->
          let x_tag = lookup_var_tag x local_env in
          let val_tag = 
            if fetch
            then
              match x_tag with
              | Adt ("Option", [t]) -> t
              | NoInfo -> NoInfo
              | _ -> Inconsistent
            else
              NoInfo in
          let m_tag_usage =
            List.fold_left ks ~init:val_tag ~f:(fun acc _ -> Map acc) in
          let m_tag = lub_tags m_tag_usage (lookup_var_tag m field_env) in
          let new_m = update_id_tag m m_tag in
          let new_field_env = AssocDictionary.update (get_id m) m_tag field_env in
          let new_local_env = AssocDictionary.remove (get_id x) local_env in
          let new_ks = update_ids_tags ks new_local_env in
          let v_tag_usage =
            List.fold_left ks ~init:m_tag
              ~f:(fun acc_tag _ ->
                  match acc_tag with
                  | Map t -> t
                  | _ -> Inconsistent) in
          let new_val_tag = lub_tags val_tag v_tag_usage in
          let new_x_tag =
            if fetch
            then
              lub_tags x_tag (Adt ("Option", [new_val_tag]))
            else
              NotMoney (* Bool *) in
          let new_x = update_id_tag x new_x_tag in
          (MapGet (new_x, new_m, new_ks, fetch),
           new_field_env,
           new_local_env,
           ctr_tag_map,
           (get_id_tag x) <> new_x_tag || (get_id_tag m) <> m_tag || new_ks <> ks)
      | MatchStmt (x, clauses) -> 
          let (res_clauses, new_field_env, new_local_env, new_ctr_tag_map, res_clause_changes) =
            List.fold_right clauses
              ~init:([], field_env, local_env, ctr_tag_map, false) 
              ~f:(fun (p, sp) (acc_clauses, acc_field_env, acc_local_env, acc_ctr_tag_map, acc_changes) ->
                 let sub_local_env =
                   insert_pattern_vars_into_env p acc_local_env in
                  let (new_stmts, new_field_env, s_local_env, s_ctr_tag_map, s_changes) =
                   cf_tag_stmts sp acc_field_env sub_local_env acc_ctr_tag_map in
                 let (new_p, new_ctr_tag_map, p_changes) = update_pattern_vars_tags_from_usage p s_local_env s_ctr_tag_map in
                 let new_local_env = remove_pattern_vars_from_env p s_local_env in
                 ((new_p, new_stmts) :: acc_clauses,
                  new_field_env,
                  new_local_env,
                  new_ctr_tag_map,
                  acc_changes || s_changes || p_changes))
              in
          let (x_res_clauses, clause_changes) =
            List.fold_right res_clauses
              ~init:([], res_clause_changes)
              ~f:(fun (p, sp) (acc_ps, acc_changes) ->
                  let (new_p, p_change) =
                    update_pattern_vars_tags_from_scrutinee p (get_id_tag x) in
                  ((new_p, sp) :: acc_ps, p_change || acc_changes)) in
          let x_tag_usage = lub_pattern_tags (List.map ~f:(fun (p, _) -> p) res_clauses) in
          let new_x_tag = lub_tags (lookup_var_tag x local_env) x_tag_usage in
          let new_x = update_id_tag x new_x_tag in
          let res_local_env = AssocDictionary.update (get_id x) new_x_tag new_local_env in
          (MatchStmt (new_x, x_res_clauses),
           new_field_env,
           res_local_env,
           new_ctr_tag_map,
           clause_changes || (get_id_tag x) <> new_x_tag)
      | ReadFromBC (x, s) ->
          let x_tag = lub_tags NotMoney (lookup_var_tag x local_env) in
          let new_x = update_id_tag x x_tag in
          let new_local_env = AssocDictionary.remove (get_id x) local_env in
          (ReadFromBC (new_x, s),
           field_env,
           new_local_env,
           ctr_tag_map,
           (get_id_tag x) <> x_tag)
      | AcceptPayment -> (AcceptPayment, field_env, local_env, ctr_tag_map, false)
      | SendMsgs m ->
          let m_tag = lub_tags NotMoney (lookup_var_tag m local_env) in
          let new_m = update_id_tag m m_tag in
          let new_local_env = AssocDictionary.update (get_id m) m_tag local_env in
          (SendMsgs new_m,
           field_env,
           new_local_env,
           ctr_tag_map,
           (get_id_tag m) <> m_tag)
      | CreateEvnt e ->
          let e_tag = lub_tags NotMoney (lookup_var_tag e local_env) in
          let new_e = update_id_tag e e_tag in
          let new_local_env = AssocDictionary.update (get_id e) e_tag local_env in
          (CreateEvnt new_e,
           field_env,
           new_local_env,
           ctr_tag_map,
           (get_id_tag e) <> e_tag)
      | CallProc (p, args) ->
          let new_args = List.map args ~f:(fun arg -> update_id_tag arg (lookup_var_tag2 arg local_env field_env)) in
          let args_changes =
            match List.exists2 ~f:(fun arg new_arg -> (get_id_tag arg) <> (get_id_tag new_arg)) args new_args with
            | Ok res          -> res
            | Unequal_lengths -> false
          in
          (CallProc (p, new_args),
           field_env,
           local_env,
           ctr_tag_map,
           args_changes)
      | Throw xopt ->
        (match xopt with
        | Some x->
          let x_tag = lub_tags NotMoney (lookup_var_tag x local_env) in
          let new_x = update_id_tag x x_tag in
          let new_local_env = AssocDictionary.update (get_id x) x_tag local_env in
          (Throw (Some new_x),
           field_env,
           new_local_env,
           ctr_tag_map,
           (get_id_tag x) <> x_tag)
        | None ->
          (Throw None, field_env, local_env, ctr_tag_map, false)
        )
      in
    ((new_s, rep), new_field_env, new_local_env, new_ctr_tag_map, changes)

    and cf_tag_stmts ss field_env local_env ctr_tag_map =
      let init_local_env =
        List.fold_left ss ~init:local_env
          ~f:(fun acc_env srep ->
             let (s, _) = srep in
             match s with
             | Load (x, _)
             | Bind (x, _)
             | MapGet (x, _, _, _)
             | ReadFromBC (x, _) ->
                 AssocDictionary.insert (get_id x) (get_id_tag x) acc_env
             | _ -> acc_env) in
      List.fold_right ss
        ~init:([], field_env, init_local_env, ctr_tag_map, false)
        ~f:(fun s (acc_ss, acc_field_env, acc_local_env, acc_ctr_tag_map, acc_changes) ->
           let (new_s, new_field_env, new_local_env, new_ctr_tag_map, new_changes) =
             cf_tag_stmt s acc_field_env acc_local_env acc_ctr_tag_map in
           (new_s :: acc_ss,
            new_field_env,
            new_local_env,
            new_ctr_tag_map,
            new_changes || acc_changes))
        
    let cf_tag_component t field_env ctr_tag_map =
      let { comp_type; comp_name ; comp_params ; comp_body } = t in
      let empty_local_env = AssocDictionary.make_dict() in
      let implicit_local_env =
        AssocDictionary.insert "_amount" Money 
          (AssocDictionary.insert "_sender" NotMoney
             (AssocDictionary.insert "_tag" NotMoney empty_local_env)) in
      let param_local_env =
        List.fold_left comp_params ~init:implicit_local_env
          ~f:(fun acc_env (p, _) ->
             AssocDictionary.insert (get_id p) (get_id_tag p) acc_env)
          in
      let (new_comp_body, new_field_env, new_local_env, new_ctr_tag_map, body_changes) =
        cf_tag_stmts comp_body field_env param_local_env ctr_tag_map in
      let (new_params, new_changes) =
        List.fold_right comp_params ~init:([], body_changes)
          ~f:(fun (p, typ) (acc_ps, acc_changes) ->
             let new_tag = lookup_var_tag p new_local_env in
             ((update_id_tag p new_tag, typ) :: acc_ps,
              acc_changes || (get_id_tag p) <> new_tag))
          in
      ({ comp_type = comp_type; comp_name = comp_name ; comp_params = new_params ; comp_body = new_comp_body },
       new_field_env,
       new_ctr_tag_map,
       new_changes)

    let cf_tag_contract c =
      let { cname ; cparams ; cfields ; ccomps } = c in
      let empty_field_env = AssocDictionary.make_dict () in
      let implicit_field_env = AssocDictionary.insert "_balance" Money empty_field_env in
      let ctr_tag_map = init_ctr_tag_map () in
      let param_field_env =
        List.fold_left cparams ~init:implicit_field_env
          ~f:(fun acc_env (p, _) ->
             AssocDictionary.insert (get_id p) (get_id_tag p) acc_env)
          in
      let init_field_env =
        List.fold_left cfields ~init:param_field_env
          ~f:(fun acc_env (f, _, e) ->
             let ((_, (e_tag, _)), _, _, _, _) =
                  cf_tag_expr e (lub_tags (get_id_tag f) NoInfo) (AssocDictionary.make_dict ()) (AssocDictionary.make_dict ()) ctr_tag_map in
             AssocDictionary.insert (get_id f) e_tag acc_env)
          in
      let rec tagger components field_env ctr_tag_map =
        let (new_ts, new_field_env, tmp_ctr_tag_map, ccomps_changes) =
          List.fold_right components ~init:([], field_env, ctr_tag_map, false) 
            ~f:(fun t (acc_ts, acc_field_env, acc_ctr_tag_map, acc_changes) ->
               let (new_t, new_field_env, new_ctr_tag_map, t_changes) =
                 cf_tag_component t acc_field_env acc_ctr_tag_map in
               (new_t :: acc_ts, new_field_env, new_ctr_tag_map, acc_changes || t_changes))
            in
        if ccomps_changes
        then
          tagger new_ts new_field_env tmp_ctr_tag_map
        else (new_ts, new_field_env, tmp_ctr_tag_map) in
      let (new_ccomps, new_field_env, final_ctr_tag_map) = tagger ccomps init_field_env ctr_tag_map in
      let new_fields =
        List.fold_right cfields ~init:[] 
          ~f:(fun (f, t, e) acc_fields ->
             let new_tag = lookup_var_tag f new_field_env in
             (update_id_tag f new_tag, t, e) :: acc_fields)
          in
      let new_params =
        List.fold_right cparams ~init:[] 
          ~f:(fun (p, t) acc_params ->
             let new_tag = lookup_var_tag p new_field_env in
             (update_id_tag p new_tag, t) :: acc_params)
          in
      ({ cname = cname ;
         cparams = new_params ;
         cfields = new_fields ;
         ccomps = new_ccomps },
       final_ctr_tag_map)

    let cf_tag_module m =
      let { smver; cname ; libs ; elibs ; contr } = m in
      let (new_contr, ctr_tag_map) = cf_tag_contract contr in
      ({ smver = smver;
         cname = cname ;
         libs = libs ;
         elibs = elibs ;
         contr = new_contr },
       ctr_tag_map)
    
  (*******************************************************)
  (*                Main entry function                  *)
  (*******************************************************)

  let main cmod token_fields =
    let init_mod = cf_init_tag_module cmod token_fields in
    let (new_mod, ctr_tag_map) = cf_tag_module init_mod in
    let param_field_tags = 
      (List.map ~f:(fun (p, _) -> (get_id p, get_id_tag p)) new_mod.contr.cparams)
      @
      (List.map ~f:(fun (f, _, _) -> (get_id f, get_id_tag f)) new_mod.contr.cfields) in
    let ctr_tags =
      let all_adts = DataTypeDictionary.get_all_adts () in
      List.filter_map all_adts
        ~f:(fun adt ->
            match List.filter_map adt.tconstr
                    ~f:(fun ctr ->
                        match List.Assoc.find ctr_tag_map ~equal:(=) ctr.cname with
                        | Some arg_tag_opts ->
                            Some (ctr.cname, arg_tag_opts)
                        | None -> None) with
            | [] ->
                (* No interesting constructors found *)
                None
            | x -> Some (adt.tname, x)) in
    (param_field_tags, ctr_tags)
end
