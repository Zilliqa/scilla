(*
  This file is part of scilla.

  Copyright (c) 2020 - present Zilliqa Research Pvt. Ltd.
  
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

open Core_kernel
open! Int.Replace_polymorphic_compare
open Result.Let_syntax
open ErrorUtils
open MonadUtil
open Identifier
open Literal
open Syntax

(*****************************************************************)
(*          Translate from local names to global names           *)
(*****************************************************************)

module ScillaDisambiguation (SR : Rep) (ER : Rep) = struct

  module PreDisLiteral = LocalLiteral
  module PostDisLiteral = GlobalLiteral
  module PreDisSyntax = ScillaSyntax (SR) (ER) (PreDisLiteral)
  module PostDisSyntax = ScillaSyntax (SR) (ER) (PostDisLiteral)
  module PreDisType = PreDisSyntax.SType
  module PreDisIdentifier = PreDisSyntax.SIdentifier
  open PreDisIdentifier
  open PreDisType
  open PreDisLiteral
  open PreDisSyntax
  
  let wrap_disambiguation_err e ?(opt = "") = wrap_err e "Disambiguation" ~opt

  let wrap_disambiguation_serr s ?(opt = "") = wrap_serr s "Disambiguation" ~opt

  (**************************************************************)
  (*                   Dictionary helpers                       *)
  (**************************************************************)

  (* Name dictionaries *)
  type str_str_dict = (string, string) List.Assoc.t
  type name_dicts = { ns_dict : str_str_dict ;
                      simp_var_dict : str_str_dict ;
                      simp_typ_dict : str_str_dict ;
                      simp_ctr_dict : str_str_dict }
  
  let remove_local_id_from_dict dict var = List.Assoc.remove dict
      ~equal:String.(=) var
    
  (**************************************************************)
  (*                   Disambiguate names                       *)
  (**************************************************************)

  let disambiguate_name ns_dict simpname_adr_dict nm =
    let open LocalName in
    match nm with
    | SimpleLocal n ->
        (match List.Assoc.find simpname_adr_dict n ~equal:String.(=) with
         | Some adr ->
             (* Imported simple name *)
             pure (GlobalName.QualifiedGlobal (adr, n), n)
         | None ->
             (* Locally defined or builtin name *)
             pure (GlobalName.SimpleGlobal n, n))
    | QualifiedLocal (ns, n) ->
        match List.Assoc.find ns_dict ns ~equal:String.(=) with
        | Some adr -> pure (GlobalName.QualifiedGlobal (adr, n), as_string nm)
        | None ->
            fail0 @@
            sprintf "Name qualifier %s is not a legal namespace" ns

  let local_id_as_global_name id =
    let open LocalName in
    let%bind dis_name = 
      match (get_id id) with
      | SimpleLocal n -> pure (GlobalName.SimpleGlobal n, n)
      | QualifiedLocal (ns, n) -> 
          fail0 @@
          sprintf "Illegal variable name: %s.%s" ns n
    in
    pure @@ PostDisSyntax.SIdentifier.mk_id dis_name (get_rep id)
  
  let disambiguate_identifier ns_dict simpname_adr_dict id =
    let%bind dis_name = disambiguate_name ns_dict simpname_adr_dict (get_id id) in
    pure @@ PostDisSyntax.SIdentifier.mk_id dis_name (get_rep id)
  
  (**************************************************************)
  (*                   Disambiguate types                       *)
  (**************************************************************)

  let rec disambiguate_type ns_dict simp_typ_dict t =
    let module PostDisType = PostDisSyntax.SType in
    let rec recurse t =
      match t with
      | PrimType pt -> pure @@ PostDisType.PrimType pt
      | MapType (kt, vt) ->
          let%bind dis_kt = recurse kt in
          let%bind dis_vt = recurse vt in
          pure @@ PostDisType.MapType (dis_kt, dis_vt)
      | FunType (arg_t, res_t) ->
          let%bind dis_arg_t = recurse arg_t in
          let%bind dis_res_t = recurse res_t in
          pure @@ PostDisType.FunType (dis_arg_t, dis_res_t)
      | ADT (t_name, targs) ->
          let%bind dis_t_name = disambiguate_identifier ns_dict simp_typ_dict t_name in
          let%bind dis_targs = mapM targs ~f:recurse in
          pure @@ PostDisType.ADT (dis_t_name, dis_targs)
      | TypeVar tvar ->
          pure @@ PostDisType.TypeVar tvar
      | PolyFun (tvar, t) ->
          let%bind dis_t = disambiguate_type ns_dict simp_typ_dict t in
          pure @@ PostDisType.PolyFun (tvar, dis_t)
      | Unit -> pure @@ PostDisType.Unit
    in
    recurse t
  
  (**************************************************************)
  (*                Disambiguate expressions                    *)
  (**************************************************************)

  let disambiguate_literal (dicts : name_dicts) l =
    let module ResLit = PostDisSyntax.SLiteral in
    let rec recurser l =
      match l with
      | StringLit s -> pure @@ ResLit.StringLit s
      | IntLit (Int32L i) -> pure @@ ResLit.IntLit (ResLit.Int32L i)
      | IntLit (Int64L i) -> pure @@ ResLit.IntLit (ResLit.Int64L i)
      | IntLit (Int128L i) -> pure @@ ResLit.IntLit (ResLit.Int128L i)
      | IntLit (Int256L i) -> pure @@ ResLit.IntLit (ResLit.Int256L i)
      | UintLit (Uint32L i) -> pure @@ ResLit.UintLit (ResLit.Uint32L i)
      | UintLit (Uint64L i) -> pure @@ ResLit.UintLit (ResLit.Uint64L i)
      | UintLit (Uint128L i) -> pure @@ ResLit.UintLit (ResLit.Uint128L i)
      | UintLit (Uint256L i) -> pure @@ ResLit.UintLit (ResLit.Uint256L i)
      | BNum s -> pure @@ ResLit.BNum s
      | ByStrX w ->
          let as_hex = Bystrx.hex_encoding w in
          pure @@ ResLit.ByStrX (ResLit.Bystrx.parse_hex as_hex)
      | ByStr w ->
          let as_hex = Bystr.hex_encoding w in
          pure @@ ResLit.ByStr (ResLit.Bystr.parse_hex as_hex)
      (* TODO: Msg and Map are needed to migrate existing jsons. They can be changed to fail once migration is done. *)
      | ADTValue (s, ts, ls) ->
          let%bind dis_s = disambiguate_name dicts.ns_dict dicts.simp_ctr_dict s in
          let%bind dis_ts = mapM ts
              ~f:(fun t -> disambiguate_type dicts.ns_dict dicts.simp_typ_dict t) in
          let%bind dis_ls = mapM ls ~f:recurser in
          pure @@ PostDisSyntax.SLiteral.ADTValue (dis_s, dis_ts, dis_ls)
      | Msg msg_entries ->
          let%bind res_msg_entries = foldrM msg_entries ~init:[]
              ~f:(fun acc (label, l) ->
                  let%bind res_l = recurser l in
                  pure @@ (label, res_l) :: acc) in
          pure @@ ResLit.Msg res_msg_entries
      | Map ((kt, vt), mentries) ->
          let open Sexplib.Std in (* Use Sexplib.Std hashtable *)
          let%bind res_kt = disambiguate_type dicts.ns_dict dicts.simp_typ_dict kt in
          let%bind res_vt = disambiguate_type dicts.ns_dict dicts.simp_typ_dict vt in
          let flat_table = Hashtbl.fold (fun k v acc -> (k, v) :: acc) mentries [] in
          let%bind res_flat_table = mapM flat_table
              ~f:(fun (k, v) ->
                  let%bind res_k = recurser k in
                  let%bind res_v = recurser v in
                  pure @@ (res_k, res_v)) in
          let res_tbl = Hashtbl.create (List.length res_flat_table) in
          let _ = List.iter res_flat_table ~f:(fun (k, v) -> Hashtbl.add res_tbl k v) in
          pure @@ ResLit.Map ((res_kt, res_vt), res_tbl)
      (* Closures and type abstractions should not appear in disambiguation phase *)
      | Clo _ ->
          raise (mk_internal_error "Closure literal found in disambiguation phase")
      | TAbs _ ->
          raise (mk_internal_error "Type abstraction literal found in disambiguation phase")
    in
    recurser l

  let disambiguate_pattern ns_dict simp_ctr_dict p =
    let rec recurser p =
      match p with
      | Wildcard ->
          pure (PostDisSyntax.Wildcard, [])
      | Binder x ->
          let%bind dis_x = local_id_as_global_name x in
          (* x is in scope as a local in rhs, so remove from var dictionary *)
          pure (PostDisSyntax.Binder dis_x, [x])
      | Constructor (ctr, ps) ->
          let%bind dis_ctr = disambiguate_identifier ns_dict simp_ctr_dict ctr in
          let%bind dis_ps, bounds = foldrM ps ~init:([], []) ~f:(fun (p_acc, bounds_acc) p' ->
              let%bind dis_p', bounds' = recurser p' in
              pure (dis_p' :: p_acc, bounds' @ bounds_acc)) in
          pure (PostDisSyntax.Constructor (dis_ctr, dis_ps), bounds)
    in
    recurser p

  let rec disambiguate_exp (dicts : name_dicts) erep =
    let disambiguate_identifier_helper simp_var_dict id =
      disambiguate_identifier dicts.ns_dict simp_var_dict id in
    let disambiguate_type_helper t = disambiguate_type dicts.ns_dict dicts.simp_typ_dict t in
    let disambiguate_literal_helper l = disambiguate_literal dicts l in
    let rec recurser simp_var_dict erep = 
      let e, rep = erep in
      let%bind new_e =
        match e with
        | Literal l ->
            let%bind dis_l = disambiguate_literal dicts l in
            pure @@ PostDisSyntax.Literal dis_l
        | Var id ->
            let%bind dis_id = disambiguate_identifier_helper dicts.simp_var_dict id in
            pure @@ PostDisSyntax.Var dis_id
        | Let (id, t, lhs, rhs) ->
            let%bind dis_id = local_id_as_global_name id in
            let%bind dis_t = option_mapM t ~f:disambiguate_type_helper in
            let%bind dis_lhs = recurser dicts.simp_var_dict lhs in
            (* id is in scope as a local in rhs, so remove from var dictionary *)
            let rhs_simp_var_dict = remove_local_id_from_dict dicts.simp_var_dict (as_string id) in
            let%bind dis_rhs = recurser rhs_simp_var_dict rhs in
            pure @@ PostDisSyntax.Let (dis_id, dis_t, dis_lhs, dis_rhs)
        | Message mentries ->
            let disambiguate_payload = function
              | MLit l ->
                  let%bind dis_l = disambiguate_literal_helper l in
                  pure @@ PostDisSyntax.MLit dis_l
              | MVar id ->
                  let%bind dis_id = disambiguate_identifier_helper dicts.simp_var_dict id in
                  pure @@ PostDisSyntax.MVar dis_id
            in
            let%bind dis_mentries = mapM mentries
                ~f:(fun (l, p) ->
                    let%bind dis_p = disambiguate_payload p in
                    pure @@ (l, dis_p)) in
            pure @@ PostDisSyntax.Message dis_mentries
        | Fun (id, t, body) ->
            let%bind dis_id = local_id_as_global_name id in
            let%bind dis_t = disambiguate_type_helper t in
            (* id is in scope as a local in body, so remove from var dictionary *)
            let body_simp_var_dict = remove_local_id_from_dict dicts.simp_var_dict (as_string id) in
            let%bind dis_body = recurser body_simp_var_dict body in
            pure @@ PostDisSyntax.Fun (dis_id, dis_t, dis_body)
        | App (f, args) ->
            let%bind dis_f = disambiguate_identifier_helper dicts.simp_var_dict f in
            let%bind dis_args = mapM args ~f:(disambiguate_identifier_helper dicts.simp_var_dict) in
            pure @@ PostDisSyntax.App (dis_f, dis_args)
        | Constr (c, ts, args) ->
            let%bind dis_c = disambiguate_identifier_helper dicts.simp_ctr_dict c in
            let%bind dis_ts_args = mapM ts ~f:disambiguate_type_helper in
            let%bind dis_args = mapM args ~f:(disambiguate_identifier_helper dicts.simp_var_dict) in
            pure @@ PostDisSyntax.Constr (dis_c, dis_ts_args, dis_args)
        | MatchExpr (x, pes) ->
            let%bind dis_x = disambiguate_identifier_helper dicts.simp_var_dict x in
            let%bind dis_pes = mapM pes ~f:(fun (p, erep') ->
                let%bind dis_p, bounds = disambiguate_pattern dicts.ns_dict dicts.simp_ctr_dict p in
                (* bounds are in scope as locals in e, so remove from var dictionary *)
                let erep'_simp_var_dict = List.fold bounds ~init:dicts.simp_var_dict
                    ~f:(fun simp_var_dict' x ->
                        remove_local_id_from_dict simp_var_dict' (as_string x)) in
                let%bind dis_erep' = recurser erep'_simp_var_dict erep' in
                pure (dis_p, dis_erep'))
            in
            pure @@ PostDisSyntax.MatchExpr (dis_x, dis_pes)
        | Builtin (b, args) ->
            let%bind dis_args = mapM args ~f:(disambiguate_identifier_helper dicts.simp_var_dict) in
            pure @@ PostDisSyntax.Builtin (b, dis_args)
        | TFun (tvar, body) ->
            let%bind dis_tvar = local_id_as_global_name tvar in
            (* tvar is in scope as a type, but won't affect disambiguation, 
               so don't worry about removing it from the environment *)
            let%bind dis_body = recurser dicts.simp_var_dict body in
            pure @@ PostDisSyntax.TFun (dis_tvar, dis_body)
        | TApp (f, targs) ->
            let%bind dis_f = disambiguate_identifier_helper dicts.simp_var_dict f in
            let%bind dis_targs = mapM targs ~f:disambiguate_type_helper in
            pure @@ PostDisSyntax.TApp (dis_f, dis_targs)
        | Fixpoint (f, t, body) ->
            let%bind dis_f = local_id_as_global_name f in
            let%bind dis_t = disambiguate_type_helper t in
            (* f is in scope as a local in body, so remove from var dictionary *)
            let body_simp_var_dict = remove_local_id_from_dict dicts.simp_var_dict (as_string f) in
            let%bind dis_body = recurser body_simp_var_dict body in
            pure @@ PostDisSyntax.Fixpoint (dis_f, dis_t, dis_body)
      in
      pure @@ (new_e, rep)
    in
    recurser dicts.simp_var_dict erep

  (**************************************************************)
  (*                Disambiguate statements                     *)
  (**************************************************************)

  let rec disambiguate_stmts (dicts : name_dicts) stmts =
    let disambiguate_identifier_helper simp_var_dict id =
      disambiguate_identifier dicts.ns_dict simp_var_dict id in
    let folder (simp_var_dict_acc, dis_stmts_acc_rev) srep = 
      let s, rep = srep in
      let%bind dis_s, new_simp_var_dict =
        match s with
        | Load (x, f) ->
            let%bind dis_x = local_id_as_global_name x in
            let%bind dis_f = disambiguate_identifier_helper simp_var_dict_acc f in
            (* x is now in scope as a local, so remove from var dictionary *)
            let new_simp_var_dict = remove_local_id_from_dict simp_var_dict_acc (as_string x) in
            pure @@ (PostDisSyntax.Load (dis_x, dis_f), new_simp_var_dict)
        | Store (f, x) ->
            let%bind dis_f = local_id_as_global_name f in
            let%bind dis_x = disambiguate_identifier_helper simp_var_dict_acc f in
            pure @@ (PostDisSyntax.Store (dis_f, dis_x), simp_var_dict_acc)
        | Bind (x, e') ->
            let%bind dis_x = local_id_as_global_name x in
            let%bind dis_e' = disambiguate_exp { dicts with simp_var_dict = simp_var_dict_acc } e' in
            (* x is now in scope as a local, so remove from var dictionary *)
            let new_simp_var_dict = remove_local_id_from_dict simp_var_dict_acc (as_string x) in
            pure @@ (PostDisSyntax.Bind (dis_x, dis_e'), new_simp_var_dict)
        | MapUpdate (m, ks, vopt) ->
            let%bind dis_m = local_id_as_global_name m in
            let%bind dis_ks = mapM ks ~f:(disambiguate_identifier_helper simp_var_dict_acc) in
            let%bind dis_vopt = option_mapM vopt ~f:(disambiguate_identifier_helper simp_var_dict_acc) in
            pure @@ (PostDisSyntax.MapUpdate (dis_m, dis_ks, dis_vopt), simp_var_dict_acc)
        | MapGet (x, m, ks, fetch) ->
            let%bind dis_x = local_id_as_global_name x in
            let%bind dis_m = local_id_as_global_name m in
            let%bind dis_ks = mapM ks ~f:(disambiguate_identifier_helper simp_var_dict_acc) in
            (* x is now in scope as a local, so remove from var dictionary *)
            let new_simp_var_dict = remove_local_id_from_dict simp_var_dict_acc (as_string x) in
            pure @@ (PostDisSyntax.MapGet (dis_x, dis_m, dis_ks, fetch), new_simp_var_dict)
        | MatchStmt (x, pss) ->
            let%bind dis_x = disambiguate_identifier_helper simp_var_dict_acc x in
            let%bind dis_pss = mapM pss ~f:(fun (p, ss) ->
                let%bind dis_p, bounds = disambiguate_pattern dicts.ns_dict dicts.simp_ctr_dict p in
                (* bounds are in scope as locals in s, so remove from var dictionary *)
                let ss_simp_var_dict = List.fold bounds ~init:simp_var_dict_acc
                    ~f:(fun simp_var_dict' x -> 
                        remove_local_id_from_dict simp_var_dict' (as_string x)) in
                let%bind dis_ss = disambiguate_stmts { dicts with simp_var_dict = ss_simp_var_dict } ss in
                pure (dis_p, dis_ss))
            in
            pure @@ (PostDisSyntax.MatchStmt (dis_x, dis_pss), simp_var_dict_acc)
        | ReadFromBC (x, f) ->
            let%bind dis_x = local_id_as_global_name x in
            pure @@ (PostDisSyntax.ReadFromBC (dis_x, f), simp_var_dict_acc)
        | AcceptPayment ->
            pure @@ (PostDisSyntax.AcceptPayment, simp_var_dict_acc)
        (* forall l p *)
        | Iterate (l, proc) ->
            let%bind dis_l = disambiguate_identifier_helper simp_var_dict_acc l in
            let%bind dis_proc = local_id_as_global_name proc in
            pure @@ (PostDisSyntax.Iterate (dis_l, dis_proc), simp_var_dict_acc)
        | SendMsgs msgs ->
            let%bind dis_msgs = disambiguate_identifier_helper simp_var_dict_acc msgs in
            pure @@ (PostDisSyntax.SendMsgs dis_msgs, simp_var_dict_acc)
        | CreateEvnt e ->
            let%bind dis_e = disambiguate_identifier_helper simp_var_dict_acc e in
            pure @@ (PostDisSyntax.CreateEvnt dis_e, simp_var_dict_acc)
        | CallProc (proc, args) ->
            let%bind dis_proc = local_id_as_global_name proc in
            let%bind dis_args = mapM args ~f:(disambiguate_identifier_helper simp_var_dict_acc) in
            pure @@ (PostDisSyntax.CallProc (dis_proc, dis_args), simp_var_dict_acc)
        | Throw xopt ->
            let%bind dis_xopt = option_mapM xopt ~f:(disambiguate_identifier_helper simp_var_dict_acc) in
            pure @@ (PostDisSyntax.Throw dis_xopt, simp_var_dict_acc)
      in
      pure @@ (new_simp_var_dict, (dis_s, rep) :: dis_stmts_acc_rev)
    in
    let%bind _, dis_stmts_rev = foldM stmts ~init:(dicts.simp_var_dict, []) ~f:folder in
    pure dis_stmts_rev
  
  (**************************************************************)
  (*                 Disambiguate libraries                     *)
  (**************************************************************)

  
    
(*  
  let disambiguate_library lib =
    let { lname; lentries } = lib in
    let disambiguate_lib_entry = function
      | LibVar (id, t, init) ->
          
        
    let dis_lentries = List.map lentries ~f:(fun 

  (**************************************************************)
  (*                  Disambiguate modules                      *)
  (**************************************************************)
  
  let disambiguate_module (md : PreDisSyntax.cmodule) :
    (PostDisSyntax.cmodule
     * PostDisSyntax.lib_entry list
     * PostDisSyntax.libtree list,
     scilla_error list) result =
    let { smver = mod_smver; libs; elibs = mod_elibs; contr } = md in
    let { cname = ctr_cname; cparams; cconstraint; cfields; ccomps } = contr in
    wrap_with_info (
      sprintf "Disambiguation error(s) in contract %s:\n" (as_error_string ctr_cname),
      SR.get_loc (get_rep contr.cname) )
    @@
    (* TODO: map library names to addresses, and incorporate into map *)
    let ns_map = List.filter_map mod_elibs ~f:(fun (import_name, ns) ->
      match ns with
        | Some ns_id -> Some (as_string ns_id, as_string import_name)
        | None -> None) in
    let dis_libs = 

(* TODO: Ensure that imported simple names are unique - otherwise it won't work. *)
(* TODO: Also remember to deal with builtins that may have been shadowed *)

    
*)    
end
