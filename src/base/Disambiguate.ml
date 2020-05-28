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
  module PostDisIdentifier = PostDisSyntax.SIdentifier
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

  type name_dicts = {
    ns_dict : str_str_dict;
    simp_var_dict : str_str_dict;
    simp_typ_dict : str_str_dict;
    simp_ctr_dict : str_str_dict;
  }

  let remove_local_id_from_dict dict var =
    List.Assoc.remove dict ~equal:String.( = ) var

  (**************************************************************)
  (*                   Disambiguate names                       *)
  (**************************************************************)

  let disambiguate_name ns_dict simpname_dict nm =
    let open LocalName in
    match nm with
    | SimpleLocal n -> (
        match List.Assoc.find simpname_dict n ~equal:String.( = ) with
        | Some adr ->
            (* Imported simple name *)
            pure (GlobalName.QualifiedGlobal (adr, n), n)
        | None ->
            (* Locally defined or builtin name *)
            pure (GlobalName.SimpleGlobal n, n) )
    | QualifiedLocal (ns, n) -> (
        match List.Assoc.find ns_dict ns ~equal:String.( = ) with
        | Some adr -> pure (GlobalName.QualifiedGlobal (adr, n), as_string nm)
        | None ->
            fail0 @@ sprintf "Name qualifier %s is not a legal namespace" ns )

  let local_id_as_global_name id =
    let open LocalName in
    let%bind dis_name =
      match get_id id with
      | SimpleLocal n -> pure (GlobalName.SimpleGlobal n, n)
      | QualifiedLocal (ns, n) ->
          fail0 @@ sprintf "Illegal variable name: %s.%s" ns n
    in
    pure @@ PostDisSyntax.SIdentifier.mk_id dis_name (get_rep id)

  let disambiguate_identifier ns_dict simpname_dict id =
    let%bind dis_name = disambiguate_name ns_dict simpname_dict (get_id id) in
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
          let%bind dis_t_name =
            disambiguate_identifier ns_dict simp_typ_dict t_name
          in
          let%bind dis_targs = mapM targs ~f:recurse in
          pure @@ PostDisType.ADT (dis_t_name, dis_targs)
      | TypeVar tvar -> pure @@ PostDisType.TypeVar tvar
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
          let%bind dis_s =
            disambiguate_name dicts.ns_dict dicts.simp_ctr_dict s
          in
          let%bind dis_ts =
            mapM ts ~f:(fun t ->
                disambiguate_type dicts.ns_dict dicts.simp_typ_dict t)
          in
          let%bind dis_ls = mapM ls ~f:recurser in
          pure @@ PostDisSyntax.SLiteral.ADTValue (dis_s, dis_ts, dis_ls)
      | Msg msg_entries ->
          let%bind res_msg_entries =
            foldrM msg_entries ~init:[] ~f:(fun acc (label, l) ->
                let%bind res_l = recurser l in
                pure @@ ((label, res_l) :: acc))
          in
          pure @@ ResLit.Msg res_msg_entries
      | Map ((kt, vt), mentries) ->
          let open Sexplib.Std in
          (* Use Sexplib.Std hashtable *)
          let%bind res_kt =
            disambiguate_type dicts.ns_dict dicts.simp_typ_dict kt
          in
          let%bind res_vt =
            disambiguate_type dicts.ns_dict dicts.simp_typ_dict vt
          in
          let flat_table =
            Hashtbl.fold (fun k v acc -> (k, v) :: acc) mentries []
          in
          let%bind res_flat_table =
            mapM flat_table ~f:(fun (k, v) ->
                let%bind res_k = recurser k in
                let%bind res_v = recurser v in
                pure @@ (res_k, res_v))
          in
          let res_tbl = Hashtbl.create (List.length res_flat_table) in
          let _ =
            List.iter res_flat_table ~f:(fun (k, v) -> Hashtbl.add res_tbl k v)
          in
          pure @@ ResLit.Map ((res_kt, res_vt), res_tbl)
      (* Closures and type abstractions should not appear in disambiguation phase *)
      | Clo _ ->
          raise
            (mk_internal_error "Closure literal found in disambiguation phase")
      | TAbs _ ->
          raise
            (mk_internal_error
               "Type abstraction literal found in disambiguation phase")
    in
    recurser l

  let disambiguate_pattern ns_dict simp_ctr_dict p =
    let rec recurser p =
      match p with
      | Wildcard -> pure (PostDisSyntax.Wildcard, [])
      | Binder x ->
          let%bind dis_x = local_id_as_global_name x in
          (* x is in scope as a local in rhs, so remove from var dictionary *)
          pure (PostDisSyntax.Binder dis_x, [ x ])
      | Constructor (ctr, ps) ->
          let%bind dis_ctr =
            disambiguate_identifier ns_dict simp_ctr_dict ctr
          in
          let%bind dis_ps, bounds =
            foldrM ps ~init:([], []) ~f:(fun (p_acc, bounds_acc) p' ->
                let%bind dis_p', bounds' = recurser p' in
                pure (dis_p' :: p_acc, bounds' @ bounds_acc))
          in
          pure (PostDisSyntax.Constructor (dis_ctr, dis_ps), bounds)
    in
    recurser p

  let disambiguate_exp (dicts : name_dicts) erep =
    let disambiguate_identifier_helper simp_var_dict id =
      disambiguate_identifier dicts.ns_dict simp_var_dict id
    in
    let disambiguate_type_helper t =
      disambiguate_type dicts.ns_dict dicts.simp_typ_dict t
    in
    let disambiguate_literal_helper l = disambiguate_literal dicts l in
    let rec recurser simp_var_dict erep =
      let e, rep = erep in
      let%bind new_e =
        match e with
        | Literal l ->
            let%bind dis_l = disambiguate_literal dicts l in
            pure @@ PostDisSyntax.Literal dis_l
        | Var id ->
            let%bind dis_id = disambiguate_identifier_helper simp_var_dict id in
            pure @@ PostDisSyntax.Var dis_id
        | Let (id, t, lhs, rhs) ->
            let%bind dis_id = local_id_as_global_name id in
            let%bind dis_t = option_mapM t ~f:disambiguate_type_helper in
            let%bind dis_lhs = recurser simp_var_dict lhs in
            (* id is in scope as a local in rhs, so remove from var dictionary *)
            let rhs_simp_var_dict =
              remove_local_id_from_dict simp_var_dict (as_string id)
            in
            let%bind dis_rhs = recurser rhs_simp_var_dict rhs in
            pure @@ PostDisSyntax.Let (dis_id, dis_t, dis_lhs, dis_rhs)
        | Message mentries ->
            let disambiguate_payload = function
              | MLit l ->
                  let%bind dis_l = disambiguate_literal_helper l in
                  pure @@ PostDisSyntax.MLit dis_l
              | MVar id ->
                  let%bind dis_id =
                    disambiguate_identifier_helper simp_var_dict id
                  in
                  pure @@ PostDisSyntax.MVar dis_id
            in
            let%bind dis_mentries =
              mapM mentries ~f:(fun (l, p) ->
                  let%bind dis_p = disambiguate_payload p in
                  pure @@ (l, dis_p))
            in
            pure @@ PostDisSyntax.Message dis_mentries
        | Fun (id, t, body) ->
            let%bind dis_id = local_id_as_global_name id in
            let%bind dis_t = disambiguate_type_helper t in
            (* id is in scope as a local in body, so remove from var dictionary *)
            let body_simp_var_dict =
              remove_local_id_from_dict simp_var_dict (as_string id)
            in
            let%bind dis_body = recurser body_simp_var_dict body in
            pure @@ PostDisSyntax.Fun (dis_id, dis_t, dis_body)
        | App (f, args) ->
            let%bind dis_f = disambiguate_identifier_helper simp_var_dict f in
            let%bind dis_args =
              mapM args ~f:(disambiguate_identifier_helper simp_var_dict)
            in
            pure @@ PostDisSyntax.App (dis_f, dis_args)
        | Constr (c, ts, args) ->
            let%bind dis_c =
              disambiguate_identifier_helper dicts.simp_ctr_dict c
            in
            let%bind dis_ts_args = mapM ts ~f:disambiguate_type_helper in
            let%bind dis_args =
              mapM args ~f:(disambiguate_identifier_helper simp_var_dict)
            in
            pure @@ PostDisSyntax.Constr (dis_c, dis_ts_args, dis_args)
        | MatchExpr (x, pes) ->
            let%bind dis_x = disambiguate_identifier_helper simp_var_dict x in
            let%bind dis_pes =
              mapM pes ~f:(fun (p, erep') ->
                  let%bind dis_p, bounds =
                    disambiguate_pattern dicts.ns_dict dicts.simp_ctr_dict p
                  in
                  (* bounds are in scope as locals in e, so remove from var dictionary *)
                  let erep'_simp_var_dict =
                    List.fold bounds ~init:simp_var_dict
                      ~f:(fun simp_var_dict' x ->
                        remove_local_id_from_dict simp_var_dict' (as_string x))
                  in
                  let%bind dis_erep' = recurser erep'_simp_var_dict erep' in
                  pure (dis_p, dis_erep'))
            in
            pure @@ PostDisSyntax.MatchExpr (dis_x, dis_pes)
        | Builtin (b, args) ->
            let%bind dis_args =
              mapM args ~f:(disambiguate_identifier_helper simp_var_dict)
            in
            pure @@ PostDisSyntax.Builtin (b, dis_args)
        | TFun (tvar, body) ->
            let%bind dis_tvar = local_id_as_global_name tvar in
            (* tvar is in scope as a type, but won't affect disambiguation,
               so don't worry about removing it from the environment *)
            let%bind dis_body = recurser simp_var_dict body in
            pure @@ PostDisSyntax.TFun (dis_tvar, dis_body)
        | TApp (f, targs) ->
            let%bind dis_f = disambiguate_identifier_helper simp_var_dict f in
            let%bind dis_targs = mapM targs ~f:disambiguate_type_helper in
            pure @@ PostDisSyntax.TApp (dis_f, dis_targs)
        | Fixpoint (f, t, body) ->
            let%bind dis_f = local_id_as_global_name f in
            let%bind dis_t = disambiguate_type_helper t in
            (* f is in scope as a local in body, so remove from var dictionary *)
            let body_simp_var_dict =
              remove_local_id_from_dict simp_var_dict (as_string f)
            in
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
      disambiguate_identifier dicts.ns_dict simp_var_dict id
    in
    let folder (simp_var_dict_acc, dis_stmts_acc_rev) srep =
      let s, rep = srep in
      let%bind dis_s, new_simp_var_dict =
        match s with
        | Load (x, f) ->
            let%bind dis_x = local_id_as_global_name x in
            (* f must be a locally defined field *)
            let%bind dis_f = local_id_as_global_name f in
            (* x is now in scope as a local, so remove from var dictionary *)
            let new_simp_var_dict =
              remove_local_id_from_dict simp_var_dict_acc (as_string x)
            in
            pure @@ (PostDisSyntax.Load (dis_x, dis_f), new_simp_var_dict)
        | Store (f, x) ->
            let%bind dis_f = local_id_as_global_name f in
            let%bind dis_x =
              disambiguate_identifier_helper simp_var_dict_acc x
            in
            pure @@ (PostDisSyntax.Store (dis_f, dis_x), simp_var_dict_acc)
        | Bind (x, e') ->
            let%bind dis_x = local_id_as_global_name x in
            let%bind dis_e' =
              disambiguate_exp
                { dicts with simp_var_dict = simp_var_dict_acc }
                e'
            in
            (* x is now in scope as a local, so remove from var dictionary *)
            let new_simp_var_dict =
              remove_local_id_from_dict simp_var_dict_acc (as_string x)
            in
            pure @@ (PostDisSyntax.Bind (dis_x, dis_e'), new_simp_var_dict)
        | MapUpdate (m, ks, vopt) ->
            let%bind dis_m = local_id_as_global_name m in
            let%bind dis_ks =
              mapM ks ~f:(disambiguate_identifier_helper simp_var_dict_acc)
            in
            let%bind dis_vopt =
              option_mapM vopt
                ~f:(disambiguate_identifier_helper simp_var_dict_acc)
            in
            pure
            @@ ( PostDisSyntax.MapUpdate (dis_m, dis_ks, dis_vopt),
                 simp_var_dict_acc )
        | MapGet (x, m, ks, fetch) ->
            let%bind dis_x = local_id_as_global_name x in
            let%bind dis_m = local_id_as_global_name m in
            let%bind dis_ks =
              mapM ks ~f:(disambiguate_identifier_helper simp_var_dict_acc)
            in
            (* x is now in scope as a local, so remove from var dictionary *)
            let new_simp_var_dict =
              remove_local_id_from_dict simp_var_dict_acc (as_string x)
            in
            pure
            @@ ( PostDisSyntax.MapGet (dis_x, dis_m, dis_ks, fetch),
                 new_simp_var_dict )
        | MatchStmt (x, pss) ->
            let%bind dis_x =
              disambiguate_identifier_helper simp_var_dict_acc x
            in
            let%bind dis_pss =
              mapM pss ~f:(fun (p, ss) ->
                  let%bind dis_p, bounds =
                    disambiguate_pattern dicts.ns_dict dicts.simp_ctr_dict p
                  in
                  (* bounds are in scope as locals in s, so remove from var dictionary *)
                  let ss_simp_var_dict =
                    List.fold bounds ~init:simp_var_dict_acc
                      ~f:(fun simp_var_dict' x ->
                        remove_local_id_from_dict simp_var_dict' (as_string x))
                  in
                  let%bind dis_ss =
                    disambiguate_stmts
                      { dicts with simp_var_dict = ss_simp_var_dict }
                      ss
                  in
                  pure (dis_p, dis_ss))
            in
            pure @@ (PostDisSyntax.MatchStmt (dis_x, dis_pss), simp_var_dict_acc)
        | ReadFromBC (x, f) ->
            let%bind dis_x = local_id_as_global_name x in
            pure @@ (PostDisSyntax.ReadFromBC (dis_x, f), simp_var_dict_acc)
        | AcceptPayment ->
            pure @@ (PostDisSyntax.AcceptPayment, simp_var_dict_acc)
        | Iterate (l, proc) ->
            let%bind dis_l =
              disambiguate_identifier_helper simp_var_dict_acc l
            in
            let%bind dis_proc = local_id_as_global_name proc in
            pure @@ (PostDisSyntax.Iterate (dis_l, dis_proc), simp_var_dict_acc)
        | SendMsgs msgs ->
            let%bind dis_msgs =
              disambiguate_identifier_helper simp_var_dict_acc msgs
            in
            pure @@ (PostDisSyntax.SendMsgs dis_msgs, simp_var_dict_acc)
        | CreateEvnt e ->
            let%bind dis_e =
              disambiguate_identifier_helper simp_var_dict_acc e
            in
            pure @@ (PostDisSyntax.CreateEvnt dis_e, simp_var_dict_acc)
        | CallProc (proc, args) ->
            let%bind dis_proc = local_id_as_global_name proc in
            let%bind dis_args =
              mapM args ~f:(disambiguate_identifier_helper simp_var_dict_acc)
            in
            pure
            @@ (PostDisSyntax.CallProc (dis_proc, dis_args), simp_var_dict_acc)
        | Throw xopt ->
            let%bind dis_xopt =
              option_mapM xopt
                ~f:(disambiguate_identifier_helper simp_var_dict_acc)
            in
            pure @@ (PostDisSyntax.Throw dis_xopt, simp_var_dict_acc)
      in
      pure @@ (new_simp_var_dict, (dis_s, rep) :: dis_stmts_acc_rev)
    in
    let%bind _, dis_stmts_rev =
      foldM stmts ~init:(dicts.simp_var_dict, []) ~f:folder
    in
    pure (List.rev dis_stmts_rev)

  (**************************************************************)
  (*                 Disambiguate components                    *)
  (**************************************************************)

  let disambiguate_component (dicts : name_dicts) comp =
    let { comp_type; comp_name; comp_params; comp_body } = comp in
    let%bind dis_comp_name = local_id_as_global_name comp_name in
    let%bind dis_comp_params =
      mapM comp_params ~f:(fun (x, t) ->
          let%bind dis_x = local_id_as_global_name x in
          let%bind dis_t =
            disambiguate_type dicts.ns_dict dicts.simp_typ_dict t
          in
          pure (dis_x, dis_t))
    in
    (* comp_params are now in scope as locals, so remove from var dictionary *)
    let body_simp_var_dict =
      List.fold_left comp_params ~init:dicts.simp_var_dict
        ~f:(fun dict (x, _) -> remove_local_id_from_dict dict (as_string x))
    in
    let body_dicts = { dicts with simp_var_dict = body_simp_var_dict } in
    let%bind dis_comp_body = disambiguate_stmts body_dicts comp_body in
    pure
    @@ {
         PostDisSyntax.comp_type;
         PostDisSyntax.comp_name = dis_comp_name;
         PostDisSyntax.comp_params = dis_comp_params;
         PostDisSyntax.comp_body = dis_comp_body;
       }

  (**************************************************************)
  (*                 Disambiguate libraries                     *)
  (**************************************************************)

  let disambiguate_lib_entry (dicts : name_dicts) libentry =
    match libentry with
    | LibVar (x, topt, e) ->
        let%bind dis_x = local_id_as_global_name x in
        let%bind dis_topt =
          option_mapM topt
            ~f:(disambiguate_type dicts.ns_dict dicts.simp_typ_dict)
        in
        let%bind dis_e = disambiguate_exp dicts e in
        (* x is now in scope as a local, so remove from var dictionary *)
        let res_dicts =
          {
            dicts with
            simp_var_dict =
              remove_local_id_from_dict dicts.simp_var_dict (as_string x);
          }
        in
        pure @@ (PostDisSyntax.LibVar (dis_x, dis_topt, dis_e), res_dicts)
    | LibTyp (tname, ctrs) ->
        let%bind dis_tname = local_id_as_global_name tname in
        let%bind dis_ctrs =
          mapM ctrs ~f:(fun ctr ->
              let { cname; c_arg_types } = ctr in
              let%bind dis_cname = local_id_as_global_name cname in
              let%bind dis_c_arg_types =
                mapM c_arg_types
                  ~f:(disambiguate_type dicts.ns_dict dicts.simp_typ_dict)
              in
              pure
                {
                  PostDisSyntax.cname = dis_cname;
                  PostDisSyntax.c_arg_types = dis_c_arg_types;
                })
        in
        (* tname is now in scope as a local type, and ctrs are in scope as local constructors,
           so remove from dictionaries *)
        let res_typ_dict =
          remove_local_id_from_dict dicts.simp_typ_dict (as_string tname)
        in
        let res_ctr_dict =
          List.fold_left ctrs ~init:dicts.simp_ctr_dict ~f:(fun dict ctr ->
              remove_local_id_from_dict dict (as_string ctr.cname))
        in
        let res_dicts =
          {
            dicts with
            simp_typ_dict = res_typ_dict;
            simp_ctr_dict = res_ctr_dict;
          }
        in
        pure @@ (PostDisSyntax.LibTyp (dis_tname, dis_ctrs), res_dicts)

  let disambiguate_library (dicts : name_dicts) lib =
    let { lname; lentries } = lib in
    let%bind dis_lname = local_id_as_global_name lname in
    let%bind dis_lentries_rev, _ =
      foldM lentries ~init:([], dicts)
        ~f:(fun (dis_lentries_acc_rev, dicts_acc) lentry ->
          let%bind dis_lentry, new_dicts =
            disambiguate_lib_entry dicts_acc lentry
          in
          pure (dis_lentry :: dis_lentries_acc_rev, new_dicts))
    in
    (* Toplevel names defined in the library are potentially in scope somewhere,
       so must be collected and returned *)
    let lib_simp_vars_def, lib_simp_typs_def, lib_simp_ctrs_def =
      List.fold_left lentries ~init:([], [], [])
        ~f:(fun (vars_acc, typs_acc, ctrs_acc) lentry ->
          match lentry with
          | LibVar (x, _, _) -> (as_string x :: vars_acc, typs_acc, ctrs_acc)
          | LibTyp (tname, ctrs) ->
              let new_ctrs =
                List.fold_left ctrs ~init:ctrs_acc ~f:(fun ctrs_acc' ctr ->
                    as_string ctr.cname :: ctrs_acc')
              in
              (vars_acc, as_string tname :: typs_acc, new_ctrs))
    in
    pure
    @@ ( {
           PostDisSyntax.lname = dis_lname;
           PostDisSyntax.lentries = List.rev dis_lentries_rev;
         },
         lib_simp_vars_def,
         lib_simp_typs_def,
         lib_simp_ctrs_def )

  (**************************************************************)
  (*                 Disambiguate contracts                     *)
  (**************************************************************)

  let disambiguate_contract (dicts : name_dicts) c =
    let { cname; cparams; cconstraint; cfields; ccomps } = c in
    let%bind dis_cname = local_id_as_global_name cname in
    let%bind dis_cparams =
      mapM cparams ~f:(fun (x, t) ->
          let%bind dis_x = local_id_as_global_name x in
          let%bind dis_t =
            disambiguate_type dicts.ns_dict dicts.simp_typ_dict t
          in
          pure (dis_x, dis_t))
    in
    (* cparams are now in scope as locals, so remove from var dictionary *)
    let body_var_dict =
      List.fold_left cparams ~init:dicts.simp_var_dict ~f:(fun dict (x, _) ->
          remove_local_id_from_dict dict (as_string x))
    in
    let body_dicts =
      {
        ns_dict = dicts.ns_dict;
        simp_var_dict = body_var_dict;
        simp_typ_dict = dicts.simp_typ_dict;
        simp_ctr_dict = dicts.simp_ctr_dict;
      }
    in
    let%bind dis_cconstraint = disambiguate_exp body_dicts cconstraint in
    let%bind dis_cfields =
      mapM cfields ~f:(fun (fname, t, init) ->
          let%bind dis_fname = local_id_as_global_name fname in
          let%bind dis_t =
            disambiguate_type dicts.ns_dict dicts.simp_typ_dict t
          in
          let%bind dis_init = disambiguate_exp body_dicts init in
          pure (dis_fname, dis_t, dis_init))
    in
    (* Ignore fields - they can never be confused with locals, and must be
       locally defined. *)
    let%bind dis_ccomps = mapM ccomps ~f:(disambiguate_component body_dicts) in
    pure
    @@ {
         PostDisSyntax.cname = dis_cname;
         PostDisSyntax.cparams = dis_cparams;
         PostDisSyntax.cconstraint = dis_cconstraint;
         PostDisSyntax.cfields = dis_cfields;
         PostDisSyntax.ccomps = dis_ccomps;
       }

  (**************************************************************)
  (*                  Disambiguate modules                      *)
  (**************************************************************)

  let build_import_dicts imports extlibs =
    let find_lib libname (extlibs : PostDisSyntax.libtree list) =
      let lib_name_str = as_string libname in
      match
        List.find extlibs ~f:(fun extlib ->
            String.(
              PostDisIdentifier.as_string extlib.libn.lname = lib_name_str))
      with
      | None -> fail0 @@ sprintf "Unrecognized library %s" lib_name_str
      | Some extlib -> pure extlib.libn
    in
    (* Find the address of an external library *)
    let find_lib_filename (lib : PostDisSyntax.library) =
      (* Find file name = address of external library *)
      let lib_filename =
        (SR.get_loc (PostDisIdentifier.get_rep lib.lname)).fname
      in
      (* Strip .scillib extension *)
      pure
        (String.sub lib_filename ~pos:0 ~len:(String.length lib_filename - 8))
    in
    let list_assoc_find_helper dict key =
      List.Assoc.find dict ~equal:String.( = ) key
    in

    (* Build dictionaries *)
    foldM imports ~init:([], [], [], [])
      ~f:(fun ( ns_dict_acc,
                simp_var_dict_acc,
                simp_typ_dict_acc,
                simp_ctr_dict_acc )
              (libname, ns_opt)
              ->
        let%bind lib = find_lib libname extlibs in
        let%bind lib_filename = find_lib_filename lib in
        let add_key_and_lib_filename_to_dict dict key =
          List.Assoc.add dict ~equal:String.( = ) key lib_filename
        in
        let check_duplicate dict key msg =
          match list_assoc_find_helper dict key with
          | Some _ -> fail0 msg
          | None -> pure ()
        in
        match ns_opt with
        | Some ns ->
            (* Namespace defined - only add to namespace dictionary *)
            (* Check for duplicate names - disambiguation won't work otherwise *)
            let msg =
              sprintf "Duplicate namespace %s in imports" (as_string ns)
            in
            let%bind _ = check_duplicate ns_dict_acc (as_string ns) msg in
            let ns_dict =
              add_key_and_lib_filename_to_dict ns_dict_acc (as_string ns)
            in
            pure
              (ns_dict, simp_var_dict_acc, simp_typ_dict_acc, simp_ctr_dict_acc)
        | None ->
            (* No namespace defined - add all simple names *)
            let open PostDisSyntax in
            let open PostDisIdentifier in
            let%bind simp_var_dict, simp_typ_dict, simp_ctr_dict =
              foldM lib.lentries
                ~init:(simp_var_dict_acc, simp_typ_dict_acc, simp_ctr_dict_acc)
                ~f:(fun ( simp_var_dict_acc',
                          simp_typ_dict_acc',
                          simp_ctr_dict_acc' )
                        lentry
                        ->
                  match lentry with
                  | LibVar (x, _, _) ->
                      (* simple var name *)
                      (* Check for duplicate names - disambiguation won't work otherwise.
                         Only check against previous imports -
                         duplicate names within the same library does not affect disambiguation *)
                      let msg =
                        sprintf "Variable %s imported from multiple sources"
                          (as_string x)
                      in
                      let%bind _ =
                        check_duplicate simp_var_dict_acc (as_string x) msg
                      in
                      (* Add x -> lib_filename to var dictionary *)
                      let simp_var_dict =
                        add_key_and_lib_filename_to_dict simp_var_dict_acc'
                          (as_string x)
                      in
                      pure
                        (simp_var_dict, simp_typ_dict_acc', simp_ctr_dict_acc')
                  | LibTyp (tname, ctr_defs) ->
                      (* simple type and constructor names *)
                      (* Check for duplicate names - disambiguation won't work otherwise. *)
                      let msg =
                        sprintf "Type %s imported from multiple sources"
                          (as_string tname)
                      in
                      let%bind _ =
                        check_duplicate simp_typ_dict_acc' (as_string tname) msg
                      in
                      (* Add tname -> lib_filename to type dictionary *)
                      let simp_typ_dict =
                        add_key_and_lib_filename_to_dict simp_typ_dict_acc'
                          (as_string tname)
                      in
                      (* Deal with constructors *)
                      let%bind simp_ctr_dict =
                        foldM ctr_defs ~init:simp_ctr_dict_acc'
                          ~f:(fun dict_acc (ctr_def : ctr_def) ->
                            (* Check for duplicate names - disambiguation won't work otherwise. *)
                            let msg =
                              sprintf
                                "Constructor %s imported from multiple sources"
                                (as_string ctr_def.cname)
                            in
                            let%bind _ =
                              check_duplicate dict_acc (as_string ctr_def.cname)
                                msg
                            in
                            (* Add ctr_def.cname -> lib_filename to ctr dictionary *)
                            let simp_ctr_dict =
                              add_key_and_lib_filename_to_dict dict_acc
                                (as_string ctr_def.cname)
                            in
                            pure simp_ctr_dict)
                      in
                      (* Type and constructor dictionaries updated *)
                      pure (simp_var_dict_acc', simp_typ_dict, simp_ctr_dict))
            in
            (* All library definiens added *)
            pure (ns_dict_acc, simp_var_dict, simp_typ_dict, simp_ctr_dict))

  let disambiguate_cmodule cmod (extlibs : PostDisSyntax.libtree list) =
    let { smver; libs; elibs; contr } = cmod in
    let%bind dis_elibs =
      mapM elibs ~f:(fun (lib, ns) ->
          let%bind dis_lib = local_id_as_global_name lib in
          let%bind dis_ns = option_mapM ns ~f:local_id_as_global_name in
          pure (dis_lib, dis_ns))
    in
    (* Ignore recursion principles, because their global names are their simple names.
       If a library defines a variable with the same name, then that definition shadows
       the generated recursion principle. *)
    (* Ignore name clashes with built-in datatypes and constructors.
       If such a name clash exists, then the recursion checker will catch them
       when checking the offending library. *)
    let%bind ns_dict, imp_simp_var_dict, imp_simp_typ_dict, imp_simp_ctr_dict =
      build_import_dicts elibs extlibs
    in
    let imp_dicts =
      {
        ns_dict;
        simp_var_dict = imp_simp_var_dict;
        simp_typ_dict = imp_simp_typ_dict;
        simp_ctr_dict = imp_simp_ctr_dict;
      }
    in
    let%bind dis_libs, lib_simp_vars, lib_typs, lib_ctrs =
      match libs with
      | None -> pure (None, [], [], [])
      | Some lib ->
          let%bind dis_lib, lib_simp_var, lib_typs, lib_ctrs =
            disambiguate_library imp_dicts lib
          in
          pure (Some dis_lib, lib_simp_var, lib_typs, lib_ctrs)
    in
    (* Remove contract's own library definitions *)
    let contract_simp_var_dict =
      List.fold_left lib_simp_vars ~init:imp_simp_var_dict
        ~f:remove_local_id_from_dict
    in
    let contract_simp_typ_dict =
      List.fold_left lib_typs ~init:imp_simp_typ_dict
        ~f:remove_local_id_from_dict
    in
    let contract_simp_ctr_dict =
      List.fold_left lib_ctrs ~init:imp_simp_ctr_dict
        ~f:remove_local_id_from_dict
    in
    let contract_dicts =
      {
        ns_dict;
        simp_var_dict = contract_simp_var_dict;
        simp_typ_dict = contract_simp_typ_dict;
        simp_ctr_dict = contract_simp_ctr_dict;
      }
    in
    let%bind dis_contr = disambiguate_contract contract_dicts contr in
    pure
    @@ {
         PostDisSyntax.smver;
         PostDisSyntax.libs = dis_libs;
         PostDisSyntax.elibs = dis_elibs;
         PostDisSyntax.contr = dis_contr;
       }

  let disambiguate_lmodule lmod (extlibs : PostDisSyntax.libtree list) =
    let { smver; libs; elibs } = lmod in
    let%bind dis_elibs =
      mapM elibs ~f:(fun (lib, ns) ->
          let%bind dis_lib = local_id_as_global_name lib in
          let%bind dis_ns = option_mapM ns ~f:local_id_as_global_name in
          pure (dis_lib, dis_ns))
    in
    (* Ignore recursion principles, because their global names are their simple names.
       If a library defines a variable with the same name, then that definition shadows
       the generated recursion principle. *)
    (* Ignore name clashes with built-in datatypes and constructors.
       If such a name clash exists, then the recursion checker will catch them
       when checking the offending library. *)
    let%bind ns_dict, imp_simp_var_dict, imp_simp_typ_dict, imp_simp_ctr_dict =
      build_import_dicts elibs extlibs
    in
    let imp_dicts =
      {
        ns_dict;
        simp_var_dict = imp_simp_var_dict;
        simp_typ_dict = imp_simp_typ_dict;
        simp_ctr_dict = imp_simp_ctr_dict;
      }
    in
    let%bind dis_libs, _, _, _ = disambiguate_library imp_dicts libs in
    pure
    @@ {
         PostDisSyntax.smver;
         PostDisSyntax.elibs = dis_elibs;
         PostDisSyntax.libs = dis_libs;
       }
end
