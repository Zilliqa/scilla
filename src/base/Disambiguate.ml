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
      ~equal:[%equal : LocalName.t] var
    
  (**************************************************************)
  (*                   Disambiguate names                       *)
  (**************************************************************)

  let disambiguate_name ns_adr_dict simpname_adr_dict nm =
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
        match List.Assoc.find ns_adr_dict ns ~equal:String.(=) with
        | Some adr -> pure (GlobalName.QualifiedGlobal (adr, n), as_string nm)
        | None ->
            fail0 @@
            sprintf "Name qualifier %s is not a legal namespace" ns

  let disambiguate_identifier ns_adr_dict simpname_adr_dict id =
    let%bind dis_name = disambiguate_name ns_adr_dict simpname_adr_dict (get_id id) in
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

  let disambiguate_literal dicts l =
    let { ns_dict ; simp_var_dict ; simp_typ_dict ; simp_ctr_dict } = dicts in
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
          let%bind dis_s = disambiguate_identifier
          let%bind dis_ts = mapM ts
              ~f:(fun t -> disambiguate_type ns_dict simp_typ_dict t) in
          let%bind dis_ls = mapM ls ~f:recurser in
          pure @@ PostDisSyntax.SLiteral.ADTValue (s, dis_ts, dis_ls)
      | Msg msg_entries ->
          let%bind res_msg_entries = foldrM msg_entries ~init:[]
              ~f:(fun acc (label, l) ->
                  let%bind res_l = recurser l in
                  pure @@ (label, res_l) :: acc) in
          pure @@ ResLit.Msg res_msg_entries
      | Map ((kt, vt), mentries) ->
          let open Sexplib.Std in (* Use Sexplib.Std hashtable *)
          let%bind res_kt = disambiguate_type ns_dict simp_typ_dict kt in
          let%bind res_vt = disambiguate_type ns_dict simp_typ_dict vt in
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
    
  let rec disambiguate_exp dicts erep =
    let { ns_dict ; simp_var_dict ; simp_typ_dict ; simp_ctr_dict } = dicts in
    let mk_dis_helper f simp_dict = f ns_dict simp_dict in
    let dis_var_helper new_simp_var_dict = mk_dis_helper disambiguate_identifier simp_var_dict in
    let dis_ctr_helper = mk_dis_helper disambiguate_identifier simp_ctr_dict in
    let dis_typ_helper = mk_dis_helper disambiguate_type simp_typ_dict in
    let dis_lit_helper new_simp_var_dict = mk_dis_helper disambiguate_literal dicts in
    let rec recurser erep = 
      let e, rep = erep in
      let new_e =
        match e with
        | Literal l ->
            let%bind dis_l = dis_lit_helper l in
            pure @@ PostDisSyntax.Literal dis_l
        | Var id ->
            let%bind dis_id = dis_id_helper id in
            pure @@ PostDisSyntax.Var dis_id
        | Let (id, t, lhs, rhs) ->
            let%bind dis_id = dis_id_helper id in
            let%bind dis_t = option_mapM t ~f:dis_typ_helper in
            let%bind dis_lhs = recurser lhs in
            (* id is now in scope, so disambiguate as local *)
            let rhs_simpname_adr_dict = add_local_id_to_dict simpname_adr_dict id in
            let%bind dis_rhs = disambiguate_exp ns_adr_dict rhs_simpname_adr_dict rhs in
            pure @@ PostDisSyntax.Let (dis_id, dis_t, dis_lhs, dis_rhs)
        | Message mentries ->
            let disambiguate_payload = function
              | MLit l ->
                  let%bind dis_l = dis_lit_helper l in
                  pure @@ PostDisSyntax.MLit dis_l
              | MVar id ->
                  let%bind dis_id = dis_id_helper id in
                  pure @@ PostDisSyntax.MVar dis_id
            in
            let%bind dis_mentries = mapM mentries
                ~f:(fun (l, p) ->
                    let%bind dis_p = disambiguate_payload p in
                    pure @@ (l, dis_p)) in
            pure @@ PostDisSyntax.Message dis_mentries
        | Fun (id, t, body) ->
            let%bind dis_id = dis_id_helper id in
            let%bind dis_t = dis_typ_helper t in
            (* id is now in scope, so disambiguate as local *)
            let body_simpname_adr_dict = add_local_id_to_dict simpname_adr_dict id in
            let%bind dis_body = disambiguate_exp ns_adr_dict body_simpname_adr_dict body in
            pure @@ PostDisSyntax.Fun (dis_id, dis_t, dis_body)
        | App (f, args) ->
            let%bind dis_f = dis_id_helper f in
            let%bind dis_args = mapM args ~f:dis_id_helper in
            pure @@ PostDisSyntax.App (dis_f, dis_args)
        | Constr (c, ts, args) ->
            let%bind dis_c = dis_ctr_helper c in
            let%bind dis_ts_args = mapM ts ~f:dis_typ_helper in
            let%bind dis_args = mapM args ~f:dis_id_helper in
            pure @@ PostDisSyntax.Constr (dis_c, dis_ts_args, dis_args)

  (* TODO: types and variables live in separate namespaces, so need to have be in different dictionaries :-( *)

  
  (* 
    | MatchExpr of ER.rep SIdentifier.t * (pattern * expr_annot) list
    | Builtin of ER.rep builtin_annot * ER.rep SIdentifier.t list
    (* Advanced features: to be added in Scilla 0.2 *)
    | TFun of ER.rep SIdentifier.t * expr_annot
    | TApp of ER.rep SIdentifier.t * SType.t list
    (* Fixpoint combinator: used to implement recursion principles *)
    | Fixpoint of ER.rep SIdentifier.t * SType.t * expr_annot
 *)      

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

