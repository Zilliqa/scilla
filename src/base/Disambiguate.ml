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

  (* Name dictionaries.
     Maps an optional namespace and a simple name to the address defining the name *)
  type nsopt_name_adr_dict =
    (string option, (string, string) List.Assoc.t) List.Assoc.t

  type name_dicts = {
    var_dict : nsopt_name_adr_dict;
    typ_dict : nsopt_name_adr_dict;
    ctr_dict : nsopt_name_adr_dict;
  }

  let find_ns_dict dict ns_opt =
    Option.value
      (List.Assoc.find dict ns_opt ~equal:[%equal: String.t option])
      ~default:[]

  let add_ns_nmdict_to_dict (dict : nsopt_name_adr_dict) ns_opt nmdict =
    List.Assoc.add dict ns_opt nmdict ~equal:[%equal: String.t option]

  let add_key_address_to_dict dict name_key value =
    List.Assoc.add dict name_key value ~equal:String.( = )

  let combine_dicts ~old_dict ~new_dict =
    List.fold_left new_dict ~init:old_dict ~f:(fun acc (x, ns) ->
        add_key_address_to_dict acc x ns)

  let add_key_and_lib_address_to_dict (dict : nsopt_name_adr_dict) ns_key
      name_key value =
    let old_nm_dict =
      Option.value
        (List.Assoc.find dict ns_key ~equal:[%equal: String.t option])
        ~default:[]
    in
    let new_nm_dict = add_key_address_to_dict old_nm_dict name_key value in
    add_ns_nmdict_to_dict dict ns_key new_nm_dict

  let remove_local_id_from_dict (dict : nsopt_name_adr_dict) var =
    (* A locally defined identifier does not belong to a namespace,
       so look up the dictionary associated with None *)
    let old_nm_dict =
      Option.value
        (List.Assoc.find dict None ~equal:[%equal: String.t option])
        ~default:[]
    in
    let new_nm_dict = List.Assoc.remove old_nm_dict var ~equal:String.( = ) in
    List.Assoc.add dict None new_nm_dict ~equal:[%equal: String.t option]

  let check_duplicate_dict_entry nm_dict name_key msg error_loc =
    match List.Assoc.find nm_dict name_key ~equal:String.( = ) with
    | None -> pure () (* Name has not already been defined *)
    | Some addr -> fail1 ~kind:(sprintf "%s %s" msg addr) ?inst:None error_loc

  (* Name has already been defined *)

  let check_duplicate_ns_dict_entry (dict : nsopt_name_adr_dict) ns_key name_key
      msg error_loc =
    match List.Assoc.find dict ns_key ~equal:[%equal: String.t option] with
    | None -> pure () (* No names defined in namespace *)
    | Some nm_dict -> check_duplicate_dict_entry nm_dict name_key msg error_loc

  let strip_filename_extension x = Filename.chop_extension (Filename.basename x)

  let get_unqualified_name = function
    | GlobalName.SimpleGlobal n, _ | GlobalName.QualifiedGlobal (_, n), _ -> n

  (**************************************************************)
  (*                   Disambiguate names                       *)
  (**************************************************************)

  let disambiguate_name (dict : nsopt_name_adr_dict) nm error_loc =
    let open LocalName in
    match nm with
    | SimpleLocal n -> (
        (* Check if a definition exists in the default namespace *)
        match List.Assoc.find dict None ~equal:[%equal: String.t option] with
        | None ->
            (* No names imported into the default namespace. The name is
                a predefined name, or a local variable defined in impure code *)
            pure (GlobalName.SimpleGlobal n, n)
        | Some nm_dict -> (
            match List.Assoc.find nm_dict n ~equal:String.( = ) with
            | None ->
                (* Name does not exist in dictionary. If a the name has been
                     imported, then a local definition shadows it, so treat
                     it as a locally defined name*)
                pure (GlobalName.SimpleGlobal n, n)
            | Some adr ->
                (* Name is defined in adr, and imported into the default
                     namespace. *)
                pure (GlobalName.QualifiedGlobal (adr, n), as_string nm)))
    | QualifiedLocal (ns, n) -> (
        (* Check the specified namespace *)
        match
          List.Assoc.find dict (Some ns) ~equal:[%equal: String.t option]
        with
        | None -> fail1 ~kind:"Unknown namespace" ~inst:ns error_loc
        | Some nm_dict -> (
            (* Check the names in the namespace *)
            match List.Assoc.find nm_dict n ~equal:String.( = ) with
            | None ->
                fail1
                  ~kind:
                    (sprintf "Name %s is not defined in the namespace %s" n ns)
                  ?inst:None error_loc
            | Some adr ->
                (* Name defined at adr. *)
                pure (GlobalName.QualifiedGlobal (adr, n), as_string nm)))

  (* Local definition that needs an address qualifier:
     User-defined types and constructors, and library variables *)
  let name_def_as_qualified_global this_address id =
    let open LocalName in
    let%bind dis_name =
      match get_id id with
      | SimpleLocal n -> pure (GlobalName.QualifiedGlobal (this_address, n), n)
      | QualifiedLocal _ ->
          fail0 ~kind:"Illegal variable, type or constructor name"
            ~inst:(as_error_string (get_id id))
    in
    pure @@ PostDisSyntax.SIdentifier.mk_id dis_name (get_rep id)

  (* Local definition that does not need an address qualifier:
     Local variables, fields (local and remote), and procedure, transition and contract parameters,
     as well as non-type, non-constructor and non-variable names. *)
  let name_def_as_simple_global id =
    let open LocalName in
    let%bind dis_name =
      match get_id id with
      | SimpleLocal n -> pure (GlobalName.SimpleGlobal n, n)
      | QualifiedLocal (_, _) ->
          fail0 ~kind:"Illegal name" ~inst:(as_error_string (get_id id))
    in
    pure @@ PostDisSyntax.SIdentifier.mk_id dis_name (get_rep id)

  let disambiguate_identifier (ns_dict : nsopt_name_adr_dict) id error_loc =
    let%bind dis_name = disambiguate_name ns_dict (get_id id) error_loc in
    pure @@ PostDisSyntax.SIdentifier.mk_id dis_name (get_rep id)

  (**************************************************************)
  (*                   Disambiguate types                       *)
  (**************************************************************)

  let disambiguate_type typ_dict t =
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
            disambiguate_identifier typ_dict t_name (get_rep t_name)
          in
          let%bind dis_targs = mapM targs ~f:recurse in
          pure @@ PostDisType.ADT (dis_t_name, dis_targs)
      | TypeVar tvar -> pure @@ PostDisType.TypeVar tvar
      | PolyFun (tvar, t) ->
          (* No need to remove tvar from typ_dict.
             Type variables are syntactically distinct from type names. *)
          let%bind dis_t = recurse t in
          pure @@ PostDisType.PolyFun (tvar, dis_t)
      | Unit -> pure @@ PostDisType.Unit
      | Address AnyAddr -> pure @@ PostDisType.Address AnyAddr
      | Address CodeAddr -> pure @@ PostDisType.Address CodeAddr
      | Address LibAddr -> pure @@ PostDisType.Address LibAddr
      | Address (ContrAddr fts) ->
          let%bind dis_fts =
            foldM (IdLoc_Comp.Map.to_alist fts)
              ~init:PostDisType.IdLoc_Comp.Map.empty ~f:(fun acc (id, t) ->
                let%bind dis_id = name_def_as_simple_global id in
                let%bind dis_t = recurse t in
                pure
                @@ PostDisType.IdLoc_Comp.Map.set acc ~key:dis_id ~data:dis_t)
          in
          pure @@ PostDisType.Address (ContrAddr dis_fts)
    in

    recurse t

  (**************************************************************)
  (*               Disambiguating explict gas charges           *)
  (**************************************************************)

  (* No variable bindings in gas charge nodes, so dis_id_helper just wraps
     a call to disambiguate_identifier using the appropriate dictionaries. *)

  let disambiguate_gas_charge dis_id_helper gc =
    let rec recurser gc =
      let open PreDisSyntax.SGasCharge in
      match gc with
      | StaticCost i -> pure @@ PostDisSyntax.SGasCharge.StaticCost i
      | SizeOf v ->
          let%bind dis_v = dis_id_helper v in
          pure @@ PostDisSyntax.SGasCharge.SizeOf dis_v
      | ValueOf v ->
          let%bind dis_v = dis_id_helper v in
          pure @@ PostDisSyntax.SGasCharge.ValueOf dis_v
      | LengthOf v ->
          let%bind dis_v = dis_id_helper v in
          pure @@ PostDisSyntax.SGasCharge.LengthOf dis_v
      | MapSortCost m ->
          let%bind dis_m = dis_id_helper m in
          pure @@ PostDisSyntax.SGasCharge.MapSortCost dis_m
      | SumOf (g1, g2) ->
          let%bind dis_g1 = recurser g1 in
          let%bind dis_g2 = recurser g2 in
          pure @@ PostDisSyntax.SGasCharge.SumOf (dis_g1, dis_g2)
      | ProdOf (g1, g2) ->
          let%bind dis_g1 = recurser g1 in
          let%bind dis_g2 = recurser g2 in
          pure @@ PostDisSyntax.SGasCharge.ProdOf (dis_g1, dis_g2)
      | MinOf (g1, g2) ->
          let%bind dis_g1 = recurser g1 in
          let%bind dis_g2 = recurser g2 in
          pure @@ PostDisSyntax.SGasCharge.MinOf (dis_g1, dis_g2)
      | DivCeil (g1, g2) ->
          let%bind dis_g1 = recurser g1 in
          pure @@ PostDisSyntax.SGasCharge.DivCeil (dis_g1, g2)
      | LogOf g ->
          let%bind dis_g = recurser g in
          pure @@ PostDisSyntax.SGasCharge.LogOf dis_g
    in
    recurser gc

  (**************************************************************)
  (*                Disambiguate expressions                    *)
  (**************************************************************)

  let disambiguate_literal (dicts : name_dicts) l error_loc =
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
      | ADTValue (s, ts, ls) ->
          let%bind dis_s = disambiguate_name dicts.ctr_dict s error_loc in
          let%bind dis_ts =
            mapM ts ~f:(fun t -> disambiguate_type dicts.typ_dict t)
          in
          let%bind dis_ls = mapM ls ~f:recurser in
          pure @@ PostDisSyntax.SLiteral.ADTValue (dis_s, dis_ts, dis_ls)
      | Msg msg_entries ->
          (* Msg literals are strictly speaking illegal, but the parser should prevent us from ever getting here. *)
          let%bind res_msg_entries =
            foldrM msg_entries ~init:[] ~f:(fun acc (label, t, l) ->
                let%bind res_l = recurser l in
                let%bind res_t = disambiguate_type dicts.typ_dict t in
                pure @@ (label, res_t, res_l) :: acc)
          in
          pure @@ ResLit.Msg res_msg_entries
      | Map ((kt, vt), mentries) ->
          (* A map literal can only be Empty, but we disambiguate everything just in case *)
          let open Sexplib.Std in
          (* Use Sexplib.Std hashtable *)
          let%bind res_kt = disambiguate_type dicts.typ_dict kt in
          let%bind res_vt = disambiguate_type dicts.typ_dict vt in
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
            (mk_internal_error
               ~kind:"Closure literal found in disambiguation phase" ?inst:None)
      | TAbs _ ->
          raise
            (mk_internal_error
               ~kind:"Type abstraction literal found in disambiguation phase"
               ?inst:None)
    in
    recurser l

  let disambiguate_pattern ctr_dict p =
    let rec recurser p =
      match p with
      | Wildcard -> pure (PostDisSyntax.Wildcard, [])
      | Binder x ->
          let%bind dis_x = name_def_as_simple_global x in
          pure (PostDisSyntax.Binder dis_x, [ x ])
      | Constructor (ctr, ps) ->
          let%bind dis_ctr =
            disambiguate_identifier ctr_dict ctr (SR.get_loc (get_rep ctr))
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
    let disambiguate_name_helper simp_var_dict loc nm =
      disambiguate_name simp_var_dict nm loc
    in
    let disambiguate_identifier_helper simp_var_dict loc id =
      disambiguate_identifier simp_var_dict id loc
    in
    let disambiguate_type_helper t = disambiguate_type dicts.typ_dict t in
    let disambiguate_literal_helper loc l = disambiguate_literal dicts l loc in
    let rec recurser simp_var_dict erep =
      let e, rep = erep in
      let%bind new_e =
        match e with
        | Literal l ->
            let%bind dis_l = disambiguate_literal dicts l (ER.get_loc rep) in
            pure @@ PostDisSyntax.Literal dis_l
        | Var id ->
            let%bind dis_id =
              disambiguate_identifier_helper simp_var_dict (ER.get_loc rep) id
            in
            pure @@ PostDisSyntax.Var dis_id
        | Let (id, t, lhs, rhs) ->
            let%bind dis_id = name_def_as_simple_global id in
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
                  let%bind dis_l =
                    disambiguate_literal_helper (ER.get_loc rep) l
                  in
                  pure @@ PostDisSyntax.MLit dis_l
              | MVar id ->
                  let%bind dis_id =
                    disambiguate_identifier_helper simp_var_dict
                      (ER.get_loc rep) id
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
            let%bind dis_id = name_def_as_simple_global id in
            let%bind dis_t = disambiguate_type_helper t in
            (* id is in scope as a local in body, so remove from var dictionary *)
            let body_simp_var_dict =
              remove_local_id_from_dict simp_var_dict (as_string id)
            in
            let%bind dis_body = recurser body_simp_var_dict body in
            pure @@ PostDisSyntax.Fun (dis_id, dis_t, dis_body)
        | App (f, args) ->
            let%bind dis_f =
              disambiguate_identifier_helper simp_var_dict (ER.get_loc rep) f
            in
            let%bind dis_args =
              mapM args
                ~f:
                  (disambiguate_identifier_helper simp_var_dict (ER.get_loc rep))
            in
            pure @@ PostDisSyntax.App (dis_f, dis_args)
        | Constr (c, ts, args) ->
            let%bind dis_c =
              disambiguate_identifier_helper dicts.ctr_dict (ER.get_loc rep) c
            in
            let%bind dis_ts_args = mapM ts ~f:disambiguate_type_helper in
            let%bind dis_args =
              mapM args
                ~f:
                  (disambiguate_identifier_helper simp_var_dict (ER.get_loc rep))
            in
            pure @@ PostDisSyntax.Constr (dis_c, dis_ts_args, dis_args)
        | MatchExpr (x, pes) ->
            let%bind dis_x =
              disambiguate_identifier_helper simp_var_dict (ER.get_loc rep) x
            in
            let%bind dis_pes =
              mapM pes ~f:(fun (p, erep') ->
                  let%bind dis_p, bounds =
                    disambiguate_pattern dicts.ctr_dict p
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
        | Builtin (b, targs, args) ->
            let%bind dis_targs = mapM targs ~f:disambiguate_type_helper in
            let%bind dis_args =
              mapM args
                ~f:
                  (disambiguate_identifier_helper simp_var_dict (ER.get_loc rep))
            in
            pure @@ PostDisSyntax.Builtin (b, dis_targs, dis_args)
        | TFun (tvar, body) ->
            let%bind dis_tvar = name_def_as_simple_global tvar in
            (* tvar is in scope as a type, but won't affect disambiguation,
               so don't worry about removing it from the environment *)
            let%bind dis_body = recurser simp_var_dict body in
            pure @@ PostDisSyntax.TFun (dis_tvar, dis_body)
        | TApp (f, targs) ->
            let%bind dis_f =
              disambiguate_identifier_helper simp_var_dict (ER.get_loc rep) f
            in
            let%bind dis_targs = mapM targs ~f:disambiguate_type_helper in
            pure @@ PostDisSyntax.TApp (dis_f, dis_targs)
        | Fixpoint (f, t, body) ->
            let%bind dis_f = name_def_as_simple_global f in
            let%bind dis_t = disambiguate_type_helper t in
            (* f is in scope as a local in body, so remove from var dictionary *)
            let body_simp_var_dict =
              remove_local_id_from_dict simp_var_dict (as_string f)
            in
            let%bind dis_body = recurser body_simp_var_dict body in
            pure @@ PostDisSyntax.Fixpoint (dis_f, dis_t, dis_body)
        | GasExpr (g, e) ->
            let%bind dis_g =
              disambiguate_gas_charge
                (disambiguate_name_helper simp_var_dict (ER.get_loc rep))
                g
            in
            let%bind dis_e = recurser simp_var_dict e in
            pure @@ PostDisSyntax.GasExpr (dis_g, dis_e)
      in
      pure @@ (new_e, rep)
    in
    recurser dicts.var_dict erep

  (**************************************************************)
  (*                Disambiguate statements                     *)
  (**************************************************************)

  let rec disambiguate_stmts (dicts : name_dicts) stmts =
    let disambiguate_name_helper simp_var_dict loc nm =
      disambiguate_name simp_var_dict nm loc
    in
    let disambiguate_identifier_helper simp_var_dict loc id =
      disambiguate_identifier simp_var_dict id loc
    in
    let folder (var_dict_acc, dis_stmts_acc_rev) srep =
      let s, rep = srep in
      let%bind dis_s, new_var_dict =
        match s with
        | Load (x, f) ->
            let%bind dis_x = name_def_as_simple_global x in
            (* f must be a locally defined field *)
            let%bind dis_f = name_def_as_simple_global f in
            (* x is now in scope as a local, so remove from var dictionary *)
            let new_var_dict =
              remove_local_id_from_dict var_dict_acc (as_string x)
            in
            pure @@ (PostDisSyntax.Load (dis_x, dis_f), new_var_dict)
        | RemoteLoad (x, adr, f) ->
            let%bind dis_x = name_def_as_simple_global x in
            (* adr may be defined anywhere, so must be disambiguated *)
            let%bind dis_adr =
              disambiguate_identifier_helper var_dict_acc (SR.get_loc rep) adr
            in
            (* f must be a field *)
            let%bind dis_f = name_def_as_simple_global f in
            (* x is now in scope as a local, so remove from var dictionary *)
            let new_var_dict =
              remove_local_id_from_dict var_dict_acc (as_string x)
            in
            pure
            @@ (PostDisSyntax.RemoteLoad (dis_x, dis_adr, dis_f), new_var_dict)
        | Store (f, x) ->
            (* f must be a locally defined field *)
            let%bind dis_f = name_def_as_simple_global f in
            let%bind dis_x =
              disambiguate_identifier_helper var_dict_acc (SR.get_loc rep) x
            in
            pure @@ (PostDisSyntax.Store (dis_f, dis_x), var_dict_acc)
        | Bind (x, e') ->
            let%bind dis_x = name_def_as_simple_global x in
            let%bind dis_e' =
              disambiguate_exp { dicts with var_dict = var_dict_acc } e'
            in
            (* x is now in scope as a local, so remove from var dictionary *)
            let new_var_dict =
              remove_local_id_from_dict var_dict_acc (as_string x)
            in
            pure @@ (PostDisSyntax.Bind (dis_x, dis_e'), new_var_dict)
        | MapUpdate (m, ks, vopt) ->
            (* m must be a locally defined field *)
            let%bind dis_m = name_def_as_simple_global m in
            let%bind dis_ks =
              mapM ks
                ~f:
                  (disambiguate_identifier_helper var_dict_acc (SR.get_loc rep))
            in
            let%bind dis_vopt =
              option_mapM vopt
                ~f:
                  (disambiguate_identifier_helper var_dict_acc (SR.get_loc rep))
            in
            pure
            @@ (PostDisSyntax.MapUpdate (dis_m, dis_ks, dis_vopt), var_dict_acc)
        | MapGet (x, m, ks, fetch) ->
            let%bind dis_x = name_def_as_simple_global x in
            (* m must be a locally defined field *)
            let%bind dis_m = name_def_as_simple_global m in
            let%bind dis_ks =
              mapM ks
                ~f:
                  (disambiguate_identifier_helper var_dict_acc (SR.get_loc rep))
            in
            (* x is now in scope as a local, so remove from var dictionary *)
            let new_var_dict =
              remove_local_id_from_dict var_dict_acc (as_string x)
            in
            pure
            @@ (PostDisSyntax.MapGet (dis_x, dis_m, dis_ks, fetch), new_var_dict)
        | RemoteMapGet (x, adr, m, ks, fetch) ->
            let%bind dis_x = name_def_as_simple_global x in
            (* adr may be defined anywhere, so must be disambiguated *)
            let%bind dis_adr =
              disambiguate_identifier_helper var_dict_acc (SR.get_loc rep) adr
            in
            (* m must be a field *)
            let%bind dis_m = name_def_as_simple_global m in
            let%bind dis_ks =
              mapM ks
                ~f:
                  (disambiguate_identifier_helper var_dict_acc (SR.get_loc rep))
            in
            (* x is now in scope as a local, so remove from var dictionary *)
            let new_var_dict =
              remove_local_id_from_dict var_dict_acc (as_string x)
            in
            pure
            @@ ( PostDisSyntax.RemoteMapGet
                   (dis_x, dis_adr, dis_m, dis_ks, fetch),
                 new_var_dict )
        | MatchStmt (x, pss) ->
            let%bind dis_x =
              disambiguate_identifier_helper var_dict_acc (SR.get_loc rep) x
            in
            let%bind dis_pss =
              mapM pss ~f:(fun (p, ss) ->
                  let%bind dis_p, bounds =
                    disambiguate_pattern dicts.ctr_dict p
                  in
                  (* bounds are in scope as locals in s, so remove from var dictionary *)
                  let ss_var_dict =
                    List.fold bounds ~init:var_dict_acc ~f:(fun var_dict' x ->
                        remove_local_id_from_dict var_dict' (as_string x))
                  in
                  let%bind dis_ss =
                    disambiguate_stmts { dicts with var_dict = ss_var_dict } ss
                  in
                  pure (dis_p, dis_ss))
            in
            pure @@ (PostDisSyntax.MatchStmt (dis_x, dis_pss), var_dict_acc)
        | ReadFromBC (x, f) ->
            let disambiguate_bcinfo = function
              | CurBlockNum -> pure @@ PostDisSyntax.CurBlockNum
              | Timestamp id ->
                  let%bind dis_id =
                    disambiguate_identifier_helper var_dict_acc (SR.get_loc rep)
                      id
                  in
                  pure @@ PostDisSyntax.Timestamp dis_id
            in
            let%bind f' = disambiguate_bcinfo f in
            let%bind dis_x = name_def_as_simple_global x in
            (* x is now in scope as a local, so remove from var dictionary *)
            let new_var_dict =
              remove_local_id_from_dict var_dict_acc (as_string x)
            in
            pure @@ (PostDisSyntax.ReadFromBC (dis_x, f'), new_var_dict)
        | TypeCast (x, r, t) ->
            let%bind dis_x = name_def_as_simple_global x in
            let%bind dis_r = name_def_as_simple_global r in
            let%bind dis_t = disambiguate_type dicts.typ_dict t in
            let new_var_dict =
              remove_local_id_from_dict var_dict_acc (as_string x)
            in
            pure @@ (PostDisSyntax.TypeCast (dis_x, dis_r, dis_t), new_var_dict)
        | AcceptPayment -> pure @@ (PostDisSyntax.AcceptPayment, var_dict_acc)
        | Iterate (l, proc) ->
            let%bind dis_l =
              disambiguate_identifier_helper var_dict_acc (SR.get_loc rep) l
            in
            (* Only locally defined procedures are allowed *)
            let%bind dis_proc = name_def_as_simple_global proc in
            pure @@ (PostDisSyntax.Iterate (dis_l, dis_proc), var_dict_acc)
        | SendMsgs msgs ->
            let%bind dis_msgs =
              disambiguate_identifier_helper var_dict_acc (SR.get_loc rep) msgs
            in
            pure @@ (PostDisSyntax.SendMsgs dis_msgs, var_dict_acc)
        | CreateEvnt e ->
            let%bind dis_e =
              disambiguate_identifier_helper var_dict_acc (SR.get_loc rep) e
            in
            pure @@ (PostDisSyntax.CreateEvnt dis_e, var_dict_acc)
        | CallProc (proc, args) ->
            (* Only locally defined procedures are allowed *)
            let%bind dis_proc = name_def_as_simple_global proc in
            let%bind dis_args =
              mapM args
                ~f:
                  (disambiguate_identifier_helper var_dict_acc (SR.get_loc rep))
            in
            pure @@ (PostDisSyntax.CallProc (dis_proc, dis_args), var_dict_acc)
        | Throw xopt ->
            let%bind dis_xopt =
              option_mapM xopt
                ~f:
                  (disambiguate_identifier_helper var_dict_acc (SR.get_loc rep))
            in
            pure @@ (PostDisSyntax.Throw dis_xopt, var_dict_acc)
        | GasStmt g ->
            let%bind dis_g =
              disambiguate_gas_charge
                (disambiguate_name_helper var_dict_acc (SR.get_loc rep))
                g
            in
            pure @@ (PostDisSyntax.GasStmt dis_g, var_dict_acc)
      in
      pure @@ (new_var_dict, (dis_s, rep) :: dis_stmts_acc_rev)
    in
    let%bind _, dis_stmts_rev =
      foldM stmts ~init:(dicts.var_dict, []) ~f:folder
    in
    pure (List.rev dis_stmts_rev)

  (**************************************************************)
  (*                 Disambiguate components                    *)
  (**************************************************************)

  let disambiguate_component (dicts : name_dicts) comp =
    let { comp_type; comp_name; comp_params; comp_body } = comp in
    let%bind dis_comp_name = name_def_as_simple_global comp_name in
    let%bind dis_comp_params =
      mapM comp_params ~f:(fun (x, t) ->
          (* Parameters are locally defined simple names *)
          let%bind dis_x = name_def_as_simple_global x in
          let%bind dis_t = disambiguate_type dicts.typ_dict t in
          pure (dis_x, dis_t))
    in
    (* comp_params are now in scope as locals, so remove from var dictionary *)
    let body_var_dict =
      List.fold_left comp_params ~init:dicts.var_dict ~f:(fun dict (x, _) ->
          remove_local_id_from_dict dict (as_string x))
    in
    let body_dicts = { dicts with var_dict = body_var_dict } in
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

  let disambiguate_lib_entry (dicts : name_dicts) libentry this_address =
    match libentry with
    | LibVar (x, topt, e) ->
        let%bind dis_x = name_def_as_qualified_global this_address x in
        let%bind dis_topt =
          option_mapM topt ~f:(disambiguate_type dicts.typ_dict)
        in
        let%bind dis_e = disambiguate_exp dicts e in
        (* x is now in scope as a locally defined library variable, so update simple
           var dictionary to point to this_address.
           If the name was previously defined, then the new definition shadows
           the old one, so don't check for previous definitions. *)
        let res_dicts =
          {
            dicts with
            var_dict =
              add_key_and_lib_address_to_dict dicts.var_dict None (as_string x)
                this_address;
          }
        in
        pure @@ (PostDisSyntax.LibVar (dis_x, dis_topt, dis_e), res_dicts)
    | LibTyp (tname, ctrs) ->
        let%bind dis_tname = name_def_as_qualified_global this_address tname in
        let%bind dis_ctrs =
          mapM ctrs ~f:(fun ctr ->
              let { cname; c_arg_types } = ctr in
              let%bind dis_cname =
                name_def_as_qualified_global this_address cname
              in
              let%bind dis_c_arg_types =
                mapM c_arg_types ~f:(disambiguate_type dicts.typ_dict)
              in
              pure
                {
                  PostDisSyntax.cname = dis_cname;
                  PostDisSyntax.c_arg_types = dis_c_arg_types;
                })
        in
        (* tname is now in scope as a local type, and ctrs are in scope as local constructors.
           Reject name clashes with imported types and constructors.
           Then map simple names to the address of the current module. *)
        let%bind res_typ_dict =
          let msg =
            sprintf "Multiple declarations of type %s in library"
              (as_error_string tname)
          in
          let%bind () =
            check_duplicate_ns_dict_entry dicts.typ_dict None (as_string tname)
              msg
              (ER.get_loc (get_rep tname))
          in
          pure
          @@ add_key_and_lib_address_to_dict dicts.typ_dict None
               (as_string tname) this_address
        in
        let%bind res_ctr_dict =
          let mk_msg cname =
            sprintf "Multiple declarations of type constructor %s in library"
              (as_error_string cname)
          in
          foldM ctrs ~init:dicts.ctr_dict
            ~f:(fun ctr_dict_acc (ctr : ctr_def) ->
              let ctr_name = ctr.cname in
              let msg = mk_msg ctr_name in
              let%bind () =
                check_duplicate_ns_dict_entry ctr_dict_acc None
                  (as_string ctr_name) msg
                  (ER.get_loc (get_rep ctr.cname))
              in
              pure
              @@ add_key_and_lib_address_to_dict ctr_dict_acc None
                   (as_string ctr_name) this_address)
        in
        let res_dicts =
          { dicts with typ_dict = res_typ_dict; ctr_dict = res_ctr_dict }
        in
        pure @@ (PostDisSyntax.LibTyp (dis_tname, dis_ctrs), res_dicts)

  let disambiguate_library (dicts : name_dicts) lib this_address =
    let { lname; lentries } = lib in
    let%bind dis_lname = name_def_as_simple_global lname in
    let%bind dis_lentries_rev, _ =
      foldM lentries ~init:([], dicts)
        ~f:(fun (dis_lentries_acc_rev, dicts_acc) lentry ->
          let%bind dis_lentry, new_dicts =
            disambiguate_lib_entry dicts_acc lentry this_address
          in
          pure (dis_lentry :: dis_lentries_acc_rev, new_dicts))
    in
    pure
    @@ {
         PostDisSyntax.lname = dis_lname;
         PostDisSyntax.lentries = List.rev dis_lentries_rev;
       }

  (**************************************************************)
  (*                 Disambiguate contracts                     *)
  (**************************************************************)

  let disambiguate_contract (dicts : name_dicts) c =
    let { cname; cparams; cconstraint; cfields; ccomps } = c in
    let%bind dis_cname = name_def_as_simple_global cname in
    let%bind dis_cparams =
      mapM cparams ~f:(fun (x, t) ->
          let%bind dis_x = name_def_as_simple_global x in
          let%bind dis_t = disambiguate_type dicts.typ_dict t in
          pure (dis_x, dis_t))
    in
    (* cparams are now in scope as locals, so remove from var dictionary *)
    let body_var_dict =
      List.fold_left cparams ~init:dicts.var_dict ~f:(fun dict (x, _) ->
          remove_local_id_from_dict dict (as_string x))
    in
    let body_dicts =
      {
        var_dict = body_var_dict;
        typ_dict = dicts.typ_dict;
        ctr_dict = dicts.ctr_dict;
      }
    in
    let%bind dis_cconstraint = disambiguate_exp body_dicts cconstraint in
    let%bind dis_cfields =
      mapM cfields ~f:(fun (fname, t, init) ->
          (* Field names are represented as simple names, since they are only
             exposed to other contracts via the remote read construct, which
             carries its own address reference. *)
          let%bind dis_fname = name_def_as_simple_global fname in
          let%bind dis_t = disambiguate_type dicts.typ_dict t in
          let%bind dis_init = disambiguate_exp body_dicts init in
          pure (dis_fname, dis_t, dis_init))
    in
    (* Do not remove fields from var dictionary, since they can never be
       confused with locals. *)
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

  (* Helper functions *)

  (* Find the address of an external library *)
  let find_lib_address libname init_extlibs_map =
    (* Use the address specified in init_extlibs_map, if it exists.
           If it does not, use the filename containing the library,
         which must be the library name itself. *)
    List.Assoc.find init_extlibs_map (as_string libname) ~equal:String.( = )
    |> Option.value ~default:(as_string libname)

  let find_lib lib_address (extlibs : PostDisSyntax.libtree list) =
    (* TODO: issue #867: We ought to be able to rely on extlib.libn.lname here *)
    match
      List.find extlibs ~f:(fun extlib ->
          String.(
            lib_address
            = strip_filename_extension
                (SR.get_loc (PostDisIdentifier.get_rep extlib.libn.lname)).fname))
    with
    | None -> fail0 ~kind:"Unrecognized library address" ~inst:lib_address
    | Some extlib -> pure extlib.libn

  let build_dict_for_lib lib_address lib =
    let open PostDisSyntax in
    let open PostDisIdentifier in
    foldM lib.lentries ~init:([], [], [])
      ~f:(fun (var_dict_acc, typ_dict_acc, ctr_dict_acc) lentry ->
        match lentry with
        | LibVar (x, _, _) ->
            (* x is a qualified name, so extract the simple name *)
            let unqualified_x = get_unqualified_name (get_id x) in
            (* Add unqualified_x -> lib_address to var dictionary *)
            let new_var_dict =
              add_key_address_to_dict var_dict_acc unqualified_x lib_address
            in
            pure (new_var_dict, typ_dict_acc, ctr_dict_acc)
        | LibTyp (tname, ctr_defs) ->
            (* tname is qualified, so extract the simple name. *)
            let unqualified_t = get_unqualified_name (get_id tname) in
            (* Add tname -> lib_address to type dictionary *)
            let new_typ_dict =
              add_key_address_to_dict typ_dict_acc unqualified_t lib_address
            in
            (* Deal with constructors *)
            let%bind new_ctr_dict =
              foldM ctr_defs ~init:ctr_dict_acc
                ~f:(fun dict_acc (ctr_def : ctr_def) ->
                  (* ctr_def.cname is qualified, so extract the simple name. *)
                  let unqualified_ctr =
                    get_unqualified_name (get_id ctr_def.cname)
                  in
                  (* Add ctr_def.cname -> lib_address to ctr dictionary *)
                  let new_ctr_dict =
                    add_key_address_to_dict dict_acc unqualified_ctr lib_address
                  in
                  pure new_ctr_dict)
            in
            (* Type and constructor dictionaries updated *)
            pure (var_dict_acc, new_typ_dict, new_ctr_dict))

  let get_dicts_for_ns ns_opt (var_dict, typ_dict, ctr_dict) =
    ( find_ns_dict var_dict ns_opt,
      find_ns_dict typ_dict ns_opt,
      find_ns_dict ctr_dict ns_opt )

  let add_dicts_for_ns ns_opt (old_var_dict, old_typ_dict, old_ctr_dict)
      (add_var_dict, add_typ_dict, add_ctr_dict) =
    let old_var_dict_for_ns, old_typ_dict_for_ns, old_ctr_dict_for_ns =
      get_dicts_for_ns ns_opt (old_var_dict, old_typ_dict, old_ctr_dict)
    in
    let new_var_dict_for_ns =
      combine_dicts ~old_dict:old_var_dict_for_ns ~new_dict:add_var_dict
    in
    let new_typ_dict_for_ns =
      combine_dicts ~old_dict:old_typ_dict_for_ns ~new_dict:add_typ_dict
    in
    let new_ctr_dict_for_ns =
      combine_dicts ~old_dict:old_ctr_dict_for_ns ~new_dict:add_ctr_dict
    in
    ( add_ns_nmdict_to_dict old_var_dict ns_opt new_var_dict_for_ns,
      add_ns_nmdict_to_dict old_typ_dict ns_opt new_typ_dict_for_ns,
      add_ns_nmdict_to_dict old_ctr_dict ns_opt new_ctr_dict_for_ns )

  let amend_imported_ns_dict lib lib_address ns_opt old_dicts error_loc =
    let old_var_dict, old_typ_dict, old_ctr_dict =
      get_dicts_for_ns ns_opt old_dicts
    in
    let%bind lib_var_nm_dict, lib_typ_nm_dict, lib_ctr_nm_dict =
      build_dict_for_lib lib_address lib
    in
    (* Each name must only be imported once.
       Check for duplicate names - disambiguation won't work otherwise.
       Only check against previous imports -
       duplicate names within the same library result in shadowing, so does not affect the importing module *)
    let mk_msg nm_category nm =
      sprintf "%s %s imported from multiple sources: libraries %s and"
        nm_category nm lib_address
    in
    let%bind () =
      forallM lib_var_nm_dict ~f:(fun (x, _) ->
          let msg = mk_msg "Variable" x in
          check_duplicate_dict_entry old_var_dict x msg error_loc)
    in
    let%bind () =
      forallM lib_typ_nm_dict ~f:(fun (t, _) ->
          let msg = mk_msg "Type" t in
          check_duplicate_dict_entry old_typ_dict t msg error_loc)
    in
    let%bind () =
      forallM lib_ctr_nm_dict ~f:(fun (c, _) ->
          let msg = mk_msg "Type constructor" c in
          check_duplicate_dict_entry old_ctr_dict c msg error_loc)
    in
    pure
    @@ add_dicts_for_ns ns_opt old_dicts
         (lib_var_nm_dict, lib_typ_nm_dict, lib_ctr_nm_dict)

  let amend_local_lib_dict lib lib_address old_dicts =
    let%bind lib_var_nm_dict, lib_typ_nm_dict, lib_ctr_nm_dict =
      build_dict_for_lib lib_address lib
    in
    (* Duplicate names shadow existing ones, so no check for duplicates needed *)
    pure
    @@ add_dicts_for_ns None old_dicts
         (lib_var_nm_dict, lib_typ_nm_dict, lib_ctr_nm_dict)

  let build_import_dicts imports (extlibs : PostDisSyntax.libtree list)
      init_extlibs_map =
    let%bind res_var_dict, res_typ_dict, res_ctr_dict =
      (* Build dictionaries *)
      foldM imports ~init:([], [], []) ~f:(fun acc_dicts (libname, ns_opt) ->
          let lib_address = find_lib_address libname init_extlibs_map in
          let%bind lib = find_lib lib_address extlibs in
          amend_imported_ns_dict lib lib_address
            (Option.map ns_opt ~f:as_string)
            acc_dicts
            (SR.get_loc (get_rep libname)))
    in
    pure (res_var_dict, res_typ_dict, res_ctr_dict)

  let disambiguate_cmodule cmod (extlibs : PostDisSyntax.libtree list)
      init_extlibs_map this_address =
    let { smver; libs; elibs; contr } = cmod in
    let%bind dis_elibs =
      mapM elibs ~f:(fun (lib, ns) ->
          let%bind dis_lib = name_def_as_simple_global lib in
          let%bind dis_ns = option_mapM ns ~f:name_def_as_simple_global in
          pure (dis_lib, dis_ns))
    in
    (* Ignore recursion principles, because their global names are their simple names.
       If a library defines a variable with the same name, then that definition shadows
       the generated recursion principle. *)
    (* Ignore name clashes with built-in datatypes and constructors.
       If such a name clash exists, then the recursion checker will catch them
       when checking the offending library. *)
    let%bind imp_var_dict, imp_typ_dict, imp_ctr_dict =
      build_import_dicts elibs extlibs init_extlibs_map
    in
    let imp_dicts =
      {
        var_dict = imp_var_dict;
        typ_dict = imp_typ_dict;
        ctr_dict = imp_ctr_dict;
      }
    in
    let%bind dis_libs =
      option_mapM libs ~f:(fun lib ->
          disambiguate_library imp_dicts lib this_address)
    in
    let%bind contract_dicts =
      option_value_mapM dis_libs
        ~f:(fun lib ->
          let%bind new_var_nm_dict, new_typ_nm_dict, new_ctr_nm_dict =
            amend_local_lib_dict lib this_address
              (imp_var_dict, imp_typ_dict, imp_ctr_dict)
          in
          pure
            {
              var_dict = new_var_nm_dict;
              typ_dict = new_typ_nm_dict;
              ctr_dict = new_ctr_nm_dict;
            })
        ~default:imp_dicts
    in
    let%bind dis_contr = disambiguate_contract contract_dicts contr in
    pure
    @@ {
         PostDisSyntax.smver;
         PostDisSyntax.libs = dis_libs;
         PostDisSyntax.elibs = dis_elibs;
         PostDisSyntax.contr = dis_contr;
       }

  let disambiguate_lmodule lmod (extlibs : PostDisSyntax.libtree list)
      init_extlibs_map this_address =
    let { smver; libs; elibs } = lmod in
    let%bind dis_elibs =
      mapM elibs ~f:(fun (lib, ns) ->
          let%bind dis_lib = name_def_as_simple_global lib in
          let%bind dis_ns = option_mapM ns ~f:name_def_as_simple_global in
          pure (dis_lib, dis_ns))
    in
    (* Ignore recursion principles, because their global names are their simple names.
       If a library defines a variable with the same name, then that definition shadows
       the generated recursion principle. *)
    (* Ignore name clashes with built-in datatypes and constructors.
       If such a name clash exists, then the recursion checker will catch them
       when checking the offending library. *)
    let%bind imp_var_dict, imp_typ_dict, imp_ctr_dict =
      build_import_dicts elibs extlibs init_extlibs_map
    in
    let imp_dicts =
      {
        var_dict = imp_var_dict;
        typ_dict = imp_typ_dict;
        ctr_dict = imp_ctr_dict;
      }
    in
    let%bind dis_libs = disambiguate_library imp_dicts libs this_address in
    pure
    @@ {
         PostDisSyntax.smver;
         PostDisSyntax.elibs = dis_elibs;
         PostDisSyntax.libs = dis_libs;
       }
end
