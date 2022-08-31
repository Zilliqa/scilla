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

open Core
open Scilla_base
open Syntax
open Literal

(** The ScillaProduct module provides the API to merge multiple contracts to a
    single "product" contract.

    This logic is implemented in two passes:
    1. Local pass: rename local identifiers and merge all to the product
       contract.
       We iterate through all components of a contract and rename all local
       identifiers that may be ambiguated. All rename information will be
       saved for the future remote rename. Then we merge all the components to
       the single product contract.
    2. Remote pass: rename remote identifiers and localize remote operations.
       We iterate through components of the product contract performing
       renaming and replacing all the remote operations to the local ones,
       defined in the product contract.

  TODO: Unimplemented features:
  * We need a way to detect what exactly the remote contract is used to avoid
    name collisions between smart contracts. For example, if have an address of
    a remote contract:
    ```
    | CollectionItemParam of ByStr20 with contract
    field spenders: Map Uint256 ByStr20,
    field token_owners: Map Uint256 ByStr20
    end Uint256 Uint32
    ```
    And later in the code we have this line:
    ```
    owner <- & token_address.token_owners[token_id];
    ```
    Suppose, that we merged multiple smart contracts that have a field
    `token_owners`. So, to disambiguate this, we need sort of config or
    user-defined annotations.
    Another solution could be interactive merging, when a tool requests to a
    user choose which contract is used in this place. In such case we'll need
    to change the representation of the [renames_map] to make it store sets of
    possible identifiers.
  * As an optimization we should not add equivalent definitions to the product
    contract.

  TODO: Tests:
  * Renaming types from libraries with `import as` *)

module ScillaProduct (SR : Rep) (ER : Rep) = struct
  module SER = SR
  module EER = ER
  module PLiteral = GlobalLiteral
  module PType = PLiteral.LType
  module PIdentifier = PType.TIdentifier
  module PSyntax = ScillaSyntax (SR) (ER) (PLiteral)

  module PIdentifierComp = struct
    include PIdentifier.Name
    include Comparable.Make (PIdentifier.Name)
  end

  module PIdentifierSet = Set.Make (PIdentifierComp)
  open PSyntax

  let emp_ids_map = Map.empty (module PIdentifierComp)
  let disambiguate_warning_level = 2

  (************************************************)
  (** UTILITIES                                   *)
  (************************************************)

  let mk_simple_id name =
    SIdentifier.mk_id (SIdentifier.Name.parse_simple_name name) SR.dummy_rep

  let product_lib_name = mk_simple_id "ProductLib"
  let product_contr_name = mk_simple_id "ProductContr"

  let get_lib_entry_id = function
    | LibTyp (id, _) | LibVar (id, _, _) -> PIdentifier.get_id id

  let lib_entries_to_map lentries =
    List.fold_left ~init:emp_ids_map lentries ~f:(fun m rlib ->
        Map.set m ~key:(get_lib_entry_id rlib) ~data:rlib)

  let add_rep id id' = PIdentifier.mk_id id' (PIdentifier.get_rep id)
  let add_loc id = PIdentifier.mk_loc_id id
  let find_id renames_map id = Map.find renames_map (SIdentifier.get_id id)

  (************************************************)
  (* LOCAL PASS                                   *)
  (************************************************)
  (* Present with two steps: renaming (rename_* functions) and merging to a
     product (extend_* functions). *)

  (** Adds a unique address to the [name]. *)
  let qualify_name captitalize prefix
      (((name : Identifier.GlobalName.t_name), i) : 'a) : PIdentifier.Name.t =
    let open Identifier in
    let gen_name =
      Printf.sprintf "%s_%s"
        (if captitalize then String.capitalize prefix
        else String.uncapitalize prefix)
    in
    ( (match name with
      | GlobalName.SimpleGlobal n -> GlobalName.SimpleGlobal (gen_name n)
      | GlobalName.QualifiedGlobal (a, n) ->
          GlobalName.QualifiedGlobal (a, gen_name n)),
      gen_name i )

  (** Sets the unique for the identifier [rep_id]. *)
  let qualify_id ?(capitalize = false) renames_map prefix
      (rep_id : 'a SIdentifier.t) =
    let name = PIdentifier.get_id rep_id in
    match find_id renames_map rep_id with
    | Some candidates ->
        let new_name = qualify_name capitalize prefix name in
        ( add_rep rep_id new_name,
          Map.set renames_map ~key:name
            ~data:(PIdentifierSet.add candidates new_name) )
    | None ->
        let new_name = qualify_name capitalize prefix name in
        ( add_rep rep_id new_name,
          Map.set renames_map ~key:name
            ~data:(PIdentifierSet.singleton new_name) )

  (** Generates a name to qualify an identifier. If there are a few possible
      candidates for this name, emits a warning. *)
  let choose_name candidates name loc =
    if phys_equal 1 @@ Set.length candidates then Set.min_elt_exn candidates
    else (
      ErrorUtils.warn1
        (Printf.sprintf
           "Name collision: Please disambiguate `%s` in the configuration file"
           (Lazy.force name))
        disambiguate_warning_level (Lazy.force loc);
      Set.to_list candidates
      |> List.sort ~compare:PIdentifierComp.compare
      |> List.fold_left ~init:[] ~f:(fun acc l ->
             acc @ [ PIdentifier.Name.as_string l ])
      |> String.concat ~sep:"|" |> Printf.sprintf "(%s)"
      |> SIdentifier.Name.parse_simple_name)

  let rename_id_er renames_map id =
    match find_id renames_map id with
    | Some candidates ->
        choose_name candidates
          (lazy (PIdentifier.Name.as_string (PIdentifier.get_id id)))
          (lazy (ER.get_loc (PIdentifier.get_rep id)))
        |> add_rep id
    | None -> id

  let rename_id_sr renames_map id =
    match find_id renames_map id with
    | Some candidates ->
        choose_name candidates
          (lazy (PIdentifier.Name.as_string (PIdentifier.get_id id)))
          (lazy (SR.get_loc (PIdentifier.get_rep id)))
        |> add_rep id
    | None -> id

  let rename_id_loc renames_map id =
    match find_id renames_map id with
    | Some candidates ->
        choose_name candidates
          (lazy (PIdentifier.Name.as_string (PIdentifier.get_id id)))
          (lazy (PIdentifier.get_rep id))
        |> add_loc
    | None -> id

  (** Renames user-defined ADTs from [renames_map]. *)
  let rec rename_ty renames_map (ty : SType.t) : SType.t =
    match ty with
    | MapType (key_ty, val_ty) ->
        MapType (rename_ty renames_map key_ty, rename_ty renames_map val_ty)
    | FunType (arg_ty, ret_ty) ->
        FunType (rename_ty renames_map arg_ty, rename_ty renames_map ret_ty)
    | ADT (id, tys) ->
        ADT
          ( rename_id_loc renames_map id,
            List.map tys ~f:(fun ty -> rename_ty renames_map ty) )
    | PolyFun (tyvar, bt) -> SType.PolyFun (tyvar, rename_ty renames_map bt)
    | TypeVar _ | Unit | PrimType _ | Address _ -> ty

  (** Sets unique names for the user-defined ADTs in [ty]. *)
  let rec qualify_ty renames_map contract_name (ty : SType.t) =
    let open SType in
    match ty with
    | MapType (key_ty, val_ty) ->
        let key_ty', renames_map' =
          qualify_ty renames_map contract_name key_ty
        in
        let val_ty', renames_map' =
          qualify_ty renames_map' contract_name val_ty
        in
        (MapType (key_ty', val_ty'), renames_map')
    | FunType (arg_ty, ret_ty) ->
        let arg_ty', renames_map' =
          qualify_ty renames_map contract_name arg_ty
        in
        let ret_ty', renames_map' =
          qualify_ty renames_map' contract_name ret_ty
        in
        (FunType (arg_ty', ret_ty'), renames_map')
    | ADT (id, tys) -> (
        let name = PIdentifier.get_id id in
        match Map.find renames_map name with
        | Some _ ->
            let id' = rename_id_loc renames_map id in
            let tys' = List.map tys ~f:(fun ty -> rename_ty renames_map ty) in
            (ADT (id', tys'), renames_map)
        | None ->
            let name', renames_map' =
              (* Don't qualify built-in ADTs. *)
              Datatypes.DataTypeDictionary.lookup_name name |> function
              | Ok _ -> (id, renames_map)
              | Error _ ->
                  qualify_id ~capitalize:true renames_map contract_name id
            in
            let tys' = List.map tys ~f:(fun ty -> rename_ty renames_map' ty) in
            (ADT (name', tys'), renames_map'))
    | PolyFun (tyvar, bt) ->
        let bt', renames_map' = qualify_ty renames_map contract_name bt in
        (SType.PolyFun (tyvar, bt'), renames_map')
    | TypeVar _ | Unit | PrimType _ | Address _ -> (ty, renames_map)

  let rec rename_pattern renames_map = function
    | Wildcard -> Wildcard
    | Binder id -> Binder (rename_id_er renames_map id)
    | Constructor (id, patterns) ->
        let id' = rename_id_sr renames_map id in
        let patterns' =
          List.map patterns ~f:(fun p -> rename_pattern renames_map p)
        in
        Constructor (id', patterns')

  let rec rename_lit renames_map lit (annot : ER.rep) =
    let open PLiteral in
    match lit with
    | Msg vals ->
        Msg
          (List.map vals ~f:(fun (tag, ty, lit) ->
               (tag, rename_ty renames_map ty, rename_lit renames_map lit annot)))
    | Map ((key_ty, val_ty), ls) ->
        let key_ty' = rename_ty renames_map key_ty in
        let val_ty' = rename_ty renames_map val_ty in
        let ls' = Caml.Hashtbl.create (Caml.Hashtbl.length ls) in
        let _ =
          Caml.Hashtbl.iter
            (fun k v ->
              let k' = rename_lit renames_map k annot in
              let v' = rename_lit renames_map v annot in
              Caml.Hashtbl.add ls' k' v')
            ls
        in
        Map ((key_ty', val_ty'), ls')
    | ADTValue (name, tys, lits) ->
        let id' =
          match Map.find renames_map name with
          | Some candidates ->
              choose_name candidates
                (PIdentifier.Name.as_string name |> Lazy.from_val)
                (lazy (ER.get_loc annot))
          | None -> name
        in
        let tys' = List.map tys ~f:(fun ty -> rename_ty renames_map ty) in
        let lits' =
          List.map lits ~f:(fun lit -> rename_lit renames_map lit annot)
        in
        ADTValue (id', tys', lits')
    (* TODO: Check closures and type abstractions. *)
    | StringLit _ | IntLit _ | UintLit _ | BNum _ | ByStrX _ | ByStr _ | Clo _
    | TAbs _ ->
        lit

  let rec rename_expr renames_map (e, annot) =
    match e with
    | Literal lit -> (Literal (rename_lit renames_map lit annot), annot)
    | Var id -> (Var (rename_id_er renames_map id), annot)
    | Let (id, ty_opt, lhs, rhs) ->
        let id' = rename_id_er renames_map id in
        let ty_opt' =
          Option.value_map ty_opt ~default:None ~f:(fun ty ->
              rename_ty renames_map ty |> Option.some)
        in
        let lhs' = rename_expr renames_map lhs in
        let rhs' = rename_expr renames_map rhs in
        (Let (id', ty_opt', lhs', rhs'), annot)
    | Message msg ->
        List.map msg ~f:(fun (tag_name, payload) ->
            let payload' =
              match payload with
              | MLit lit -> MLit lit
              | MVar id -> MVar (rename_id_er renames_map id)
            in
            (tag_name, payload'))
        |> fun msg' -> (Message msg', annot)
    | Fun (id, ty, body) ->
        let id' = rename_id_er renames_map id in
        let ty' = rename_ty renames_map ty in
        let body' = rename_expr renames_map body in
        (Fun (id', ty', body'), annot)
    | App (fun_id, args) ->
        let fun_id' = rename_id_er renames_map fun_id in
        let args' =
          List.map args ~f:(fun arg -> rename_id_er renames_map arg)
        in
        (App (fun_id', args'), annot)
    | Constr (id, type_params, params) ->
        let id' = rename_id_sr renames_map id in
        let type_params' =
          List.map type_params ~f:(fun tp -> rename_ty renames_map tp)
        in
        let params' =
          List.map params ~f:(fun p -> rename_id_er renames_map p)
        in
        (Constr (id', type_params', params'), annot)
    | MatchExpr (id, patterns) ->
        let id' = rename_id_er renames_map id in
        let patterns' =
          List.map patterns ~f:(fun (pat, ea) ->
              let pat' = rename_pattern renames_map pat in
              let ea' = rename_expr renames_map ea in
              (pat', ea'))
        in
        (MatchExpr (id', patterns'), annot)
    | Builtin (builtin, tys, params) ->
        let tys' = List.map tys ~f:(fun ty -> rename_ty renames_map ty) in
        let params' =
          List.map params ~f:(fun p -> rename_id_er renames_map p)
        in
        (Builtin (builtin, tys', params'), annot)
    | TFun (id, body) ->
        let id' = rename_id_er renames_map id in
        let body' = rename_expr renames_map body in
        (TFun (id', body'), annot)
    | TApp (id, tys) ->
        let id' = rename_id_er renames_map id in
        let tys' = List.map tys ~f:(fun ty -> rename_ty renames_map ty) in
        (TApp (id', tys'), annot)
    | Fixpoint (id, ty, body) ->
        let id' = rename_id_er renames_map id in
        let ty' = rename_ty renames_map ty in
        let body' = rename_expr renames_map body in
        (Fixpoint (id', ty', body'), annot)
    | GasExpr (charge, body) ->
        let body' = rename_expr renames_map body in
        (GasExpr (charge, body'), annot)

  let rec rename_stmt renames_map (stmt, annot) =
    match stmt with
    | Bind (id, expr) ->
        let id' = rename_id_er renames_map id in
        (Bind (id', rename_expr renames_map expr), annot)
    | CallProc (id, args) ->
        let id' = rename_id_sr renames_map id in
        let args' = List.map args ~f:(fun a -> rename_id_er renames_map a) in
        (CallProc (id', args'), annot)
    | Iterate (list, id) ->
        let id' = rename_id_sr renames_map id in
        let list' = rename_id_er renames_map list in
        (Iterate (list', id'), annot)
    | MatchStmt (id, arms) ->
        let id' = rename_id_er renames_map id in
        let arms' =
          List.map arms ~f:(fun (pattern, stmts) ->
              let pattern' = rename_pattern renames_map pattern in
              let stmts' =
                List.fold_left stmts ~init:[] ~f:(fun acc sa ->
                    acc @ [ rename_stmt renames_map sa ])
              in
              (pattern', stmts'))
        in
        (MatchStmt (id', arms'), annot)
    | MapUpdate (m, keys, v_opt) ->
        let m' = rename_id_er renames_map m in
        let keys' = List.map keys ~f:(fun k -> rename_id_er renames_map k) in
        let v_opt' =
          Option.value_map v_opt ~default:None ~f:(fun v ->
              Some (rename_id_er renames_map v))
        in
        (MapUpdate (m', keys', v_opt'), annot)
    | MapGet (v, m, keys, exists) ->
        let v' = rename_id_er renames_map v in
        let m' = rename_id_er renames_map m in
        let keys' = List.map keys ~f:(fun k -> rename_id_er renames_map k) in
        (MapGet (v', m', keys', exists), annot)
    | RemoteMapGet (v, adr, m, keys, exists) ->
        (* Map will be replaced to the local one in the Remote pass. *)
        let v' = rename_id_er renames_map v in
        let keys' = List.map keys ~f:(fun k -> rename_id_er renames_map k) in
        (RemoteMapGet (v', adr, m, keys', exists), annot)
    | Load (lhs, rhs) ->
        let lhs' = rename_id_er renames_map lhs in
        let rhs' = rename_id_er renames_map rhs in
        (Load (lhs', rhs'), annot)
    | RemoteLoad (lhs, adr, rhs) ->
        (* Address will be replaced in the Remote pass. *)
        let lhs' = rename_id_er renames_map lhs in
        let rhs' = rename_id_er renames_map rhs in
        (RemoteLoad (lhs', adr, rhs'), annot)
    | Store (lhs, rhs) ->
        let lhs' = rename_id_er renames_map lhs in
        let rhs' = rename_id_er renames_map rhs in
        (Store (lhs', rhs'), annot)
    | TypeCast (lhs, rhs, ty) ->
        let lhs' = rename_id_er renames_map lhs in
        let rhs' = rename_id_er renames_map rhs in
        let ty' = rename_ty renames_map ty in
        (TypeCast (lhs', rhs', ty'), annot)
    | ReadFromBC (id, q) ->
        let id' = rename_id_er renames_map id in
        (ReadFromBC (id', q), annot)
    | AcceptPayment | SendMsgs _ | CreateEvnt _ | Throw _ | GasStmt _ ->
        (stmt, annot)

  let rename_lentry renames_map contract_name = function
    | LibVar (name, ty_annot, body) ->
        let name', renames_map = qualify_id renames_map contract_name name in
        let body' = rename_expr renames_map body in
        let ty_annot' =
          Option.value_map ty_annot ~default:None ~f:(fun ty ->
              Some (rename_ty renames_map ty))
        in
        (LibVar (name', ty_annot', body'), renames_map)
    | LibTyp (id, ctrs) ->
        let id', renames_map =
          qualify_id ~capitalize:true renames_map contract_name id
        in
        let adt_prefix =
          PIdentifier.as_string id
          |> String.map ~f:(fun c -> if phys_equal c '.' then '_' else c)
        in
        let ctrs', renames_map =
          List.fold_left ctrs ~init:([], renames_map)
            ~f:(fun (acc_ctrs, m) ctr_def ->
              let cname, m =
                qualify_id ~capitalize:true m adt_prefix ctr_def.cname
              in
              let c_arg_types, m =
                List.fold_left ~init:([], m) ctr_def.c_arg_types
                  ~f:(fun (acc, m) ty ->
                    let ty', m = qualify_ty m contract_name ty in
                    (acc @ [ ty' ], m))
              in
              (acc_ctrs @ [ { cname; c_arg_types } ], m))
        in
        (LibTyp (id', ctrs'), renames_map)

  let rename_lib renames_map contract_name (lib : library) =
    let lentries, renames_map' =
      List.fold_left lib.lentries ~init:([], renames_map)
        ~f:(fun (acc_lentries, m) lentry ->
          let lentry', m = rename_lentry m contract_name lentry in
          (acc_lentries @ [ lentry' ], m))
    in
    ({ lib with lentries }, renames_map')

  (** Renames named imports in [elibs].
      For example, `import Foo as Bar` will be renamed to `import Foo as
      Contr1_Bar`. *)
  let rename_elibs renames_map elibs =
    List.map elibs ~f:(fun (name, import_as_opt) ->
        match import_as_opt with
        | Some import_as ->
            let import_as' = rename_id_sr renames_map import_as in
            (name, Some import_as')
        | None -> (name, import_as_opt))

  let rename_params renames_map (params : ('a SIdentifier.t * SType.t) list) =
    List.map params ~f:(fun (name, ty) ->
        let name' = rename_id_er renames_map name in
        let ty' = rename_ty renames_map ty in
        (name', ty'))

  let qualify_params renames_map contract_name
      (params : ('a SIdentifier.t * SType.t) list) =
    (* Contract parameters must be qualified, because they could have
       different types. TODO: To optimize this, we should not rename params
       with the same type and name. *)
    List.fold_left params ~init:([], renames_map)
      ~f:(fun (acc, renames_map) (name, ty) ->
        let name', renames_map = qualify_id renames_map contract_name name in
        let ty' = rename_ty renames_map ty in
        (acc @ [ (name', ty') ], renames_map))

  let qualify_fields renames_map contract_name fields =
    (* Contract fields must be qualified, because they could have different
        types. TODO: To optimize this, we should not rename fields with the
        same type and name. *)
    List.fold_left fields ~init:([], renames_map)
      ~f:(fun (acc, renames_map) (name, ty, init_expr) ->
        let name', renames_map = qualify_id renames_map contract_name name in
        let ty' = rename_ty renames_map ty in
        let init_expr' = rename_expr renames_map init_expr in
        (acc @ [ (name', ty', init_expr') ], renames_map))

  let rename_component renames_map contract_name component =
    let comp_name, renames_map =
      qualify_id renames_map contract_name component.comp_name
    in
    let comp_params = rename_params renames_map component.comp_params in
    let comp_body =
      List.map component.comp_body ~f:(fun stmt_annot ->
          rename_stmt renames_map stmt_annot)
    in
    ({ component with comp_name; comp_params; comp_body }, renames_map)

  let rename_components renames_map contract_name components =
    let components', renames_map' =
      List.fold_left components ~init:([], renames_map)
        ~f:(fun (acc_components, m) component ->
          let component', m = rename_component m contract_name component in
          (acc_components @ [ component' ], m))
    in
    (components' |> List.rev, renames_map')

  let rename_contr renames_map (contract_name : string) contr =
    let cparams, renames_map =
      qualify_params renames_map contract_name contr.cparams
    in
    let cconstraint = rename_expr renames_map contr.cconstraint in
    let cfields, renames_map =
      qualify_fields renames_map contract_name contr.cfields
    in
    let ccomps, renames_map =
      rename_components renames_map contract_name contr.ccomps
    in
    ({ contr with cparams; cconstraint; cfields; ccomps }, renames_map)

  (** Renames local identifiers in [cmod] and saves rename information to
      [renames_map]. *)
  let rename_cmod renames_map (cmod : cmodule) =
    let contract_name =
      SIdentifier.Name.as_string (SIdentifier.get_id cmod.contr.cname)
    in
    let libs, renames_map =
      Option.value_map cmod.libs ~default:(None, renames_map) ~f:(fun lib ->
          let lib, renames_map = rename_lib renames_map contract_name lib in
          (Some lib, renames_map))
    in
    let elibs = rename_elibs renames_map cmod.elibs in
    let contr, renames_map =
      rename_contr renames_map contract_name cmod.contr
    in
    ({ cmod with libs; elibs; contr }, renames_map)

  (** Extends the functional contract library [lib] with definitions from the
      library [ext_lib]. *)
  let extend_lib lib_opt ext_lib_opt =
    match (lib_opt, ext_lib_opt) with
    | None, Some _ -> ext_lib_opt
    | Some _, None -> lib_opt
    | Some lib, Some ext_lib ->
        let lentries =
          List.fold_left ~init:lib.lentries ext_lib.lentries
            ~f:(fun acc lentry -> acc @ [ lentry ])
        in
        Some { lname = product_lib_name; lentries }
    | None, None -> None

  (** Extends contract components [constraint] with [ext_constraint]. *)
  let extend_contract_constraint (constr : expr_annot)
      (_ext_constr : expr_annot) =
    (* TODO: We should create a chain of let bindings. Something like:
         let cond1 = <expr1> in
         let cond2 = <expr2> in
         let cond3 = <expr3> in
       We use ANF form, so we will combine them by this way:
         let cond1_2 = builtin_band cond1 cond2 in
         let cond1_2_3 = builtin_band cond1_2 cond3 in
         cond1_2_3
       For now I'd like to keep just the first expression, because there is
       only one such case on the marketplace contracts. *)
    constr

  let extend_contract c ext_c =
    {
      cname = product_contr_name;
      cparams = c.cparams @ ext_c.cparams;
      cconstraint = extend_contract_constraint c.cconstraint ext_c.cconstraint;
      ccomps = c.ccomps @ ext_c.ccomps;
      cfields = c.cfields @ ext_c.cfields;
    }

  (** Extends [cmod] with definitions from module [ext_cmod]. *)
  let extend_cmod (cmod : cmodule) (ext_cmod : cmodule) =
    (* TODO: Check for incompatible smver *)
    let libs = extend_lib cmod.libs ext_cmod.libs in
    let elibs =
      cmod.elibs @ ext_cmod.elibs
      |> List.dedup_and_sort
           ~compare:(fun (lhs_import, lhs_import_as) (rhs_import, rhs_import_as)
                    ->
             let v = SIdentifier.compare lhs_import rhs_import in
             match (lhs_import_as, rhs_import_as) with
             | Some lhs_as, Some rhs_as -> SIdentifier.compare lhs_as rhs_as + v
             | None, None -> v
             | None, Some _ -> v + 1
             | Some _, None -> 1 - v)
    in
    let contr = extend_contract cmod.contr ext_cmod.contr in
    { cmod with libs; elibs; contr }

  (** Extends [rlibs] with imported libraries [extenstion_rlibs]. *)
  let extend_rlibs rlibs_map (extenstion_rlibs : lib_entry list) =
    List.fold_left extenstion_rlibs ~init:rlibs_map ~f:(fun m rlib ->
        Map.set m ~key:(get_lib_entry_id rlib) ~data:rlib)

  let run_local_pass
      (contract_infos : (cmodule * lib_entry list * libtree list) list) =
    List.fold_left contract_infos ~init:(None, emp_ids_map)
      ~f:(fun (acc, renames_map) (cmod, rlibs, _elibs) ->
        match acc with
        | Some (prod_cmod, rlibs_map) ->
            let cmod', renames_map = rename_cmod renames_map cmod in
            let prod_cmod = extend_cmod prod_cmod cmod' in
            let prod_rlibs = extend_rlibs rlibs_map rlibs in
            (Some (prod_cmod, prod_rlibs), renames_map)
        | None ->
            let cmod', renames_map = rename_cmod renames_map cmod in
            (Some (cmod', lib_entries_to_map rlibs), renames_map))
    |> fun (result, renames_map) ->
    Option.value_map result ~default:None ~f:(fun (cmod, rlibs_map) ->
        Some (cmod, Map.data rlibs_map, renames_map))

  (************************************************)
  (* REMOTE PASS                                  *)
  (************************************************)
  (* localize_* functions replace remote operations with the local ones *)

  (** Generates a name to qualify an identifier. If there are a few possible
      candidates for this name, emits a warning. *)
  let remote_rename candidates name loc =
    if phys_equal 1 @@ Set.length candidates then Set.min_elt_exn candidates
    else (
      ErrorUtils.warn1
        (Printf.sprintf
           "Name collision: Please disambiguate `%s` in the configuration file \
            or set the name in the generated product contract."
           (Lazy.force name))
        disambiguate_warning_level (Lazy.force loc);
      Set.to_list candidates
      |> List.sort ~compare:PIdentifierComp.compare
      |> List.fold_left ~init:"" ~f:(fun acc l ->
             acc ^ "|" ^ PIdentifier.Name.as_string l)
      |> Printf.sprintf "(%s)" |> SIdentifier.Name.parse_simple_name)

  let remote_rename_er renames_map id =
    match find_id renames_map id with
    | Some candidates ->
        choose_name candidates
          (lazy (PIdentifier.Name.as_string (PIdentifier.get_id id)))
          (lazy (ER.get_loc (PIdentifier.get_rep id)))
        |> add_rep id
    | None -> id

  let rec localize_stmt renames_map (stmt, annot) =
    match stmt with
    | RemoteLoad (l, _, v) ->
        let v' = remote_rename_er renames_map v in
        (Load (l, v'), annot)
    | RemoteMapGet (l, _, m, keys, exists) ->
        let m' = remote_rename_er renames_map m in
        (MapGet (l, m', keys, exists), annot)
    | MatchStmt (id, arms) ->
        let arms' =
          List.map arms ~f:(fun (pat, stmts) ->
              let stmts' =
                List.map stmts ~f:(fun stmt -> localize_stmt renames_map stmt)
              in
              (pat, stmts'))
        in
        (MatchStmt (id, arms'), annot)
    | TypeCast (id, _addr, _typ) ->
        let id' = remote_rename_er renames_map id in
        let body =
          let some =
            PIdentifier.mk_id
              (PType.TIdentifier.Name.parse_simple_name "Some")
              SR.dummy_rep
          in
          let bystr20 = PType.PrimType (Type.PrimType.Bystrx_typ 20) in
          let this_address =
            PIdentifier.mk_id
              (PType.TIdentifier.Name.parse_simple_name "_this_address")
              ER.dummy_rep
          in
          (Constr (some, [ bystr20 ], [ this_address ]), ER.dummy_rep)
        in
        (Bind (id', body), annot)
    | Load _ | Store _ | Bind _ | MapUpdate _ | MapGet _ | ReadFromBC _
    | AcceptPayment | Iterate _ | SendMsgs _ | CreateEvnt _ | CallProc _
    | Throw _ | GasStmt _ ->
        (stmt, annot)

  let localize_comp renames_map comp =
    let comp_body =
      List.map comp.comp_body ~f:(fun stmt -> localize_stmt renames_map stmt)
    in
    { comp with comp_body }

  let localize_cmod renames_map cmod =
    let ccomps =
      List.map cmod.contr.ccomps ~f:(fun comp -> localize_comp renames_map comp)
    in
    let contr = { cmod.contr with ccomps } in
    { cmod with contr }

  let run_remote_pass product_cmod product_rlib renames_map =
    let product_cmod' = localize_cmod renames_map product_cmod in
    (product_cmod', product_rlib)

  (************************************************)
  (* ENTRY POINT                                  *)
  (************************************************)

  let run (contract_infos : (cmodule * lib_entry list * libtree list) list) =
    run_local_pass contract_infos
    |> Option.value_map ~default:None
         ~f:(fun (product_cmod, product_rlib, renames_map) ->
           let product_cmod', product_rlib' =
             run_remote_pass product_cmod product_rlib renames_map
           in
           Some (product_cmod', product_rlib'))
end
