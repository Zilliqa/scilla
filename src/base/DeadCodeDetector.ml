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

(**
  DeadCodeDetector module checks for unused:
  * Procedures and their parameters
  * Mutable Fields
  * Immutable contract parameters
  * Pattern-matching binders and arms
  * Library functions (and their parameters) and types
  * Let bindings
  * Library imports
  * User-defined ADTs and their constructors
  * Fields in contract address types

  An ADT is considered as used if it one of:
  * Occurs in type annotations of the source code elements after typechecking
    is finished.
  * Used in constructors of other user-defined ADTs
  * Used as an argument of a type function
  Otherwise, it will be considered as unused and reported.

  A constructor of any used ADT is considered as used if it one of:
  * Instantiated in the source code of the contract or of its library
  * Is a part of an ADT that is used as a parameter of a transition
  Otherwise, it will be reported as unused and reported.
*)

open Core
open ErrorUtils
open Syntax
open Literal

(* ************************************** *)
(* ******** Dead Code Detector ********** *)
(* ************************************** *)

module DeadCodeDetector (SR : Rep) (ER : Rep) = struct
  module SCLiteral = GlobalLiteral
  module SCType = SCLiteral.LType
  module SCIdentifier = SCType.TIdentifier

  module SCIdentifierComp = struct
    include SCIdentifier.Name
    include Comparable.Make (SCIdentifier.Name)
  end

  module ERComp = struct
    module T = struct
      include ER

      type t = rep SCIdentifier.t [@@deriving sexp]

      let compare (a : t) (b : t) =
        SCIdentifier.Name.compare (SCIdentifier.get_id a)
          (SCIdentifier.get_id b)
    end

    include T
    include Comparable.Make (T)
  end

  module SCIdentifierSet = Set.Make (SCIdentifierComp)
  module ERSet = Set.Make (ERComp)
  module SCSyntax = ScillaSyntax (SR) (ER) (SCLiteral)
  module SCU = ContractUtil.ScillaContractUtil (SR) (ER)
  open SCIdentifier
  open SCSyntax

  let emp_idset = SCIdentifierSet.empty
  let emp_erset = ERSet.empty
  let emp_idsmap = Map.empty (module SCIdentifierComp)

  (** Warning level for dead code detection *)
  let warning_level_dead_code = 3

  (** Raises a warning *)
  let warn warn_msg name get_loc =
    warn1
      (warn_msg ^ as_error_string name)
      warning_level_dead_code
      (get_loc (get_rep name))

  (** Keeps a mutable state to track read, write and unused fields of a
      cmodule. *)
  module FieldsState = struct
    type fs = {
      cfields : ER.rep t list;
      mutable read_fields : ERSet.t;
      mutable write_fields : ERSet.t;
    }

    let mk cmod =
      {
        cfields = List.map cmod.contr.cfields ~f:(fun (f', _, _) -> f');
        read_fields = ERSet.empty;
        write_fields = ERSet.empty;
      }

    let mark_field_read fs field =
      if SCIdentifier.is_mem_id field fs.cfields then
        fs.read_fields <- ERSet.add fs.read_fields field

    let mark_field_write fs field =
      if SCIdentifier.is_mem_id field fs.cfields then
        fs.write_fields <- ERSet.add fs.write_fields field

    let report_read_only fs =
      fs.read_fields
      |> ERSet.filter ~f:(fun f -> not @@ ERSet.mem fs.write_fields f)
      |> ERSet.iter ~f:(fun field ->
             warn "Read only field, consider making it a contract parameter: "
               field ER.get_loc)

    let report_write_only fs =
      fs.write_fields
      |> ERSet.filter ~f:(fun f -> not @@ ERSet.mem fs.read_fields f)
      |> ERSet.iter ~f:(fun field -> warn "Write only field: " field ER.get_loc)

    let report_unused (fs : fs) =
      List.filter fs.cfields ~f:(fun f ->
          (not @@ ERSet.mem fs.read_fields f)
          && (not @@ ERSet.mem fs.write_fields f))
      |> List.iter ~f:(fun f -> warn "Unused field: " f ER.get_loc)
  end

  (** Returns a set of names of ADTs as Name from a type *)
  let rec user_types_in_adt (tys : SType.t list) =
    let rec iden_iter ty acc =
      match ty with
      | SCType.ADT (iden, targs) ->
          user_types_in_adt targs |> SCIdentifierSet.union acc |> fun s ->
          SCIdentifierSet.add s (SCIdentifier.get_id iden)
      | SCType.MapType (ty1, ty2) | SCType.FunType (ty1, ty2) ->
          SCIdentifierSet.union (iden_iter ty1 emp_idset)
            (iden_iter ty2 emp_idset)
      | _ -> emp_idset
    in
    List.fold_left tys ~init:emp_idset ~f:(fun acc ty ->
        iden_iter ty emp_idset |> Set.union acc)

  (** Returns used names of ADT types and ADT constructors *)
  let user_types_in_literal lits =
    let rec aux lits =
      List.fold_left lits ~init:(emp_idset, emp_idset)
        ~f:(fun (acc_adts, acc_ctrs) lit ->
          match lit with
          | SLiteral.Map ((ty1, ty2), _) ->
              user_types_in_adt [ ty1 ]
              |> SCIdentifierSet.union @@ user_types_in_adt [ ty2 ]
              |> SCIdentifierSet.union acc_adts
              |> fun adts -> (adts, acc_ctrs)
          | SLiteral.ADTValue (ctr, tys, ts) ->
              let ts_adts, ts_ctrs = aux ts in
              user_types_in_adt tys
              |> SCIdentifierSet.union ts_adts
              |> SCIdentifierSet.union acc_adts
              |> fun adts ->
              ( adts,
                SCIdentifierSet.add acc_ctrs ctr
                |> SCIdentifierSet.union ts_ctrs )
          | _ -> (emp_idset, emp_idset))
    in
    aux lits

  (** Returns user names of ADT and constructors occurred in the constructor
      definition. *)
  let user_types_in_ctr ctr_def =
    List.fold_left ctr_def.c_arg_types ~init:SCIdentifierSet.empty
      ~f:(fun acc ty -> user_types_in_adt [ ty ] |> SCIdentifierSet.union acc)

  let user_types_in_ctrs ctr_defs =
    List.fold_left ctr_defs ~init:SCIdentifierSet.empty ~f:(fun acc ctr_def ->
        user_types_in_ctr ctr_def |> Set.union acc)

  let ctr_names (ctr_defs : ctr_def list) =
    List.fold_left ctr_defs ~init:[] ~f:(fun acc ctr_def ->
        [ SCIdentifier.get_id ctr_def.cname ] |> List.append acc)

  let proc_dict = ref []

  (** Collects a mapping from ADTs to their constructor definitions. *)
  let collect_adts_to_ctrs lentries =
    List.fold_left lentries ~init:emp_idsmap ~f:(fun m lentry ->
        match lentry with
        | LibTyp (id, ctr_defs) ->
            Map.set m ~key:(SCIdentifier.get_id id) ~data:ctr_defs
        | LibVar _ -> m)

  (** Returns a set of ADT and constructor names used in constructors other of
      user-defined ADTs. *)
  let collect_adts_in_adts adts_to_ctrs =
    Map.data adts_to_ctrs
    |> List.fold_left ~init:SCIdentifierSet.empty ~f:(fun acc ctr_defs ->
           user_types_in_ctrs ctr_defs |> SCIdentifierSet.union acc)

  (** Returns a set of ADT and constructor names transitively used in
      transition parameters. *)
  let collect_adts_in_params adts_to_ctrs param_adts =
    let rec aux adt =
      Map.find adts_to_ctrs adt
      |> Option.fold ~init:emp_idset ~f:(fun acc ctr_defs ->
             user_types_in_ctrs ctr_defs
             |> (fun adts ->
                  Set.fold adts ~init:adts ~f:(fun acc adt ->
                      Map.find adts_to_ctrs adt
                      |> Option.fold ~init:[] ~f:(fun acc ctr_defs ->
                             ctr_names ctr_defs |> List.append acc)
                      |> SCIdentifierSet.of_list |> SCIdentifierSet.union acc))
             |> Set.fold ~init:acc ~f:(fun acc adt ->
                    SCIdentifierSet.add acc adt
                    |> SCIdentifierSet.union @@ aux adt))
    in
    Set.fold param_adts ~init:emp_idset ~f:(fun acc adt ->
        aux adt |> SCIdentifierSet.union acc)

  (** Shows warnings for pattern-matching arms that check for unused
      user-defined ADT constructors. *)
  let report_unreachable_pm_arms (cmod : cmodule) reported_ctrs =
    let report id = warn "Unreachable pattern " id SR.get_loc in
    let rec report_unreachable = function
      | Constructor (id, plist) ->
          if
            List.mem reported_ctrs (SCIdentifier.get_id id)
              ~equal:SCIdentifier.Name.equal
          then report id
          else List.iter plist ~f:report_unreachable
      | Wildcard | Binder _ -> ()
    in
    let report_unreachable_adapter (p, _) = report_unreachable p in
    let rec report_expr (expr, _) =
      match expr with
      | Let (_, _, lhs, rhs) ->
          report_expr lhs;
          report_expr rhs
      | Fun (_, _, body) -> report_expr body
      | MatchExpr (_, plist) -> List.iter plist ~f:report_unreachable_adapter
      | TFun (_, body) -> report_expr body
      | Literal _ | Var _ | GasExpr _ | Fixpoint _ | TApp _ | Message _
      | Builtin _ | Constr _ | App _ ->
          ()
    in
    let report_stmt (stmt, _) =
      match stmt with
      | Bind (_, e) -> report_expr e
      | MatchStmt (_, pslist) -> List.iter pslist ~f:report_unreachable_adapter
      | Load _ | RemoteLoad _ | Store _ | MapUpdate _ | MapGet _
      | RemoteMapGet _ | ReadFromBC _ | TypeCast _ | AcceptPayment | GasStmt _
      | Throw _ | Iterate _ | CallProc _ | CreateEvnt _ | SendMsgs _ ->
          ()
    in
    Option.iter cmod.libs ~f:(fun l ->
        List.iter l.lentries ~f:(function
          | LibVar (_, _, ea) -> report_expr ea
          | LibTyp _ -> ()));
    List.iter cmod.contr.ccomps ~f:(fun comp ->
        List.iter comp.comp_body ~f:(fun stmt -> report_stmt stmt))

  (** Checks for dead code in the given expressions list
      @return Used variables, ADTs and their constructors *)
  let rec expr_iter (expr, _) =
    match expr with
    | Literal l ->
        let lit_adts, lit_ctrs = user_types_in_literal [ l ] in
        (emp_erset, lit_adts, lit_ctrs)
    | Var v -> (ERSet.singleton v, emp_idset, emp_idset)
    | TApp (v, tys) -> (ERSet.singleton v, user_types_in_adt tys, emp_idset)
    | Message mlist ->
        let fvars =
          List.filter_map mlist ~f:(fun (_, pl) ->
              match pl with MVar v -> Some v | MLit _ -> None)
          |> ERSet.of_list
        in
        (fvars, emp_idset, emp_idset)
    | App (f, actuals) ->
        ((ERSet.of_list actuals |> fun s -> ERSet.add s f), emp_idset, emp_idset)
    | Constr (ctr, tys, actuals) ->
        ( ERSet.of_list actuals,
          user_types_in_adt tys,
          SCIdentifierSet.singleton @@ SCIdentifier.get_id ctr )
    | Builtin (_, tys, actuals) ->
        (ERSet.of_list actuals, user_types_in_adt tys, emp_idset)
    | Fixpoint (a, ty, e) | Fun (a, ty, e) ->
        let e_fv, e_adts, e_ctrs = expr_iter e in
        let e_fv_no_a =
          ERSet.filter ~f:(fun i -> not @@ SCIdentifier.equal i a) e_fv
        in
        ( e_fv_no_a,
          SCIdentifierSet.union (user_types_in_adt [ ty ]) e_adts,
          e_ctrs )
    | Let (i, _, lhs, rhs) ->
        let fv_rhs, adts_rhs, ctrs_rhs = expr_iter rhs in
        let fvrhs_no_i =
          ERSet.filter ~f:(fun x -> not @@ SCIdentifier.equal i x) fv_rhs
        in
        if ERSet.mem fv_rhs i then
          (* LHS is not dead *)
          let fvlhs, tylhs, ctrlhs = expr_iter lhs in
          let fv = ERSet.union fvlhs fvrhs_no_i in
          let adts = SCIdentifierSet.union adts_rhs tylhs in
          let ctrs = SCIdentifierSet.union ctrs_rhs ctrlhs in
          (fv, adts, ctrs)
        else (
          (* Give a warning *)
          warn "Unused let expression: " i ER.get_loc;
          (fvrhs_no_i, adts_rhs, ctrs_rhs))
    | MatchExpr (x, plist) ->
        (* Iterate through each pattern like Let *)
        List.fold_left plist ~init:(emp_erset, emp_idset, emp_idset)
          ~f:(fun (res_fv, res_adts, res_ctrs) (pat, exp') ->
            let fvl, adts, ctrs = expr_iter exp' in
            let bounds = get_pattern_bounds pat in
            (* Check that every bound is used in the expression *)
            let bounds_unused =
              List.filter bounds ~f:(fun bound -> not @@ ERSet.mem fvl bound)
            in
            (* Checking for dead bounds *)
            if not @@ List.is_empty bounds_unused then
              List.iter bounds_unused ~f:(fun bound ->
                  warn "Unused match bound: " bound ER.get_loc);
            (* Remove bound variables from the free variables list *)
            let fvl' =
              ERSet.filter fvl ~f:(fun a ->
                  not @@ SCIdentifier.is_mem_id a bounds)
            in
            ( ERSet.singleton x |> ERSet.union fvl' |> ERSet.union res_fv,
              SCIdentifierSet.union res_adts adts,
              SCIdentifierSet.union res_ctrs ctrs ))
    | TFun (a, e) ->
        let e_fv, e_adts, e_ctrs = expr_iter e in
        let e_fv' =
          ERSet.filter ~f:(fun i -> not @@ SCIdentifier.equal i a) e_fv
        in
        (e_fv', e_adts, e_ctrs)
    | GasExpr (_, e) -> expr_iter e

  (** Provides checking for dead statement.
      @return Used variables, ADTs and their constructors found after
              traversing statements. *)
  let rec stmt_iter (fs : FieldsState.fs) stmts =
    match stmts with
    | (s, _) :: rest_stmts -> (
        let lv, adts, ctrs = stmt_iter fs rest_stmts in
        match s with
        | Load (x, m) ->
            FieldsState.mark_field_read fs m;
            if ERSet.mem lv x then
              (* m is a field, thus we don't track its liveness *)
              (* Remove liveness of x - as seen when checking:
                 tests/contracts/dead_code_test4.scilla *)
              let live_vars_no_x =
                ERSet.filter lv ~f:(fun i -> not @@ SCIdentifier.equal i x)
              in
              (live_vars_no_x, adts, ctrs) (* (live_vars, adts, ctrs) *)
            else (
              warn "Unused load statement to: " x ER.get_loc;
              (lv, adts, ctrs))
        | RemoteLoad (x, addr, m) ->
            FieldsState.mark_field_read fs m;
            (* m is a field, thus we don't track its liveness *)
            if ERSet.mem lv x then
              let live_vars_no_x =
                ERSet.filter ~f:(fun i -> not @@ SCIdentifier.equal i x) lv
              in
              (ERSet.add live_vars_no_x addr, adts, ctrs)
            else (
              warn "Unused remote load statement to: " x ER.get_loc;
              (lv, adts, ctrs))
        | Store (i, m) ->
            FieldsState.mark_field_write fs i;
            (ERSet.add lv m, adts, ctrs)
        | MapUpdate (i, il, io) ->
            FieldsState.mark_field_write fs i;
            let live_vars' =
              match io with Some ii -> i :: ii :: il | None -> i :: il
            in
            (ERSet.of_list live_vars' |> ERSet.union lv, adts, ctrs)
        | MapGet (x, i, il, _) ->
            (* i is a field, thus we don't track its liveness *)
            FieldsState.mark_field_read fs i;
            if ERSet.mem lv x then
              let live_vars_no_x =
                ERSet.filter ~f:(fun i -> not @@ SCIdentifier.equal i x) lv
              in
              (ERSet.of_list il |> ERSet.union live_vars_no_x, adts, ctrs)
            else (
              warn "Unused map get statement to: " x ER.get_loc;
              (lv, adts, ctrs))
        | RemoteMapGet (x, addr, i, il, _) ->
            (* i is a field, thus we don't track its liveness *)
            FieldsState.mark_field_read fs i;
            if ERSet.mem lv x then
              let live_vars_no_x =
                ERSet.filter ~f:(fun i -> not @@ SCIdentifier.equal i x) lv
              in
              ( addr :: il |> ERSet.of_list |> ERSet.union live_vars_no_x,
                adts,
                ctrs )
            else (
              warn "Unused remote map get statement to: " x ER.get_loc;
              (lv, adts, ctrs))
        | ReadFromBC (x, bf) ->
            if not @@ ERSet.mem lv x then
              warn "Unused Read From BC statement to: " x ER.get_loc;
            ( bf
              |> (function
                   | CurBlockNum | ChainID -> []
                   | Timestamp bn -> [ bn ]
                   | ReplicateContr (addr, iparams) -> [ addr; iparams ])
              |> ERSet.of_list |> ERSet.union lv,
              adts,
              ctrs )
        | Throw topt -> (
            match topt with
            | Some x -> (ERSet.add lv x, adts, ctrs)
            | None -> (lv, adts, ctrs))
        | CallProc (p, al) ->
            proc_dict := p :: !proc_dict;
            (ERSet.of_list al |> ERSet.union lv, adts, ctrs)
        | Iterate (l, p) ->
            proc_dict := p :: !proc_dict;
            (ERSet.add lv l, adts, ctrs)
        | Bind (i, e) ->
            let live_vars_no_i =
              ERSet.filter ~f:(fun x -> not @@ SCIdentifier.equal i x) lv
            in
            if ERSet.mem lv i then
              let e_live_vars, adts', ctrs' = expr_iter e in
              ( ERSet.union e_live_vars live_vars_no_i,
                SCIdentifierSet.union adts' adts,
                SCIdentifierSet.union ctrs' ctrs )
            else (
              warn "Unused bind statement to: " i ER.get_loc;
              let e_live_vars, adts', ctrs' = expr_iter e in
              ( ERSet.union e_live_vars lv,
                SCIdentifierSet.union adts' adts,
                SCIdentifierSet.union ctrs' ctrs ))
        | MatchStmt (i, pslist) ->
            let live_vars', adts', ctrs' =
              (* No live variables when analysing MatchStmt, as seen when
                 checking: tests/contracts/dead_code_test4.scilla *)
              List.fold_left pslist ~init:(emp_erset, emp_idset, emp_idset)
                ~f:(fun (res_fv, res_adts, res_ctrs) (pat, stmts) ->
                  let fvl, adts, ctrs = stmt_iter fs stmts in
                  let bounds = get_pattern_bounds pat in
                  (* Check that every bound is named in the expression *)
                  let bounds_unused =
                    List.filter bounds ~f:(fun bound ->
                        not @@ ERSet.mem fvl bound)
                  in
                  (* Checking for dead bounds *)
                  if not @@ List.is_empty bounds_unused then
                    List.iter bounds_unused ~f:(fun bound ->
                        warn "Unused match bound: " bound ER.get_loc);
                  (* Remove bound variables from the free variables list *)
                  let fvl' =
                    ERSet.filter fvl ~f:(fun a ->
                        not (SCIdentifier.is_mem_id a bounds))
                  in
                  ( ERSet.union fvl' res_fv,
                    SCIdentifierSet.union adts res_adts,
                    SCIdentifierSet.union ctrs res_ctrs ))
            in
            ( ERSet.add lv i |> ERSet.union live_vars',
              SCIdentifierSet.union adts adts',
              SCIdentifierSet.union ctrs ctrs' )
        | TypeCast (x, r, t) ->
            if ERSet.mem lv x then
              let live_vars_no_x =
                ERSet.filter ~f:(fun i -> not @@ SCIdentifier.equal x i) lv
              in
              ( ERSet.add live_vars_no_x r,
                user_types_in_adt [ t ] |> SCIdentifierSet.union adts,
                ctrs )
            else (
              warn "Unused type cast statement to: " x ER.get_loc;
              (ERSet.add lv r, adts, ctrs))
        | SendMsgs v | CreateEvnt v -> (ERSet.add lv v, adts, ctrs)
        | AcceptPayment | GasStmt _ -> (lv, adts, ctrs))
    | _ -> (emp_erset, emp_idset, emp_idset)

  (** Checks for unused module's components.
      @return Used: variables, ADTs, constructors, ADTs used in params. *)
  let check_comps cmod fields_state =
    List.fold_left cmod.contr.ccomps
      ~init:(emp_erset, emp_idset, emp_idset, emp_idset)
      ~f:(fun (res_lv, res_adts, res_ctrs, res_param_adts) comp ->
        let lv, adts, ctrs = stmt_iter fields_state comp.comp_body in
        (* Remove bound parameters *)
        let lv' =
          ERSet.filter lv ~f:(fun a ->
              not
                (List.exists comp.comp_params ~f:(fun (b, _) ->
                     SCIdentifier.equal a b)))
        in
        (* Checking for dead component parameters *)
        List.iter comp.comp_params ~f:(fun (cparam, _) ->
            if not @@ ERSet.mem lv cparam then
              match comp.comp_type with
              | CompTrans ->
                  warn "Unused transition parameter: " cparam ER.get_loc
              | CompProc ->
                  warn "Unused procedure parameter: " cparam ER.get_loc);
        (* Take out the type of bound parameters *)
        let param_adts =
          user_types_in_adt @@ List.map comp.comp_params ~f:snd
        in
        ( ERSet.union lv' res_lv,
          SCIdentifierSet.union param_adts adts
          |> SCIdentifierSet.union res_adts,
          SCIdentifierSet.union res_ctrs ctrs,
          (* We need only parameters from transitions, because they could
             receive any constructor from the outside, so all of these
             constructors are not dead. *)
          match comp.comp_type with
          | CompTrans -> SCIdentifierSet.union param_adts res_param_adts
          | CompProc -> res_param_adts ))

  let merge_id_maps m1 m2 =
    Map.merge m1 m2 ~f:(fun ~key:_ -> function
      | `Both (s1, s2) -> Some (Set.union s1 s2) | `Left s | `Right s -> Some s)

  (** Returns a list of fields with the contract address type from
      [address_params] that are used in [s]. *)
  let rec get_used_address_fields address_params (s, _annot) =
    match s with
    | RemoteLoad (_, addr, field) | RemoteMapGet (_, addr, field, _, _) ->
        Map.set emp_idsmap ~key:(get_id addr)
          ~data:(SCIdentifierSet.singleton (get_id field))
    | MatchStmt (_id, arms) ->
        List.fold_left arms ~init:emp_idsmap ~f:(fun m (_pattern, stmts) ->
            List.fold_left stmts ~init:emp_idsmap ~f:(fun m sa ->
                get_used_address_fields address_params sa |> merge_id_maps m)
            |> merge_id_maps m)
    | Bind _ | Load _ | Store _ | MapUpdate _ | MapGet _ | ReadFromBC _
    | TypeCast _ | AcceptPayment | Iterate _ | SendMsgs _ | CreateEvnt _
    | CallProc _ | Throw _ | GasStmt _ ->
        emp_idsmap

  (** Returns a set of field names of the contract address type. *)
  let get_addr_fields addr =
    List.fold_left (SType.IdLoc_Comp.Map.keys addr) ~init:emp_idset
      ~f:(fun s id -> SCIdentifierSet.add s (get_id id))

  (** Updates a map of identifiers [m] iff [ty] has contract address type.
      [m] has the following structure: [id |-> F] where [F] is a set of field
      names used in the contract address type. *)
  let update_contract_params_map m id ty =
    match ty with
    | SType.Address (ContrAddr addr) ->
        let data = get_addr_fields addr in
        Map.set m ~key:(SCIdentifier.get_id id) ~data
    | _ -> m

  (** Checks for unused fields in contract address types of [comp]'s
      parameters.
      Returns a set of ids of contract parameters used in this [comp].  *)
  let check_contract_address_types_in_params used_contract_params
      contract_params comp =
    let address_params =
      List.fold_left comp.comp_params ~init:contract_params
        ~f:(fun m (id, ty) -> update_contract_params_map m id ty)
    in
    let used_addresses =
      (* addr |-> set of used fields *)
      List.fold_left comp.comp_body ~init:emp_idsmap ~f:(fun m s ->
          get_used_address_fields address_params s |> merge_id_maps m)
    in
    (* Generate warnings for unused fields in component arguments. *)
    List.iter comp.comp_params ~f:(fun (id, ty) ->
        let name = get_id id in
        match (Map.find address_params name, Map.find used_addresses name) with
        | Some fields, Some used_fields
          when not @@ phys_equal (Set.length fields) (Set.length used_fields)
          -> (
            match ty with
            | SType.Address (ContrAddr m) ->
                List.iter (SType.IdLoc_Comp.Map.keys m) ~f:(fun id ->
                    let name = get_id id in
                    if Set.mem fields name && (not @@ Set.mem used_fields name)
                    then
                      warn1
                        ("Unused field in the contract address type: "
                       ^ as_error_string id)
                        warning_level_dead_code
                        (SType.TIdentifier.get_rep id))
            | _ -> ())
        | _ -> ());
    (* Collect fields of the [cmod] contract with contract address types used
       in this component. *)
    Map.keys used_addresses
    |> List.fold_left ~init:used_contract_params ~f:(fun used_ids_map used_id ->
           match Map.find used_ids_map used_id with
           | Some used_fields ->
               let data =
                 Map.find_exn used_addresses used_id |> Set.union used_fields
               in
               Map.set used_ids_map ~key:used_id ~data
           | None ->
               let data = Map.find_exn used_addresses used_id in
               Map.set used_ids_map ~key:used_id ~data)

  (** Checks for unused fields in contract address types occurred in contract
      parameters and parameters of contract's components. *)
  let check_param_contract_address_types cmod =
    let contract_params =
      List.fold_left cmod.contr.cparams ~init:emp_idsmap ~f:(fun m (id, ty) ->
          update_contract_params_map m id ty)
    in
    let used_contract_params =
      List.fold_left cmod.contr.ccomps ~init:emp_idsmap ~f:(fun used c ->
          check_contract_address_types_in_params used contract_params c)
    in
    (* Report unused contract parameters with contract address types. *)
    Map.keys contract_params
    |> List.iter ~f:(fun param ->
           match Map.find used_contract_params param with
           | Some used_fields ->
               let all_fields = Map.find_exn contract_params param in
               let unused_fields = Set.diff all_fields used_fields in
               if not @@ Set.is_empty unused_fields then
                 List.iter cmod.contr.cparams ~f:(fun (id, _ty) ->
                     if SCIdentifier.Name.equal param (SCIdentifier.get_id id)
                     then
                       Set.iter unused_fields ~f:(fun f ->
                           warn1
                             ("Unused field in the contract address type: "
                             ^ SCIdentifier.Name.as_string f)
                             warning_level_dead_code
                             (ER.get_loc (SCIdentifier.get_rep id))))
           | None ->
               (* All the fields of [param] are unused, so we don't report it.
                  It is an unused contract parameter.*)
               ())

  type adts_ty =
    ( Name.t,
      (int, SCIdentifierSet.t, Core.Int.comparator_witness) Map_intf.Map.t,
      SCIdentifierComp.comparator_witness )
    Map_intf.Map.t
  (** An information about used fields in ADT constructors that have contract
      address type: [ctr_name |-> (ctr_arg_pos |-> field_names)] *)

  type env_ty =
    ( Name.t,
      (Name.t, int, SCIdentifierComp.comparator_witness) Map_intf.Map.t,
      SCIdentifierComp.comparator_witness )
    Map_intf.Map.t
  (** An environment holds mapping of ADT constructor fields that are bound to
      variables inside this pattern matching arm in the following format:
        [ctr_name |-> (bind_name |> ctr_arg_pos)] *)

  (** Returns a mapping of user-defined ADTs that have contract address types
      as their constructors. *)
  let collect_contract_address_types_in_adts (cmod : SCSyntax.cmodule) : adts_ty
      =
    Option.value_map cmod.libs ~default:emp_idsmap ~f:(fun lib ->
        List.fold_left lib.lentries ~init:emp_idsmap ~f:(fun m -> function
          | LibVar _ -> m
          | LibTyp (_id, ctrs) ->
              List.fold_left ctrs ~init:m ~f:(fun m ctr ->
                  let args_map =
                    List.foldi ctr.c_arg_types
                      ~init:(Map.empty (module Int))
                      ~f:
                        (fun i m -> function
                          | SType.Address (ContrAddr addr) ->
                              Map.set m ~key:i ~data:(get_addr_fields addr)
                          | _ -> m)
                  in
                  if Map.is_empty args_map then m
                  else
                    Map.set m
                      ~key:(SCIdentifier.get_id ctr.cname)
                      ~data:args_map)))

  (** Returns a map of constructors of user-defined ADTs with contract address
      types used in the [comp]. [used] is an accumulator that holds a used map
      collected from other components. *)
  let get_contract_address_types_in_adts (used : adts_ty) (adt_ctrs : adts_ty)
      comp =
    (* [env_stack] is used to hold a stack of environments to handle recursive
       match statements. *)
    let env_stack = Stack.create () in
    (* Finds [ctr_name] and [ctr_arg_pos] for the given [bind_name]. *)
    let env_find_bind bind_name =
      let env_stack' = Stack.copy env_stack in
      let find_in_env (env : env_ty) =
        let rec aux = function
          | [] -> None
          | ctr :: ctrs ->
              let bind_name_to_ctr_arg_pos = Map.find_exn env ctr in
              if
                List.mem
                  (Map.keys bind_name_to_ctr_arg_pos)
                  bind_name ~equal:SCIdentifier.Name.equal
              then
                let ctr_arg_pos =
                  Map.find_exn bind_name_to_ctr_arg_pos bind_name
                in
                Some (ctr, ctr_arg_pos)
              else aux ctrs
        in
        aux @@ Map.keys env
      in
      let rec iter_stack = function
        | Some env ->
            find_in_env env
            |> Option.value_map
                 ~default:(iter_stack @@ Stack.pop env_stack')
                 ~f:(fun res -> Some res)
        | None -> None
      in
      iter_stack (Stack.pop env_stack')
    in
    (* Creates an environment for the given [pattern] that contains
       constructors occurred in [adt_ctrs]. *)
    let rec collect_env ?(env = emp_idsmap) ?(idx_stack = Stack.create ())
        (pat : pattern) : env_ty =
      match pat with
      | Constructor (ctr, patterns) ->
          let ctr_name = SCIdentifier.get_id ctr in
          if ctr_name |> Map.mem adt_ctrs then (
            let unreachable = -1 in
            let ctr_name = SCIdentifier.get_id ctr in
            let contract_address_poses =
              Map.find_exn adt_ctrs ctr_name |> Map.keys
            in
            Stack.push idx_stack (ctr_name, unreachable);
            let env' =
              List.foldi patterns ~init:env ~f:(fun i acc pat ->
                  if List.mem contract_address_poses i ~equal:phys_equal then (
                    ignore @@ Stack.pop_exn idx_stack;
                    Stack.push idx_stack (ctr_name, i);
                    collect_env ~env:acc ~idx_stack pat)
                  else acc)
            in
            ignore @@ Stack.pop idx_stack;
            env')
          else env
      | Binder bind_id when Stack.top idx_stack |> Option.is_some ->
          let used_ctr, idx = Stack.top_exn idx_stack in
          let bind_name_to_idx =
            Map.find env used_ctr |> Option.value ~default:emp_idsmap
          in
          let bind_name = SCIdentifier.get_id bind_id in
          Map.set env ~key:used_ctr
            ~data:(Map.set bind_name_to_idx ~key:bind_name ~data:idx)
      | _ ->
          (* TODO: Type aliases (single [Binder] with contract address type). *)
          env
    in
    let rec aux (used : adts_ty) (s, _ann) =
      match s with
      | MatchStmt (_id, arms) ->
          List.fold_left arms ~init:used ~f:(fun acc (pattern, stmts) ->
              let arm_env = collect_env pattern in
              if Map.is_empty arm_env then acc
              else (
                Stack.push env_stack arm_env;
                let res =
                  List.fold_left stmts ~init:acc ~f:(fun used' sa ->
                      aux used' sa)
                in
                ignore @@ Stack.pop env_stack;
                res))
      | RemoteLoad (_, addr, field) | RemoteMapGet (_, addr, field, _, _) -> (
          match env_find_bind (SCIdentifier.get_id addr) with
          | Some (ctr_name, ctr_arg_pos) when Map.mem adt_ctrs ctr_name ->
              let ctr_arg_pos_to_fields =
                Map.find used ctr_name
                |> Option.value ~default:(Map.empty (module Int))
              in
              let ctr_arg_pos_to_fields' =
                let data =
                  let s =
                    Map.find ctr_arg_pos_to_fields ctr_arg_pos
                    |> Option.value ~default:emp_idset
                  in
                  SCIdentifier.get_id field |> Set.add s
                in
                Map.set ctr_arg_pos_to_fields ~key:ctr_arg_pos ~data
              in
              Map.set used ~key:ctr_name ~data:ctr_arg_pos_to_fields'
          | _ -> used)
      | Bind _ | Load _ | Store _ | MapUpdate _ | MapGet _ | ReadFromBC _
      | TypeCast _ | AcceptPayment | Iterate _ | SendMsgs _ | CreateEvnt _
      | CallProc _ | Throw _ | GasStmt _ ->
          used
    in
    List.fold_left comp.comp_body ~init:used ~f:(fun used s -> aux used s)

  (** Reports unused fields in ADT constructors. *)
  let report_contract_address_types_in_adts (cmod : SCSyntax.cmodule)
      (used : adts_ty) (adt_ctrs : adts_ty) =
    Option.value_map cmod.libs ~default:() ~f:(fun lib ->
        List.iter lib.lentries ~f:(function
          | LibVar _ -> ()
          | LibTyp (_id, ctrs) ->
              List.iter ctrs ~f:(fun ctr ->
                  let ctr_name = SCIdentifier.get_id ctr.cname in
                  if Map.mem adt_ctrs ctr_name && Map.mem used ctr_name then
                    let ctr_arg_pos_to_fields_all =
                      Map.find_exn adt_ctrs ctr_name
                    in
                    let ctr_arg_pos_to_fields_used =
                      Map.find_exn used ctr_name
                    in
                    (* Likewise, we are are interested only in used constructor
                       parameters. *)
                    let ctr_arg_pos_used =
                      Map.keys ctr_arg_pos_to_fields_used
                    in
                    let ctr_arg_pos_to_fields =
                      Map.filter_keys ctr_arg_pos_to_fields_all
                        ~f:(fun ctr_arg_pos ->
                          List.mem ctr_arg_pos_used ctr_arg_pos
                            ~equal:phys_equal)
                    in
                    Map.iter_keys ctr_arg_pos_to_fields ~f:(fun ctr_arg_pos ->
                        let all_fields =
                          Map.find_exn ctr_arg_pos_to_fields_all ctr_arg_pos
                        in
                        let used_fields =
                          Map.find_exn ctr_arg_pos_to_fields_used ctr_arg_pos
                        in
                        let unused_fields = Set.diff all_fields used_fields in
                        Set.iter unused_fields ~f:(fun f ->
                            warn1
                              ("Unused field in the contract address type: "
                              ^ SCIdentifier.Name.as_string f)
                              warning_level_dead_code
                              (ER.get_loc (SCIdentifier.get_rep ctr.cname)))))))

  (** Checks for unused fields in contract address types occurred in
      user-defined ADTs. *)
  let check_adt_contract_address_types cmod =
    let adt_ctrs = collect_contract_address_types_in_adts cmod in
    if not @@ Map.is_empty adt_ctrs then
      let used =
        List.fold_left cmod.contr.ccomps ~init:emp_idsmap ~f:(fun used c ->
            get_contract_address_types_in_adts used adt_ctrs c)
      in
      if not @@ Map.is_empty used then
        report_contract_address_types_in_adts cmod used adt_ctrs

  (** Checks for dead code in the fields and constraints of a contract
      @return Live variables, ADTs and constructors, including ones found in
              fields and constraints. *)
  let check_fields_and_constraints cmod lv adts ctrs =
    let cons_lv, cons_adt, cons_ctrs = expr_iter cmod.contr.cconstraint in
    List.fold_left cmod.contr.cfields
      ~init:
        ( ERSet.union lv cons_lv,
          SCIdentifierSet.union adts cons_adt,
          SCIdentifierSet.union ctrs cons_ctrs )
      ~f:(fun (res_fv, res_adts, res_ctrs) (_, ty, fexp) ->
        let f_lv, f_adt, f_ctr = expr_iter fexp in
        ( ERSet.union f_lv res_fv,
          user_types_in_adt [ ty ]
          |> SCIdentifierSet.union f_adt
          |> SCIdentifierSet.union res_adts,
          SCIdentifierSet.union f_ctr res_ctrs ))

  (** Checks for unused parameters of a contract
      @return Live variables without contract parameters and ADTs including
              used in parameters. *)
  let check_parameters cmod lv adts =
    let param_iden, param_ty = List.unzip cmod.contr.cparams in
    List.iter param_iden ~f:(fun iden ->
        if not @@ ERSet.mem lv iden then
          warn "Unused contract parameter: " iden ER.get_loc);
    (* Remove contract params from live variable list *)
    let live_vars' =
      ERSet.filter lv ~f:(fun a -> not (SCIdentifier.is_mem_id a param_iden))
    in
    (* Adding used ADTs in parameters *)
    let adts' = user_types_in_adt param_ty |> SCIdentifierSet.union adts in
    (live_vars', adts')

  (** Checks for unused procedures *)
  let check_dead_procedures cmod =
    List.iter cmod.contr.ccomps ~f:(fun comp ->
        match comp.comp_type with
        | CompProc ->
            if not @@ SCIdentifier.is_mem_id comp.comp_name !proc_dict then
              warn "Unused procedure: " comp.comp_name SR.get_loc
        | CompTrans -> ())

  (** Checks for unused library definitions: functions, variables, types.
      @return Used library variables and ADTs after traversing library
              components and unused ADT constructors. *)
  let check_lib (cmod : cmodule) lv adts ctrs param_adts adts_to_ctrs =
    let rec check_lentries lentries lv adts ctrs param_adts used_adts =
      match lentries with
      | lentry :: rentries -> (
          let lv', adts', ctrs', reported_ctrs =
            check_lentries rentries lv adts ctrs param_adts used_adts
          in
          match lentry with
          | LibVar (i, topt, lexp) ->
              let freevars'_no_i =
                ERSet.filter ~f:(fun i' -> not @@ SCIdentifier.equal i' i) lv'
              in
              if not @@ ERSet.mem lv' i then
                warn "Unused library value: " i ER.get_loc;
              let lv'', tyl, ctrl = expr_iter lexp in
              let res_lv = ERSet.union lv'' freevars'_no_i in
              let res_adts =
                match topt with
                | None -> SCIdentifierSet.union tyl adts'
                | Some ty ->
                    user_types_in_adt [ ty ] |> SCIdentifierSet.union tyl
                    |> SCIdentifierSet.union adts'
              in
              let res_ctrs = SCIdentifierSet.union ctrs' ctrl in
              (res_lv, res_adts, res_ctrs, reported_ctrs)
          | LibTyp (i, ctrs) ->
              let ids_eq rep_id id =
                SCIdentifier.Name.equal (SCIdentifier.get_id rep_id) id
              in
              let in_set ?(i = i) l = Set.exists l ~f:(fun a -> ids_eq i a) in
              let used_ctrs, unused_ctrs =
                List.partition_tf ctrs ~f:(fun ctr ->
                    in_set ~i:ctr.cname ctrs' || in_set ~i:ctr.cname used_adts)
              in
              let adts'_no_i =
                Set.filter
                  ~f:(fun i' ->
                    not
                    @@ (ids_eq i i'
                       || List.exists used_ctrs ~f:(fun ctr ->
                              ids_eq ctr.cname i')))
                  adts'
              in
              let reported_ctrs' =
                if not @@ in_set param_adts then
                  if
                    List.is_empty used_ctrs
                    && (not @@ in_set adts')
                    && (not @@ in_set used_adts)
                  then (
                    warn "Unused library ADT: " i ER.get_loc;
                    [])
                  else
                    List.fold_left unused_ctrs ~init:reported_ctrs
                      ~f:(fun acc ctr ->
                        warn "Unused ADT constructor: " ctr.cname ER.get_loc;
                        acc @ [ SCIdentifier.get_id ctr.cname ])
                else []
              in
              (lv', adts'_no_i, ctrs', reported_ctrs'))
      | [] -> (lv, adts, ctrs, [])
    in
    Option.find_map cmod.libs ~f:(fun lib ->
        collect_adts_in_adts adts_to_ctrs
        |> SCIdentifierSet.union
           @@ collect_adts_in_params adts_to_ctrs param_adts
        |> check_lentries lib.lentries lv adts ctrs param_adts
        |> (fun (used_vars, adts, _ctrs, reported_ctrs) ->
             (used_vars, adts, reported_ctrs))
        |> Option.some)
    |> Option.value ~default:(lv, adts, [])

  (** Checks for dead code in imported libraries *)
  let check_imported_libraries (cmod : cmodule) elibs lv adts =
    List.iter elibs ~f:(fun elib ->
        let lib_used =
          List.exists elib.libn.lentries ~f:(fun lentry ->
              match lentry with
              | LibVar (iden, _, _) -> ERSet.mem lv iden
              | LibTyp (iden, _) ->
                  Set.exists adts ~f:(fun i ->
                      SCIdentifier.Name.equal (SCIdentifier.get_id iden) i))
        in
        (* Check for dead imported library *)
        if not lib_used then
          (* Okay to use find_exn cause elib must exist in cmod.elibs *)
          let import_rep, _ =
            List.find_exn cmod.elibs ~f:(fun (cmod_elib, _) ->
                SCIdentifier.equal cmod_elib elib.libn.lname)
          in
          warn1
            ("Unused imported library: " ^ as_error_string elib.libn.lname)
            warning_level_dead_code
            (SR.get_loc @@ get_rep import_rep))

  (** Detects dead code in a cmodule *)
  let dc_cmod (cmod : cmodule) (elibs : libtree list) =
    let fields_state = FieldsState.mk cmod in
    let adts_to_ctrs =
      Option.value_map cmod.libs ~default:emp_idsmap ~f:(fun l ->
          collect_adts_to_ctrs l.lentries)
    in

    (* Check components *)
    let lv_comps, comps_adts, comps_ctrs, comp_param_adts =
      check_comps cmod fields_state
    in

    (* Check for unused fields in contract address types *)
    check_param_contract_address_types cmod;
    check_adt_contract_address_types cmod;

    (* Check fields and constraints *)
    let lv_fields, adts_fields, ctrs_fields =
      check_fields_and_constraints cmod lv_comps comps_adts comps_ctrs
    in

    (* Check for dead contract parameters *)
    let lv_params, adts_params = check_parameters cmod lv_fields adts_fields in

    (* Check for dead procedures *)
    check_dead_procedures cmod;

    (* Check for dead code in library elements *)
    let lv_lib, adts_lib, reported_ctrs =
      check_lib cmod lv_params adts_params ctrs_fields comp_param_adts
        adts_to_ctrs
    in

    (* Report unused ADT constructors in pattern matching arms *)
    report_unreachable_pm_arms cmod reported_ctrs;

    (* Check for unused library items *)
    check_imported_libraries cmod elibs lv_lib adts_lib;

    (* Report fields that are read-only, write-only, or unused *)
    FieldsState.report_read_only fields_state;
    FieldsState.report_write_only fields_state;
    FieldsState.report_unused fields_state
end
