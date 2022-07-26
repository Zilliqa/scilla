(*
Checks for unused
* Procedures and their parameters
* Mutable Fields
* Immutable contract parameters
* Pattern-matching binders
* Library functions (and their parameters) and types
* Let bindings
* Library imports
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

  module SCSyntax = ScillaSyntax (SR) (ER) (SCLiteral)
  module SCU = ContractUtil.ScillaContractUtil (SR) (ER)
  open SCIdentifier
  open SCSyntax

  (* All checking procedures are marked with the comment
     (******** Checking for dead ______ ********)
  *)

  (* Warning level for dead code detection *)
  let warning_level_dead_code = 3

  (* Raise a warning *)
  let warn warn_msg name get_loc =
    warn1
      (warn_msg ^ as_error_string name)
      warning_level_dead_code
      (get_loc (get_rep name))

  (* Return a list of names of ADTs as Name from a type *)
  (* Used to finding dead user defined ADTs *)
  let rec user_types_in_adt tys =
    let rec iden_iter ty acc =
      match ty with
      | SCType.ADT (iden, targs) ->
          [ SCIdentifier.get_id iden ] @ user_types_in_adt targs @ acc
      | SCType.MapType (ty1, ty2) | SCType.FunType (ty1, ty2) ->
          iden_iter ty1 [] @ iden_iter ty2 []
      | _ -> []
    in
    List.dedup_and_sort ~compare:SCIdentifier.Name.compare
      (List.fold_left tys ~init:[] ~f:(fun iden_l ty ->
           iden_iter ty [] @ iden_l))

  (* Returns used names of ADT types with their constructors. *)
  let rec user_types_in_literal lits =
    let res =
      List.fold_left lits ~init:[] ~f:(fun res_adts lit ->
          match lit with
          | SLiteral.Map ((ty1, ty2), _) ->
              user_types_in_adt [ ty1 ] @ user_types_in_adt [ ty2 ] @ res_adts
          | SLiteral.ADTValue (ctr, tys, ts) ->
              [ ctr ] @ user_types_in_adt tys @ user_types_in_literal ts
              @ res_adts
          | _ -> [])
    in
    List.dedup_and_sort ~compare:SCIdentifier.Name.compare res

  let dedup_id_list = SCIdentifier.dedup_id_list

  let dedup_name_list l =
    List.dedup_and_sort ~compare:SCIdentifier.Name.compare l

  let proc_dict = ref []

  (* Distinguishing read-only, write-only, and unused fields *)
  let read_field = ref []
  let write_field = ref []

  (** [collect_adts_to_ctrs lentries] collects a mapping from ADTs to their
      constructor definitions. *)
  let collect_adts_to_ctrs lentries =
    List.fold_left lentries
      ~init:(Map.empty (module SCIdentifierComp))
      ~f:(fun m lentry ->
        match lentry with
        | LibTyp (id, ctr_defs) ->
            Map.set m ~key:(SCIdentifier.get_id id) ~data:ctr_defs
        | LibVar _ -> m)

  (** [collect_used_adts adts_to_ctrs] returns a list of ADT and
      constructor names used in the constructors of user-defined ADTs. *)
  let collect_used_adts adts_to_ctrs =
    Map.data adts_to_ctrs
    |> List.fold_left ~init:[] ~f:(fun acc ctr_defs ->
           List.fold_left ctr_defs ~init:[] ~f:(fun acc ctr_def ->
               List.fold_left ctr_def.c_arg_types ~init:[] ~f:(fun acc ty ->
                   acc @ user_types_in_adt [ ty ])
               |> List.append acc)
           |> List.append acc)
    |> dedup_name_list

  (** [report_unreachable_pm_arms cmod reported_ctrs] shows warnings for
      pattern-matching arms that check for unused user-defined ADT
      constructors. *)
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

  (* Detect Dead Code in a cmod *)
  let dc_cmod (cmod : cmodule) (elibs : libtree list) =
    (* Marking a field is used for read or write *)
    let cfields = List.map cmod.contr.cfields ~f:(fun (f', _, _) -> f') in
    let mark_field_read f =
      if SCIdentifier.is_mem_id f cfields then read_field := f :: !read_field
    in
    let mark_field_write f =
      if SCIdentifier.is_mem_id f cfields then write_field := f :: !write_field
    in

    (******** Checking for dead expressions ********)
    (* Returns free variables, used ADT types with their constructors *)
    let rec expr_iter (expr, _) =
      match expr with
      | Literal l ->
          let free_tys = user_types_in_literal [ l ] in
          ([], dedup_name_list free_tys)
      | Var v -> ([ v ], [])
      | TApp (v, tys) -> ([ v ], user_types_in_adt tys)
      | Message mlist ->
          let fvars =
            List.filter_map mlist ~f:(fun (_, pl) ->
                match pl with MVar v -> Some v | MLit _ -> None)
          in
          (fvars, [])
      | App (f, actuals) -> (f :: actuals, [])
      | Constr (ctr, tys, actuals) ->
          (actuals, user_types_in_adt tys @ [ SCIdentifier.get_id ctr ])
      | Builtin (_, tys, actuals) -> (actuals, user_types_in_adt tys)
      | Fixpoint (a, ty, e) | Fun (a, ty, e) ->
          let e_fv, e_adts = expr_iter e in
          let e_fv_no_a =
            List.filter ~f:(fun i -> not @@ SCIdentifier.equal i a) e_fv
          in
          (e_fv_no_a, dedup_name_list @@ user_types_in_adt [ ty ] @ e_adts)
      | Let (i, _, lhs, rhs) ->
          let fv_rhs, adts_rhs = expr_iter rhs in
          let fvrhs_no_i =
            List.filter ~f:(fun x -> not @@ SCIdentifier.equal i x) fv_rhs
          in
          if SCIdentifier.is_mem_id i fv_rhs then
            (* LHS is not dead *)
            let fvlhs, tylhs = expr_iter lhs in
            let fv = dedup_id_list (fvlhs @ fvrhs_no_i) in
            let adts = dedup_name_list (adts_rhs @ tylhs) in
            (fv, adts)
          else (
            (* Give a warning *)
            warn "Unused let expression: " i ER.get_loc;
            (fvrhs_no_i, adts_rhs))
      | MatchExpr (x, plist) ->
          (* Iterate through each pattern like Let *)
          List.fold_left plist ~init:([], [])
            ~f:(fun (res_fv, res_adts) (pat, exp') ->
              let fvl, adts = expr_iter exp' in
              let bounds = get_pattern_bounds pat in
              (* Check that every bound is used in the expression *)
              let bounds_unused =
                List.filter bounds ~f:(fun bound ->
                    not @@ SCIdentifier.is_mem_id bound fvl)
              in
              (******** Checking for dead bounds ********)
              if not @@ List.is_empty bounds_unused then
                List.iter bounds_unused ~f:(fun bound ->
                    warn "Unused match bound: " bound ER.get_loc);
              (* Remove bound variables from the free variables list *)
              let fvl' =
                List.filter fvl ~f:(fun a ->
                    not @@ SCIdentifier.is_mem_id a bounds)
              in
              ((x :: fvl') @ res_fv, res_adts @ adts))
      | TFun (a, e) ->
          let e_fv, e_adts = expr_iter e in
          let e_fv' =
            List.filter ~f:(fun i -> not @@ SCIdentifier.equal i a) e_fv
          in
          (e_fv', e_adts)
      | GasExpr (_, e) -> expr_iter e
    in

    (******** Checking for dead statements ********)
    (* Returns free variables and used ADTs *)
    let rec stmt_iter stmts =
      match stmts with
      | (s, _) :: rest_stmts -> (
          let live_vars, adts = stmt_iter rest_stmts in
          match s with
          | Load (x, m) ->
              mark_field_read m;
              if SCIdentifier.is_mem_id x live_vars then
                (* m is a field, thus we don't track its liveness *)
                (* Remove liveness of x - as seen when checking tests/contracts/dead_code_test4.scilla  *)
                let live_vars_no_x =
                  List.filter
                    ~f:(fun i -> not @@ SCIdentifier.equal i x)
                    live_vars
                in
                (live_vars_no_x, adts) (* (live_vars, adts) *)
              else (
                warn "Unused load statement to: " x ER.get_loc;
                (live_vars, adts))
          | RemoteLoad (x, addr, m) ->
              mark_field_read m;
              (* m is a field, thus we don't track its liveness *)
              if SCIdentifier.is_mem_id x live_vars then
                let live_vars_no_x =
                  List.filter
                    ~f:(fun i -> not @@ SCIdentifier.equal i x)
                    live_vars
                in
                (dedup_id_list (addr :: live_vars_no_x), adts)
              else (
                warn "Unused remote load statement to: " x ER.get_loc;
                (live_vars, adts))
          | Store (i, m) ->
              mark_field_write i;
              (dedup_id_list (m :: live_vars), adts)
          | MapUpdate (i, il, io) ->
              mark_field_write i;
              let live_vars' =
                match io with Some ii -> i :: ii :: il | None -> i :: il
              in
              (dedup_id_list @@ live_vars' @ live_vars, adts)
          | MapGet (x, i, il, _) ->
              (* i is a field, thus we don't track its liveness *)
              mark_field_read i;
              if SCIdentifier.is_mem_id x live_vars then
                let live_vars_no_x =
                  List.filter
                    ~f:(fun i -> not @@ SCIdentifier.equal i x)
                    live_vars
                in
                (dedup_id_list (il @ live_vars_no_x), adts)
              else (
                warn "Unused map get statement to: " x ER.get_loc;
                (live_vars, adts))
          | RemoteMapGet (x, addr, i, il, _) ->
              (* i is a field, thus we don't track its liveness *)
              mark_field_read i;
              if SCIdentifier.is_mem_id x live_vars then
                let live_vars_no_x =
                  List.filter
                    ~f:(fun i -> not @@ SCIdentifier.equal i x)
                    live_vars
                in
                (dedup_id_list (addr :: (il @ live_vars_no_x)), adts)
              else (
                warn "Unused remote map get statement to: " x ER.get_loc;
                (live_vars, adts))
          | ReadFromBC (x, bf) ->
              if not @@ SCIdentifier.is_mem_id x live_vars then
                warn "Unused Read From BC statement to: " x ER.get_loc;
              ( dedup_id_list
                  (match bf with
                  | CurBlockNum | ChainID -> []
                  | Timestamp bn -> [ bn ]
                  | ReplicateContr (addr, iparams) -> [ addr; iparams ])
                @ live_vars,
                adts )
          | Throw topt -> (
              match topt with
              | Some x -> (dedup_id_list (x :: live_vars), adts)
              | None -> (live_vars, adts))
          | CallProc (p, al) ->
              proc_dict := p :: !proc_dict;
              (dedup_id_list (al @ live_vars), adts)
          | Iterate (l, p) ->
              proc_dict := p :: !proc_dict;
              (dedup_id_list (l :: live_vars), adts)
          | Bind (i, e) ->
              let live_vars_no_i =
                List.filter
                  ~f:(fun x -> not @@ SCIdentifier.equal i x)
                  live_vars
              in
              if SCIdentifier.is_mem_id i live_vars then
                let e_live_vars, adts' = expr_iter e in
                ( dedup_id_list @@ e_live_vars @ live_vars_no_i,
                  dedup_name_list @@ adts' @ adts )
              else (
                warn "Unused bind statement to: " i ER.get_loc;
                let e_live_vars, adts' = expr_iter e in
                ( dedup_id_list @@ e_live_vars @ live_vars,
                  dedup_name_list @@ adts' @ adts ))
          | MatchStmt (i, pslist) ->
              let live_vars', adts' =
                (* No live variables when analysing MatchStmt, as seen when checking tests/contracts/dead_code_test4.scilla *)
                List.fold_left pslist ~init:([], [])
                  ~f:(fun (res_fv, res_adts) (pat, stmts) ->
                    let fvl, adts = stmt_iter stmts in
                    let bounds = get_pattern_bounds pat in
                    (* Check that every bound is named in the expression *)
                    let bounds_unused =
                      List.filter bounds ~f:(fun bound ->
                          not @@ SCIdentifier.is_mem_id bound fvl)
                    in
                    (******** Checking for dead bounds ********)
                    if not @@ List.is_empty bounds_unused then
                      List.iter bounds_unused ~f:(fun bound ->
                          warn "Unused match bound: " bound ER.get_loc);
                    (* Remove bound varaibles from the free variables list *)
                    let fvl' =
                      List.filter fvl ~f:(fun a ->
                          not (SCIdentifier.is_mem_id a bounds))
                    in
                    (fvl' @ res_fv, adts @ res_adts))
              in
              ((i :: live_vars) @ live_vars', adts @ adts')
          | TypeCast (x, r, t) ->
              if SCIdentifier.is_mem_id x live_vars then
                let live_vars_no_x =
                  List.filter
                    ~f:(fun i -> not @@ SCIdentifier.equal x i)
                    live_vars
                in
                (r :: live_vars_no_x, adts @ user_types_in_adt [ t ])
              else (
                warn "Unused type case statement to: " x ER.get_loc;
                (live_vars, adts))
          | SendMsgs v | CreateEvnt v ->
              (dedup_id_list @@ (v :: live_vars), adts)
          | AcceptPayment | GasStmt _ -> (live_vars, adts))
      | _ -> ([], [])
    in

    let adts_to_ctrs =
      Option.value_map cmod.libs
        ~default:(Map.empty (module SCIdentifierComp))
        ~f:(fun l -> collect_adts_to_ctrs l.lentries)
    in

    (******** Checking for dead library function/variable/type definitions ********)
    (* DCD library entries. *)
    let rec dcd_lib_entries lentries freevars adts param_adts used_adts =
      match lentries with
      | lentry :: rentries -> (
          let freevars', adts', reported_ctrs =
            dcd_lib_entries rentries freevars adts param_adts used_adts
          in
          match lentry with
          | LibVar (i, topt, lexp) ->
              let freevars'_no_i =
                List.filter
                  ~f:(fun i' -> not @@ SCIdentifier.equal i' i)
                  freevars'
              in
              if not @@ SCIdentifier.is_mem_id i freevars' then
                warn "Unused library value: " i ER.get_loc;
              let fv, tyl = expr_iter lexp in
              let res_fv = dedup_id_list @@ fv @ freevars'_no_i in
              let res_adts =
                match topt with
                | None -> dedup_name_list (tyl @ adts')
                | Some ty ->
                    dedup_name_list (user_types_in_adt [ ty ] @ tyl @ adts')
              in
              (res_fv, res_adts, reported_ctrs)
          | LibTyp (i, ctrs) ->
              let ids_eq rep_id id =
                SCIdentifier.Name.equal (SCIdentifier.get_id rep_id) id
              in
              let in_list ?(i = i) l = List.exists l ~f:(fun a -> ids_eq i a) in
              let used_ctrs, unused_ctrs =
                List.partition_tf ctrs ~f:(fun ctr ->
                    in_list ~i:ctr.cname adts' || in_list ~i:ctr.cname used_adts)
              in
              let adts'_no_i =
                List.filter
                  ~f:(fun i' ->
                    not
                    @@ (ids_eq i i'
                       || List.exists used_ctrs ~f:(fun ctr ->
                              ids_eq ctr.cname i')))
                  adts'
              in
              let reported_ctrs' =
                if not @@ (in_list param_adts || in_list used_adts) then
                  if List.is_empty used_ctrs && (not @@ in_list adts') then (
                    warn "Unused library ADT: " i ER.get_loc;
                    [])
                  else
                    List.fold_left unused_ctrs ~init:reported_ctrs
                      ~f:(fun acc ctr ->
                        warn "Unused ADT constructor: " ctr.cname ER.get_loc;
                        acc @ [ SCIdentifier.get_id ctr.cname ])
                else []
              in
              (freevars', adts'_no_i, reported_ctrs'))
      | [] -> (freevars, adts, [])
    in

    (* Detect Dead Code in a library. *)
    let dcd_lib lib freevars adts param_adts =
      collect_used_adts adts_to_ctrs
      |> dcd_lib_entries lib.lentries freevars adts param_adts
    in

    (* START *)
    (* Iterate through contract components. *)
    let comps_lv, comps_adts, comp_param_adts =
      List.fold_left cmod.contr.ccomps ~init:([], [], [])
        ~f:(fun (res_fv, res_adts, res_param_adts) comp ->
          let lv, adts = stmt_iter comp.comp_body in
          (* Remove bound parameters *)
          let lv' =
            List.filter lv ~f:(fun a ->
                not
                  (List.exists comp.comp_params ~f:(fun (b, _) ->
                       SCIdentifier.equal a b)))
          in
          (******** Checking for dead component parameters ********)
          List.iter comp.comp_params ~f:(fun (cparam, _) ->
              if not @@ SCIdentifier.is_mem_id cparam lv then
                match comp.comp_type with
                | CompTrans ->
                    warn "Unused transition parameter: " cparam ER.get_loc
                | CompProc ->
                    warn "Unused procedure parameter: " cparam ER.get_loc);
          (* Take out the type of bound parameters *)
          let param_adts =
            user_types_in_adt @@ List.map comp.comp_params ~f:snd
          in
          ( lv' @ res_fv,
            param_adts @ adts @ res_adts,
            (* We need only parameters from transitions, because they could
               receive any constructor from the outside, so all of these
               constructors are not dead. *)
            match comp.comp_type with
            | CompTrans -> param_adts @ res_param_adts
            | CompProc -> res_param_adts ))
    in
    let comps_lv' = dedup_id_list comps_lv in
    let comps_adts' = dedup_name_list comps_adts in
    let comp_param_adts' = dedup_name_list comp_param_adts in

    (* Iterate through constraints *)
    let cons_lv, cons_adt = expr_iter cmod.contr.cconstraint in

    (* Iterate through field initialisations *)
    let fields_lv, fields_adts =
      List.fold_left cmod.contr.cfields
        ~init:(comps_lv' @ cons_lv, comps_adts' @ cons_adt)
        ~f:(fun (res_fv, res_adts) (_, ty, fexp) ->
          let f_lv, f_adt = expr_iter fexp in
          (f_lv @ res_fv, user_types_in_adt [ ty ] @ f_adt @ res_adts))
    in

    (* Note: fields_lv' and fields_adts' also contains data from contraints and components *)
    let fields_lv' = dedup_id_list fields_lv in
    let fields_adts' = dedup_name_list fields_adts in

    (******** Checking for dead contract parameters ********)
    let param_iden, param_ty = List.unzip cmod.contr.cparams in
    List.iter param_iden ~f:(fun iden ->
        if not @@ SCIdentifier.is_mem_id iden fields_lv' then
          warn "Unused contract parameter: " iden ER.get_loc);

    (* Remove contract params from live variable list *)
    let lv_contract =
      List.filter fields_lv' ~f:(fun a ->
          not (SCIdentifier.is_mem_id a param_iden))
    in
    (* Adding used ADTs in parameters *)
    let lv_adts =
      dedup_name_list @@ fields_adts' @ user_types_in_adt param_ty
    in

    (******** Checking for dead procedures ********)
    List.iter cmod.contr.ccomps ~f:(fun comp ->
        match comp.comp_type with
        | CompProc ->
            if not @@ SCIdentifier.is_mem_id comp.comp_name !proc_dict then
              warn "Unused procedure: " comp.comp_name SR.get_loc
        | CompTrans -> ());

    (* Iterate through contract library *)
    let lv_clibs, lv_adts, reported_ctrs =
      match cmod.libs with
      | Some l -> dcd_lib l lv_contract lv_adts comp_param_adts'
      | None -> (lv_contract, lv_adts, [])
    in

    report_unreachable_pm_arms cmod reported_ctrs;

    (* Iterate through elibs to check if imported library is used *)
    List.iter elibs ~f:(fun elib ->
        let lib_used =
          List.exists elib.libn.lentries ~f:(fun lentry ->
              match lentry with
              | LibVar (iden, _, _) -> SCIdentifier.is_mem_id iden lv_clibs
              | LibTyp (iden, _) ->
                  List.exists lv_adts ~f:(fun i ->
                      SCIdentifier.Name.equal (SCIdentifier.get_id iden) i))
        in
        (******** Checking for dead imported library ********)
        if not lib_used then
          (* Okay to use find_exn cause elib must exist in cmod.elibs *)
          let import_rep, _ =
            List.find_exn cmod.elibs ~f:(fun (cmod_elib, _) ->
                SCIdentifier.equal cmod_elib elib.libn.lname)
          in
          warn1
            ("Unused imported library: " ^ as_error_string elib.libn.lname)
            warning_level_dead_code
            (SR.get_loc @@ get_rep import_rep));

    (* Printing what fields are read-only, write-only, or unused *)
    let field_read_only =
      List.filter (dedup_id_list !read_field) ~f:(fun f ->
          not @@ SCIdentifier.is_mem_id f !write_field)
    in
    let field_write_only =
      List.filter (dedup_id_list !write_field) ~f:(fun f ->
          not @@ SCIdentifier.is_mem_id f !read_field)
    in
    let field_unused =
      List.filter cmod.contr.cfields ~f:(fun (f, _, _) ->
          (not @@ SCIdentifier.is_mem_id f !read_field)
          && (not @@ SCIdentifier.is_mem_id f !write_field))
    in

    if not @@ List.is_empty field_read_only then
      List.iter field_read_only ~f:(fun field ->
          warn "Read only field, consider making it a contract parameter: "
            field ER.get_loc);

    if not @@ List.is_empty field_write_only then
      List.iter field_write_only ~f:(fun field ->
          warn "Write only field: " field ER.get_loc);

    (******** Checking for dead fields ********)
    if not @@ List.is_empty field_unused then
      List.iter field_unused ~f:(fun (field, _, _) ->
          warn "Unused field: " field ER.get_loc)
end
