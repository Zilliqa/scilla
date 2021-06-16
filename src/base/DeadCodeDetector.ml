(*
Checks for unused
- Procedures and their parameters
- Mutable Fields
- Immutable contract parameters
- Pattern-matching binders
- Library functions (and their parameters) and types
- Limbrary imports
*)

open Core_kernel
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
  module SCSyntax = ScillaSyntax (SR) (ER) (SCLiteral)
  module SCU = ContractUtil.ScillaContractUtil (SR) (ER)
  open SCIdentifier
  open SCSyntax
  open AssocDictionary

  (* Warning level for dead code detection *)
  let warning_level_dead_code = 3

  (* Update a dictionary that a value is used, return whether we updated a dict *)
  let mark_used dict_ref name =
    let sname = as_error_string name in
    let v = lookup sname !dict_ref in
    match v with
    | Some (false, rep) ->
        dict_ref := update_all (as_error_string name) (true, rep) !dict_ref;
        true
    | _ -> false

  (* Clear dictionary *)
  let clear_dict dict_ref = dict_ref := make_dict ()

  (* Add to dict *)
  let add_dict dict_ref name =
    dict_ref :=
      insert_unique (as_error_string name) (false, get_rep name) !dict_ref

  (* Filter through dictionary to find unused identifiers *)
  let find_unused dict warn_msg get_loc =
    let unused_list =
      List.map
        (List.filter (to_list dict) ~f:(fun (_, (v, _)) -> not v))
        ~f:(fun (name, (_, rep)) -> (name, rep))
    in
    if not (List.is_empty unused_list) then
      List.iter unused_list ~f:(fun (name, rep) ->
          warn1 (warn_msg ^ name) warning_level_dead_code (get_loc rep))

  (* Detect Dead code in Contract Modules *)
  let dc_cmod (cmod : cmodule) (elibs : libtree list) =
    (**************** Assertions ***************)
    (* Checking elibs and cmod.elib are the same *)
    let cmod_elib_list = List.map cmod.elibs ~f:fst in
    let elibs_list = List.map elibs ~f:(fun libt -> libt.libn.lname) in
    if List.length cmod_elib_list <> List.length elibs_list then
      warn1 "Bug in imported libraries - imported libraries might be incorrect"
        warning_level_dead_code (SR.get_loc SR.dummy_rep);
    List.iter cmod_elib_list ~f:(fun elib ->
        let find =
          List.find elibs_list ~f:(fun e -> SCIdentifier.equal e elib)
        in
        match find with
        | None ->
            warn1
              (as_error_string elib ^ " not found in elib")
              warning_level_dead_code (SR.get_loc SR.dummy_rep)
        | Some _ -> ());
    List.iter elibs_list ~f:(fun elib ->
        let find =
          List.find cmod_elib_list ~f:(fun e -> SCIdentifier.equal e elib)
        in
        match find with
        | None ->
            warn1
              (as_error_string elib ^ " not found in cmod_elib_list")
              warning_level_dead_code (SR.get_loc SR.dummy_rep)
        | Some _ -> ());

    (************** DC Detector ***************)
    (* Global contract dictionaries: fields, contract parameters, procedures *)
    let cfields_dict = ref (make_dict ()) in
    let cparams_dict = ref (make_dict ()) in
    let proc_dict = ref (make_dict ()) in

    (* Library entries *)
    let libvar_dict = ref (make_dict ()) in
    let libty_dict = ref (make_dict ()) in

    (* Library imports *)
    let elibs_dict = ref (make_dict ()) in

    (********* Populate library imports ***********)
    List.iter cmod.elibs ~f:(fun (lib_name, _) -> add_dict elibs_dict lib_name);

    (******* Populate library entries *******)
    let pop_lib_dict () =
      match cmod.libs with
      | None -> ()
      | Some lib ->
          List.iter lib.lentries ~f:(fun lib_entry ->
              match lib_entry with
              | LibVar (name, _, _) -> add_dict libvar_dict name
              | LibTyp (name, _) -> add_dict libty_dict name)
    in
    pop_lib_dict ();

    (******* Populate contract dictionaries *******)
    List.iter cmod.contr.cfields ~f:(fun (iden, _, _) ->
        add_dict cfields_dict iden);

    List.iter cmod.contr.cparams ~f:(fun (cparam, _) ->
        add_dict cparams_dict cparam);

    List.iter cmod.contr.ccomps ~f:(fun comp ->
        match comp.comp_type with
        | CompTrans -> ()
        | CompProc ->
            (* Populate the procedure dictionary *)
            proc_dict :=
              insert_unique
                (as_error_string comp.comp_name)
                (false, ER.dummy_rep) !proc_dict);

    (******** Marking Used Identifiers *********)
    (* "The scope of variables in an imported library is restricted to
       the immediate importer." *)
    (* Check if var or type exists in library *)
    let exists_in_lib x elib =
      let iter_lib_entry lentries x =
        let found = ref false in
        List.iter lentries ~f:(fun lentry ->
            match lentry with
            | LibVar (iden, _, _) ->
                if SCIdentifier.equal iden x then found := true
            | LibTyp (iden, _) ->
                (* safe to check with var cause rep would be unique *)
                if SCIdentifier.equal iden x then found := true);
        !found
      in
      iter_lib_entry elib.libn.lentries x
    in

    (* Given a lib_entry, mark its library parent as used *)
    let mark_used_elibs f =
      (* find all elibs that have not been used *)
      let unused_elibs =
        List.filter elibs ~f:(fun elib ->
            match
              AssocDictionary.lookup
                (as_error_string elib.libn.lname)
                !elibs_dict
            with
            | None -> false (* shouldn't happen *)
            | Some (is_used, _) -> not is_used)
      in
      let res = ref false in
      (* iterate through elibs, check if they have been used, find the f function in them, mark if found*)
      List.iter unused_elibs ~f:(fun elib ->
          if exists_in_lib f elib then
            let _ = mark_used elibs_dict elib.libn.lname in
            res := true);
      !res
    in

    (* Marking use of ADTs *)
    let rec mark_used_ty ty =
      match ty with
      | SCType.ADT (iden, _) ->
          let _ = mark_used libty_dict iden in
          let _ = mark_used_elibs iden in
          ()
      | SCType.MapType (ty1, ty2) | SCType.FunType (ty1, ty2) ->
          mark_used_ty ty1;
          mark_used_ty ty2
      | _ -> ()
    in

    (**************** Iterators ****************)
    (* Iterate through expressions to look for use of
       - Contract parameters
       - Contract parameters/Pattern binders/Local variables
       - Defined libraries' functions/types
       - Imported libraries
    *)
    let rec expr_iter expr local_dicts =
      let mark_used' x =
        if not @@ mark_used cparams_dict x then
          if not @@ mark_used libvar_dict x then
            if not @@ mark_used_elibs x then
              List.iter local_dicts ~f:(fun dict ->
                  let _ = mark_used dict x in
                  ())
      in

      match fst expr with
      | Literal l -> (
          match l with
          | Msg msg -> List.iter msg ~f:(fun (_, ty, _) -> mark_used_ty ty)
          | Map ((ty1, ty2), _) ->
              mark_used_ty ty1;
              mark_used_ty ty2
          | ADTValue (_, tys, _) -> List.iter tys ~f:mark_used_ty
          | _ -> ())
      | Var x -> mark_used' x
      | Let (i, ty_o, e1, e2) ->
          (match ty_o with None -> () | Some ty -> mark_used_ty ty);
          let local_dict = ref (make_dict ()) in
          local_dict :=
            insert_unique (as_error_string i) (false, get_rep i) !local_dict;
          expr_iter e1 local_dicts;
          expr_iter e2 (local_dict :: local_dicts);
          find_unused !local_dict "Unused local variable: " ER.get_loc
      | Message sl ->
          List.iter sl ~f:(fun (_, payload) ->
              match payload with MLit _ -> () | MVar x -> mark_used' x)
      | Constr (_, tys, es) ->
          List.iter tys ~f:(fun ty -> mark_used_ty ty);
          List.iter es ~f:(fun e -> mark_used' e)
      | App (f, actuals) ->
          mark_used' f;
          List.iter actuals ~f:(fun act -> mark_used' act)
      | TApp (f, tys) ->
          mark_used' f;
          List.iter tys ~f:mark_used_ty
      | MatchExpr (x, plist) ->
          mark_used' x;
          List.iter plist ~f:(fun (pat, exp') ->
              let bounds = get_pattern_bounds pat in
              let local_dict = ref (make_dict ()) in
              List.iter bounds ~f:(fun bound ->
                  local_dict :=
                    insert_unique (as_error_string bound)
                      (false, get_rep bound)
                      !local_dict);
              expr_iter exp' (local_dict :: local_dicts);
              find_unused !local_dict "Unused Variable: " ER.get_loc)
      | Builtin (_, tys, actuals) ->
          List.iter tys ~f:mark_used_ty;
          List.iter actuals ~f:(fun act -> mark_used' act)
      | Fixpoint (_, ty, e) | Fun (_, ty, e) ->
          expr_iter e local_dicts;
          mark_used_ty ty
      | TFun (_, e) | GasExpr (_, e) -> expr_iter e local_dicts
    in

    (* Iterate through stmts to look for use of
       - Mutable fields
       - Pattern binders/Local variables
       - Defined libraries' functions
       - Imported libraries
    *)
    let rec stmt_iter stmts local_dicts =
      let mark_used' x =
        if not @@ mark_used cfields_dict x then
          if not @@ mark_used libvar_dict x then
            if not @@ mark_used_elibs x then
              List.iter local_dicts ~f:(fun dict ->
                  let _ = mark_used dict x in
                  ())
      in

      List.iter stmts ~f:(fun (s, _) ->
          match s with
          | Load (_, f) -> mark_used' f
          | RemoteLoad (_, f1, f2) | Store (f1, f2) ->
              mark_used' f1;
              mark_used' f2
          | MapUpdate (f, i1, i2) -> (
              mark_used' f;
              List.iter i1 ~f:mark_used';
              match i2 with Some i -> mark_used' i | None -> ())
          | MapGet (_, f, i1, _) | RemoteMapGet (_, _, f, i1, _) ->
              mark_used' f;
              List.iter i1 ~f:mark_used'
          | Bind (_, expr) -> expr_iter expr local_dicts
          | CallProc (p, actuals) ->
              let _ = mark_used proc_dict p in
              ();
              List.iter actuals ~f:mark_used'
          | MatchStmt (x, plist) ->
              mark_used' x;
              List.iter plist ~f:(fun (pat, stmts') ->
                  let bounds = get_pattern_bounds pat in
                  let local_dict = ref (make_dict ()) in
                  List.iter bounds ~f:(fun bound ->
                      local_dict :=
                        insert_unique (as_error_string bound)
                          (false, get_rep bound)
                          !local_dict);
                  stmt_iter stmts' (local_dict :: local_dicts);
                  find_unused !local_dict "Unused Variable: " ER.get_loc)
          | Iterate (l, p) ->
              let _ = mark_used proc_dict p in
              ();
              mark_used' l
          | SendMsgs m -> mark_used' m
          | ReadFromBC (x, _) | CreateEvnt x -> mark_used' x
          | Throw x_o -> ( match x_o with Some x -> mark_used' x | _ -> ())
          | AcceptPayment | GasStmt _ -> ())
    in

    (* Iterate through body of components *)
    List.iter cmod.contr.ccomps ~f:(fun c ->
        (* Create local dictionaries: component params *)
        let param_dict = ref (make_dict ()) in
        List.iter c.comp_params ~f:(fun (param, ty) ->
            param_dict :=
              insert_unique (as_error_string param)
                (false, get_rep param)
                !param_dict;
            mark_used_ty ty);
        stmt_iter c.comp_body [ param_dict ];
        find_unused !param_dict "Unused local component parameter: " ER.get_loc);

    (* Iterate through expressions of fields *)
    List.iter cmod.contr.cfields ~f:(fun (_, ty, exp) ->
        expr_iter exp [];
        mark_used_ty ty);

    (* Check constraints through expressions *)
    expr_iter cmod.contr.cconstraint [];

    (* Check use of contract identifiers *)
    find_unused !proc_dict "Unused procedures: " ER.get_loc;
    find_unused !cfields_dict "Unused fields: " ER.get_loc;
    find_unused !cparams_dict "Unused contract params: " ER.get_loc;

    (* Clear contract dictionaries for checking libraries *)
    clear_dict proc_dict;
    clear_dict cfields_dict;
    clear_dict cparams_dict;

    (* Check exp from module's library *)
    match cmod.libs with
    | None -> ()
    | Some lib ->
        List.iter lib.lentries ~f:(fun l_entry ->
            match l_entry with
            | LibTyp _ -> ()
            | LibVar (_, _, e) -> expr_iter e []);

        (* Libraries can be imported just for the new lmodule definition *)
        find_unused !elibs_dict "Unused imported libraries: " SR.get_loc;
        find_unused !libvar_dict "Unused library var: " ER.get_loc;
        find_unused !libty_dict "Unused user defined ADT: " ER.get_loc
end
