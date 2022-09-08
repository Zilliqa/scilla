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
open Result.Let_syntax
open TypeUtil
open Literal
open Syntax
open ErrorUtils
open MonadUtil
open ContractUtil.MessagePayload
open DeadCodeDetector
open Callgraph

module ScillaSanityChecker
    (SR : Rep) (ER : sig
      include Rep

      val get_type : rep -> PlainTypes.t inferred_type
    end) =
struct
  module SER = SR
  module EER = ER
  module SCLiteral = GlobalLiteral
  module SCType = SCLiteral.LType
  module SCIdentifier = SCType.TIdentifier
  module SCName = SCIdentifier.Name
  module SCSyntax = ScillaSyntax (SR) (ER) (SCLiteral)
  module TU = TypeUtilities
  module SCU = ContractUtil.ScillaContractUtil (SR) (ER)
  module DCD = DeadCodeDetector (SR) (ER)
  module CG = ScillaCallgraph (SR) (ER)
  open SCIdentifier
  open SCSyntax
  open SCU

  (* Warning level to use when contract loads/stores entire Maps. *)
  let warning_level_map_load_store = 1

  (* Warning level to use when warning about shadowing of contract parameters and fields. *)
  let warning_level_name_shadowing = 2

  (* Warning level to use when contract hashes maps / messages / ADTs *)
  let warning_level_hash_compound_types = 3

  (* Warning level to use when contract uses empty "_tag" in message *)
  let warning_level_empty_tag = 3

  (* Warning level to use when warning about not unboxed value from map get. *)
  let warning_level_not_unboxed = 2

  (* ************************************** *)
  (* ******** Basic Sanity Checker ******** *)
  (* ************************************** *)

  (* Basic sanity tests on the contract. *)
  let basic_sanity (cmod : cmodule) =
    let contr = cmod.contr in

    (* Check if there are duplicate entries in "ilist". *)
    let check_duplicate_ident gloc ilist =
      let rec recurser ilist' e =
        match ilist' with
        | i :: rem ->
            let e' =
              if is_mem_id i rem then
                e
                @ mk_error1 ~kind:"Identifier used more than once"
                    ~inst:(as_error_string i)
                    (gloc @@ get_rep i)
              else e
            in
            recurser rem e'
        | [] -> e
      in
      recurser ilist []
    in

    (* No repeating names for params. *)
    let e =
      check_duplicate_ident ER.get_loc
        (List.map contr.cparams ~f:(fun (i, _) -> i))
    in
    (* No repeating field names. *)
    let e =
      e
      @ check_duplicate_ident ER.get_loc
          (List.map contr.cfields ~f:(fun (i, _, _) -> i))
    in
    (* No repeating component names. *)
    let e =
      e
      @ check_duplicate_ident SR.get_loc
          (List.map contr.ccomps ~f:(fun c -> c.comp_name))
    in
    (* No repeating component parameter names. *)
    let e =
      List.fold_left contr.ccomps ~init:e ~f:(fun e t ->
          e
          @ check_duplicate_ident ER.get_loc
              (List.map t.comp_params ~f:(fun (i, _) -> i)))
    in

    (* Message literals must either be for "send" or "event" and well formed. *)
    let check_message eloc msg e =
      (* No repeating message field. *)
      let e =
        e
        @ check_duplicate_ident
            (fun _ -> eloc)
            (List.map msg ~f:(fun (s, _) ->
                 mk_id (SCName.parse_simple_name s) SR.string_rep))
      in

      (* Either "_tag" or "_eventname" must be present. *)
      let e =
        if List.Assoc.mem msg tag_label ~equal:String.( = ) then
          (* This is a "send" Message. Ensure "_amount" and "_recipient" provided. *)
          if
            List.Assoc.mem msg amount_label ~equal:String.( = )
            && List.Assoc.mem msg recipient_label ~equal:String.( = )
          then e
          else
            e
            @ mk_error1
                ~kind:
                  ("Missing " ^ amount_label ^ " or " ^ recipient_label
                 ^ " in Message")
                ?inst:None eloc
        else if
          (* It must be an event or an exception or a contract replication. *)
          List.exists msg ~f:(fun (s, _) ->
              String.(
                s = eventname_label || s = exception_label
                || s = replicate_contr_label))
        then e
        else e @ mk_error1 ~kind:"Invalid message construct" ?inst:None eloc
      in

      (* Empty "_tag" is suspicious, because when using it, refunds in exchange
         and wallet contracts are not possible. See ZRC-5 for the reference. *)
      let _ =
        match List.Assoc.find msg tag_label ~equal:String.equal with
        | Some (MLit (StringLit "")) ->
            warn1
              "Consider using \"AddFunds\" tag instead of empty tag as \
               suggested by ZRC-5"
              warning_level_empty_tag eloc
        | _ -> ()
      in

      pure e
      (* as required by "fold_over_messages" *)
    in
    let%bind e = fold_over_messages cmod ~init:e ~f:check_message in

    (* Component parameters cannot have names as that of implicit ones. *)
    let e =
      List.fold_left contr.ccomps ~init:e ~f:(fun e c ->
          match
            List.find c.comp_params ~f:(fun (s, _) ->
                String.(
                  as_string s = amount_label
                  || as_string s = sender_label
                  || as_string s = origin_label))
          with
          | Some (s, _) ->
              e
              @ mk_error1
                  ~kind:
                    (sprintf "Parameter %s in %s %s cannot be explicit.\n"
                       (as_error_string s)
                       (component_type_to_string c.comp_type)
                       (as_error_string c.comp_name))
                  ?inst:None
                  (SR.get_loc @@ get_rep c.comp_name)
          | None -> e)
    in

    (* Contract parameters cannot have names of implicit ones. *)
    let e =
      match
        List.find contr.cparams ~f:(fun (s, _) ->
            let open ContractUtil in
            [%equal: SCName.t] (get_id s) creation_block_label
            || [%equal: SCName.t] (get_id s) scilla_version_label
            || [%equal: SCName.t] (get_id s) this_address_label)
      with
      | Some (s, _) ->
          e
          @ mk_error1
              ~kind:
                (sprintf "Contract parameter %s cannot be explicit.\n"
                   (as_error_string s))
              ?inst:None
              (ER.get_loc @@ get_rep s)
      | None -> e
    in

    (* Look for any statement that is loading/storing a full Map and warn. *)
    let check_typ_warn s =
      let t = (ER.get_type (get_rep s)).tp in
      let lc = ER.get_loc (get_rep s) in
      let warn () =
        warn1 "Consider using in-place Map access" warning_level_map_load_store
          lc
      in
      match t with
      | MapType _ -> warn ()
      (* The result of a <- a[][], i.e., "a" is an Option type. *)
      | ADT (adt_name, [ MapType _ ])
        when Datatypes.is_option_adt_name (get_id adt_name) ->
          warn ()
      | _ -> ()
    in
    List.iter cmod.contr.ccomps ~f:(fun comp ->
        let rec stmt_iter stmts =
          List.iter stmts ~f:(fun (stmt, _) ->
              match stmt with
              (* Recursion basis. *)
              | Load (_, s) | MapGet (s, _, _, _) -> check_typ_warn s
              | MapUpdate (_, _, vopt) -> (
                  match vopt with Some s -> check_typ_warn s | None -> ())
              (* Recurse through match statements. *)
              | MatchStmt (_, pat_stmts) ->
                  List.iter pat_stmts ~f:(fun (_, stmts) -> stmt_iter stmts)
              | _ -> ())
        in
        stmt_iter comp.comp_body);
    if List.is_empty e then pure () else fail e

  (* ************************************** *)
  (* ******** Check name shadowing ******** *)
  (* ************************************** *)

  module CheckShadowing = struct
    (* A utility function that checks if "id" is shadowing cparams, cfields or pnames. *)
    let check_warn_redef cparams cfields pnames stmts_defs id =
      if List.mem cparams (get_id id) ~equal:[%equal: SCName.t] then
        warn1
          (Printf.sprintf "Name %s shadows a contract parameter."
             (as_error_string id))
          warning_level_name_shadowing
          (ER.get_loc (get_rep id))
      else if List.mem cfields (get_id id) ~equal:[%equal: SCName.t] then
        warn1
          (Printf.sprintf "Name %s shadows a field declaration."
             (as_error_string id))
          warning_level_name_shadowing
          (ER.get_loc (get_rep id))
      else if List.mem pnames (get_id id) ~equal:[%equal: SCName.t] then
        warn1
          (Printf.sprintf "Name %s shadows a transition parameter."
             (as_error_string id))
          warning_level_name_shadowing
          (ER.get_loc (get_rep id))
      else if List.mem stmts_defs (get_id id) ~equal:[%equal: SCName.t] then
        warn1
          (Printf.sprintf
             "%s is a new variable. It does not reassign the previously \
              defined variable."
             (as_error_string id))
          warning_level_name_shadowing
          (ER.get_loc (get_rep id))

    (* Check for shadowing in patterns. *)
    let pattern_iter pat cparams cfields pnames =
      (* Check if any variable bound in this pattern shadows cparams/cfields/pnames *)
      let rec outer_scope_iter = function
        | Wildcard -> ()
        | Binder i -> check_warn_redef cparams cfields pnames [] i
        | Constructor (_, plist) -> List.iter plist ~f:outer_scope_iter
      in
      outer_scope_iter pat;
      (* Check for shadowing of names within this pattern and warn that it is
       * deprecated. This will be disallowed (i.e., an error) in future versions.
       * https://github.com/Zilliqa/scilla/issues/687. To close this Issue:
       * Make this an error by just using fail1 below instead of warn1. *)
      let bounds = get_pattern_bounds pat in
      match List.find_a_dup ~compare:SCIdentifier.compare bounds with
      | Some v ->
          warn1
            (Printf.sprintf
               "Deprecated: variable %s shadows a previous binding in the same \
                pattern."
               (as_error_string v))
            warning_level_name_shadowing
            (ER.get_loc (get_rep v));
          pure ()
      | None -> pure ()

    (* Check for shadowing in expressions. *)
    let rec expr_iter (e, _) cparams cfields pnames =
      match e with
      | Literal _ | Builtin _ | Constr _ | App _ | Message _ | Var _ | TApp _ ->
          pure ()
      | GasExpr (_, e) -> expr_iter e cparams cfields pnames
      | Let (i, _, e_lhs, e_rhs) ->
          check_warn_redef cparams cfields pnames [] i;
          let%bind () = expr_iter e_lhs cparams cfields pnames in
          expr_iter e_rhs cparams cfields pnames
      | Fun (i, _, e_body) | Fixpoint (i, _, e_body) | TFun (i, e_body) ->
          (* "i" being a type variable shouldn't be shadowing contract parameters,
             fields or component parameters. This is just a conservative check. *)
          check_warn_redef cparams cfields pnames [] i;
          expr_iter e_body cparams cfields pnames
      | MatchExpr (_, clauses) ->
          forallM
            ~f:(fun (pat, mbody) ->
              let%bind () = pattern_iter pat cparams cfields pnames in
              expr_iter mbody cparams cfields pnames)
            clauses

    let shadowing_libentries lentries =
      forallM
        ~f:(fun lentry ->
          match lentry with
          | LibTyp _ -> pure ()
          | LibVar (_, _, vexp) -> expr_iter vexp [] [] [])
        lentries

    let rec shadowing_libtree ltree =
      let%bind () = forallM ~f:(fun dep -> shadowing_libtree dep) ltree.deps in
      shadowing_libentries ltree.libn.lentries

    let shadowing_cmod (cmod : cmodule) =
      (* Check for match pattern shadowing in library functions. *)
      let%bind () =
        match cmod.libs with
        | Some lib -> shadowing_libentries lib.lentries
        | None -> pure ()
      in

      let cparams = List.map cmod.contr.cparams ~f:(fun (p, _) -> get_id p) in

      (* Check for shadowing in contract constraint *)
      let%bind () = expr_iter cmod.contr.cconstraint cparams [] [] in

      (* Check if a field shadows any contract parameter. *)
      let%bind () =
        forallM
          ~f:(fun (f, _, finit_expr) ->
            check_warn_redef cparams [] [] [] f;
            expr_iter finit_expr cparams [] [])
          cmod.contr.cfields
      in

      let cfields =
        List.map cmod.contr.cfields ~f:(fun (f, _, _) -> get_id f)
      in

      (* Go through each component. *)
      forallM
        ~f:(fun c ->
          (* 1. If a parameter name shadows one of cparams or cfields, warn. *)
          List.iter c.comp_params ~f:(fun (p, _) ->
              check_warn_redef cparams cfields [] [] p);
          let pnames = List.map c.comp_params ~f:(fun (p, _) -> get_id p) in
          (* Check for shadowing in statements. *)
          let rec stmt_iter stmts stmt_defs =
            foldM stmts ~init:stmt_defs ~f:(fun acc_stmt_defs (s, _) ->
                match s with
                | Load (x, _)
                | RemoteLoad (x, _, _)
                | MapGet (x, _, _, _)
                | RemoteMapGet (x, _, _, _, _)
                | ReadFromBC (x, _)
                | TypeCast (x, _, _) ->
                    check_warn_redef cparams cfields pnames stmt_defs x;
                    pure (get_id x :: acc_stmt_defs)
                | Store _ | MapUpdate _ | SendMsgs _ | AcceptPayment | GasStmt _
                | CreateEvnt _ | Throw _ | CallProc _ | Iterate _ ->
                    pure acc_stmt_defs
                | Bind (x, e) ->
                    check_warn_redef cparams cfields pnames stmt_defs x;
                    let%bind () = expr_iter e cparams cfields pnames in
                    pure (get_id x :: acc_stmt_defs)
                | MatchStmt (_, clauses) ->
                    let%bind () =
                      forallM
                        ~f:(fun (pat, mbody) ->
                          let%bind () =
                            pattern_iter pat cparams cfields pnames
                          in
                          Result.ignore_m @@ stmt_iter mbody acc_stmt_defs)
                        clauses
                    in
                    pure acc_stmt_defs)
          in
          (* Go through all statements and see if any of cparams, cfields or pnames are redefined. *)
          Result.ignore_m @@ stmt_iter c.comp_body [])
        cmod.contr.ccomps

    let shadowing_lmod (lmod : lmodule) =
      (* Check for match pattern shadowing in library functions. *)
      shadowing_libentries lmod.libs.lentries
  end

  (* ********************************************** *)
  (* ******** Check hashing builtins usage ******** *)
  (* ********************************************** *)

  module CheckHashingBuiltinsUsage = struct
    let rec expr_iter (e, _annot) =
      match e with
      | Builtin ((builtin, _annot), _typ_args, args) -> (
          match builtin with
          | Builtin_sha256hash | Builtin_keccak256hash | Builtin_ripemd160hash
            ->
              forallM args ~f:(fun arg ->
                  let type_of_arg = (ER.get_type (get_rep arg)).tp in
                  match type_of_arg with
                  | MapType _ | ADT _
                  | PrimType (Msg_typ | Event_typ | Exception_typ) ->
                      warn1
                        (Printf.sprintf
                           "A hashing builtin is applied to argument \"%s\" \
                            whose compound type makes it prone to hash \
                            collisions. Consider using values of more \
                            primitive types in your hashing scheme."
                           (as_error_string arg))
                        warning_level_hash_compound_types
                        (ER.get_loc (get_rep arg));
                      pure ()
                  | _ -> pure ())
          | _ -> pure ())
      | Literal _ | Message _ | Constr _ | Var _ | TApp _ | App _ -> pure ()
      | Fun (_, _, e) | Fixpoint (_, _, e) | TFun (_, e) | GasExpr (_, e) ->
          expr_iter e
      | Let (_id, _opt_typ, e_lhs, e_rhs) ->
          let%bind () = expr_iter e_lhs in
          expr_iter e_rhs
      | MatchExpr (_id, clauses) ->
          forallM clauses ~f:(fun (_pat, mbody) -> expr_iter mbody)

    let in_libentries (rlibs : lib_entry list) =
      forallM rlibs ~f:(function
        | LibVar (_id, _opt_typ, e) -> expr_iter e
        | LibTyp _ -> pure ())

    let in_cmod (cmod : cmodule) =
      (* Check for hash builtins usage in the contract constraint *)
      let%bind () = expr_iter cmod.contr.cconstraint in

      (* Check for hash builtins in the contract's library *)
      let%bind () =
        match cmod.libs with
        | None -> pure ()
        | Some { lname = _; lentries } -> in_libentries lentries
      in

      (* Go through each field initializers *)
      let%bind () =
        forallM cmod.contr.cfields ~f:(fun (_field, _typ, init_e) ->
            expr_iter init_e)
      in

      (* Go through each procedure/transition *)
      forallM cmod.contr.ccomps ~f:(fun component ->
          (* Traverse statements *)
          let rec stmt_iter stmts =
            foldM stmts ~init:() ~f:(fun _acc (s, _) ->
                match s with
                | Bind (_, e) -> expr_iter e
                | MatchStmt (_, clauses) ->
                    forallM clauses ~f:(fun (_pat, mbody) -> stmt_iter mbody)
                | Load _ | RemoteLoad _ | MapGet _ | RemoteMapGet _
                | ReadFromBC _ | TypeCast _ | Store _ | MapUpdate _ | SendMsgs _
                | AcceptPayment | GasStmt _ | CreateEvnt _ | Throw _
                | CallProc _ | Iterate _ ->
                    pure ())
          in
          stmt_iter component.comp_body)
  end

  (* ************************************************** *)
  (* ******** Check unboxing of option results ******** *)
  (* ************************************************** *)

  module CheckUnboxing = struct
    module SCIdentifierComp = struct
      include SCIdentifier.Name
      include Comparable.Make (SCIdentifier.Name)
    end

    module SCIdentifierSet = Set.Make (SCIdentifierComp)

    let emp_ids_map = Map.empty (module SCIdentifierComp)
    let emp_ids_set = SCIdentifierSet.empty

    let is_option_name id =
      String.equal "Option" @@ SCIdentifier.Name.as_string (get_id id)

    let has_option_ty ty =
      let re = Str.regexp ".*Option.*$" in
      Str.string_match re (SType.pp_typ ty) 0

    let is_option_ty id =
      let re = Str.regexp "Option.*$" in
      Str.string_match re (SType.pp_typ (ER.get_type (get_rep id)).tp) 0

    (** Returns a list of variables from [unboxed_options] that are used in the
        expression [e] as arguments to the function that doesn't present in [m]. *)
    let rec used_in_unknown_calls_in_expr m unboxed_options (e, _annot) =
      match e with
      | Let (_id, _ty, lhs, rhs) ->
          used_in_unknown_calls_in_expr m unboxed_options lhs
          @ used_in_unknown_calls_in_expr m unboxed_options rhs
      | Fun (_id, _ty, body) ->
          used_in_unknown_calls_in_expr m unboxed_options body
      | MatchExpr (_id, arms) ->
          List.fold_left arms ~init:[] ~f:(fun acc (_pattern, ea) ->
              used_in_unknown_calls_in_expr m unboxed_options ea
              |> List.append acc)
      | App (id, args) ->
          if not @@ Map.mem m (get_id id) then
            List.fold_left args ~init:[] ~f:(fun acc arg ->
                List.fold_left unboxed_options ~init:[] ~f:(fun acc opt ->
                    if SCIdentifier.equal opt arg then
                      acc @ [ SCIdentifier.get_id opt ]
                    else acc)
                |> List.append acc)
          else []
      | TFun (_id, body) -> used_in_unknown_calls_in_expr m unboxed_options body
      | Fixpoint (_id, _ty, ea) ->
          used_in_unknown_calls_in_expr m unboxed_options ea
      | GasExpr (_, ea) -> used_in_unknown_calls_in_expr m unboxed_options ea
      | Literal _ | Builtin _ | Var _ | TApp _ | Message _ | Constr _ -> []

    (** Returns list a of variables from [unboxed_options] that are used in the
        statement [s] as arguments to the function that doesn't present in [m]. *)
    let rec used_in_unknown_calls m unboxed_options (s, _annot) =
      match s with
      | Bind (_id, ea) -> used_in_unknown_calls_in_expr m unboxed_options ea
      | MatchStmt (_id, arms) ->
          List.fold_left arms ~init:[] ~f:(fun acc (_pattern, stmts) ->
              List.fold_left stmts ~init:acc ~f:(fun acc s ->
                  used_in_unknown_calls m unboxed_options s |> List.append acc)
              |> List.append acc)
      | CallProc (id, args) ->
          if not @@ Map.mem m (get_id id) then
            List.fold_left args ~init:[] ~f:(fun acc arg ->
                List.fold_left unboxed_options ~init:[] ~f:(fun acc opt ->
                    if SCIdentifier.equal arg opt then
                      acc @ [ SCIdentifier.get_id opt ]
                    else acc)
                |> List.append acc)
          else []
      (* We shouldn't handle `forall` here, because it operates only with iterables. *)
      | Iterate _ | Load _ | RemoteLoad _ | Store _ | MapUpdate _ | MapGet _
      | RemoteMapGet _ | ReadFromBC _ | TypeCast _ | AcceptPayment | SendMsgs _
      | CreateEvnt _ | Throw _ | GasStmt _ ->
          []

    (** Returns a list of variables from [unboxed_options] that are assigned to
        one of the [fields] *)
    let rec assigned_to_optional_field fields unboxed_options (s, _annot) =
      let id_is_unboxed id =
        List.mem unboxed_options id ~equal:(fun l r ->
            SCIdentifier.Name.equal (SCIdentifier.get_id l)
              (SCIdentifier.get_id r))
      in
      let has_field f = SCIdentifierSet.mem fields (SCIdentifier.get_id f) in
      match s with
      | Store (lhs, rhs) when id_is_unboxed rhs && has_field lhs ->
          [ SCIdentifier.get_id rhs ]
      | MapUpdate (m, keys, v_opt) when has_field m ->
          let unboxed_values =
            Option.value_map v_opt ~default:[] ~f:(fun v ->
                if id_is_unboxed v then [ SCIdentifier.get_id v ] else [])
          in
          let unboxed_keys =
            List.fold_left keys ~init:[] ~f:(fun acc k ->
                if id_is_unboxed k then acc @ [ SCIdentifier.get_id k ] else acc)
          in
          unboxed_values @ unboxed_keys
      | MatchStmt (_id, arms) ->
          List.fold_left arms ~init:[] ~f:(fun acc (_pattern, stmts) ->
              List.fold_left stmts ~init:[] ~f:(fun acc s ->
                  acc @ assigned_to_optional_field fields unboxed_options s)
              |> List.append acc)
      | Store _ | MapUpdate _ | CallProc _ | Bind _ | Iterate _ | Load _
      | RemoteLoad _ | MapGet _ | RemoteMapGet _ | ReadFromBC _ | TypeCast _
      | AcceptPayment | SendMsgs _ | CreateEvnt _ | Throw _ | GasStmt _ ->
          []

    (** Returns names of variables that are matched in the expression. *)
    let rec collect_matches_in_expr m (e, _annot) =
      match e with
      | Let (_id, _ty, lhs, rhs) ->
          collect_matches_in_expr m lhs @ collect_matches_in_expr m rhs
      | Fun (_id, _ty, body) -> collect_matches_in_expr m body
      | MatchExpr (id, arms) ->
          List.fold_left arms ~init:[] ~f:(fun acc (_pattern, ea) ->
              collect_matches_in_expr m ea |> List.append acc)
          |> List.append [ get_id id ]
      | App (id, args) -> (
          match Map.find m (get_id id) with
          | Some arg_matches ->
              List.foldi args ~init:[] ~f:(fun i acc arg ->
                  if Array.length arg_matches > i && arg_matches.(i) then
                    acc @ [ get_id arg ]
                  else acc)
          | None -> [])
      | TFun (_id, body) -> collect_matches_in_expr m body
      | Fixpoint (_id, _ty, ea) -> collect_matches_in_expr m ea
      | GasExpr (_, ea) -> collect_matches_in_expr m ea
      | Literal _ | Builtin _ | Var _ | TApp _ | Message _ | Constr _ -> []

    (** Returns names of variables that are matched in the statement. *)
    let rec collect_matches_in_stmt m (s, _annot) =
      match s with
      | Bind (_id, ea) -> collect_matches_in_expr m ea
      | MatchStmt (id, arms) ->
          List.fold_left arms ~init:[] ~f:(fun acc (_pattern, stmts) ->
              List.fold_left stmts ~init:[] ~f:(fun acc sa ->
                  collect_matches_in_stmt m sa |> List.append acc)
              |> List.append acc)
          |> List.append [ get_id id ]
      | CallProc (id, args) -> (
          match Map.find m (get_id id) with
          | Some arg_matches ->
              List.foldi args ~init:[] ~f:(fun i acc arg ->
                  if Array.length arg_matches > i && arg_matches.(i) then
                    acc @ [ get_id arg ]
                  else acc)
          | None -> [])
      (* We shouldn't handle `forall` here, because it operates only with iterables. *)
      | Iterate _ -> []
      | Load _ | RemoteLoad _ | Store _ | MapUpdate _ | MapGet _
      | RemoteMapGet _ | ReadFromBC _ | TypeCast _ | AcceptPayment | SendMsgs _
      | CreateEvnt _ | Throw _ | GasStmt _ ->
          []

    (** Collects function calls that don't call type functions directly or
        undirectly. We don't handle them because that slows down the analysis. *)
    let collect_function_calls cg =
      let rec has_tfun_calls (n : CG.Node.t) =
        CG.Node.succs n
        |> List.find ~f:(fun (n : CG.Node.t) ->
               match n.ty with
               | TFunAlias | TFun -> true
               | _ -> has_tfun_calls n)
        |> Option.is_some
      in
      CG.fold_over_nodes_dfs cg ~init:[] ~f:(fun acc n ->
          match n.ty with
          | TFun | TFunAlias -> acc
          | Trans | Proc | Fun | FunAlias ->
              acc @ if not @@ has_tfun_calls n then [ get_id n.id ] else [])

    (** Returns arity of the function, mapping name |-> index for arguments
          with the Optional type and body expression of the function. *)
    let inspect_lib_fun ea =
      let rec aux cnt option_args ea =
        let e, _annot = ea in
        match e with
        | Fun (id, _, ea) | Fixpoint (id, _, ea) ->
            let option_args =
              if is_option_ty id then
                Map.set option_args ~key:(get_id id) ~data:cnt
              else option_args
            in
            aux (cnt + 1) option_args ea
        | TFun _ | MatchExpr _ | Let _ | GasExpr _ | Literal _ | Builtin _
        | Var _ | TApp _ | App _ | Message _ | Constr _ ->
            (cnt, option_args, ea)
      in
      aux 0 emp_ids_map ea

    (** Collects a mapping with information which argument of a library
        function or a procedure with the [Optional] type matches inside its
        body. *)
    let collect_option_args_matches (cmod : cmodule) (cg : CG.cg) =
      (* Returns an array with information about matched Optional arguments
         [Some(args)] if the [fun_name] is a pure library function. *)
      let handle_lentries lentries option_args_matches fun_name =
        List.find_map lentries ~f:(function
          | LibVar (name, _ty, e)
            when SCIdentifier.Name.equal fun_name (get_id name) ->
              let arity, option_args, body = inspect_lib_fun e in
              let args_list = Array.init arity ~f:(fun _ -> false) in
              collect_matches_in_expr option_args_matches body
              |> List.iter ~f:(fun matched ->
                     match Map.find option_args matched with
                     | Some idx -> Array.set args_list idx true
                     | None -> ());
              Some args_list
          | LibVar _ | LibTyp _ -> None)
      in
      (* Returns an array with information about matched Optional arguments
         [Some(args)] if the [fun_name] is a procedure. *)
      let handle_comp (cmod : cmodule) option_args_matches fun_name =
        let get_comp_args comp =
          match comp.comp_type with
          | CompProc ->
              let args_list =
                Array.init (List.length comp.comp_params) ~f:(fun _ -> false)
              in
              let option_args (* name |-> idx *) =
                List.foldi comp.comp_params ~init:emp_ids_map
                  ~f:(fun i m (param_id, param_ty) ->
                    match param_ty with
                    | ADT (id, _targs) when is_option_name id ->
                        Map.set m ~key:(get_id param_id) ~data:i
                    | ADT _ | PrimType _ | MapType _ | FunType _ | TypeVar _
                    | PolyFun _ | Unit | Address _ ->
                        m)
              in
              (* Mark Option arguments that matches inside the body. *)
              List.iter comp.comp_body ~f:(fun stmt ->
                  collect_matches_in_stmt option_args_matches stmt
                  |> List.iter ~f:(fun matched ->
                         match Map.find option_args matched with
                         | Some idx -> Array.set args_list idx true
                         | None -> ()));
              Some args_list
          | CompTrans -> None
        in
        List.find_map cmod.contr.ccomps ~f:(fun comp ->
            if SCIdentifier.Name.equal (get_id comp.comp_name) fun_name then
              get_comp_args comp
            else None)
      in
      let lentries =
        Option.value_map cmod.libs ~default:[] ~f:(fun lib -> lib.lentries)
      in
      collect_function_calls cg
      |> List.fold_left
           ~init:(Map.empty (module SCIdentifierComp))
           ~f:(fun m (fun_name : SCIdentifierComp.t) ->
             match handle_lentries lentries m fun_name with
             | Some arg_matches -> Map.set m ~key:fun_name ~data:arg_matches
             | None -> (
                 match handle_comp cmod m fun_name with
                 | Some arg_matches -> Map.set m ~key:fun_name ~data:arg_matches
                 | None -> m))

    (** Collects contract fields with the [Optional] type. *)
    let collect_optional_fields (cmod : cmodule) =
      List.fold_left ~init:emp_ids_set cmod.contr.cfields
        ~f:(fun s (id, ty, _init) ->
          if has_option_ty ty then Set.add s (SCIdentifier.get_id id) else s)

    let collect_variables_from_map_get (s, _annot) =
      match s with
      | MapGet (v, _, _, true) | RemoteMapGet (v, _, _, _, true) -> [ v ]
      | MapGet _ | RemoteMapGet _ | Load _ | RemoteLoad _ | Store _ | Bind _
      | MapUpdate _ | MatchStmt _ | ReadFromBC _ | TypeCast _ | AcceptPayment
      | Iterate _ | SendMsgs _ | CreateEvnt _ | CallProc _ | Throw _ | GasStmt _
        ->
          []

    (** Collects different names for the not unboxed option values. *)
    let collect_aliases (s, _annot) unboxed_options =
      match s with
      | Bind (bind_id, (e, _annot)) -> (
          match e with
          | Var id ->
              if
                List.find unboxed_options ~f:(fun o ->
                    SCIdentifier.Name.equal (SCIdentifier.get_id id)
                      (SCIdentifier.get_id o))
                |> Option.is_some
              then [ bind_id ]
              else []
          | _ -> [])
      | MapGet _ | RemoteMapGet _ | Load _ | RemoteLoad _ | Store _
      | MapUpdate _ | MatchStmt _ | ReadFromBC _ | TypeCast _ | AcceptPayment
      | Iterate _ | SendMsgs _ | CreateEvnt _ | CallProc _ | Throw _ | GasStmt _
        ->
          []

    (** Collects not matched local variables returned from map get operations
        that should be reported. *)
    let collect_not_unboxed optional_fields (comp : component) matched_args =
      let rec aux stmts unboxed_options =
        match stmts with
        | [] -> unboxed_options
        | s :: ss ->
            let filter_set =
              collect_matches_in_stmt matched_args s
              |> List.append
                 @@ used_in_unknown_calls matched_args unboxed_options s
              |> List.append
                 @@ assigned_to_optional_field optional_fields unboxed_options s
              |> SCIdentifierSet.of_list
            in
            List.filter unboxed_options ~f:(fun v ->
                not @@ Set.mem filter_set (get_id v))
            |> List.append @@ collect_variables_from_map_get s
            |> fun unboxed_options' ->
            unboxed_options' @ collect_aliases s unboxed_options' |> aux ss
      in
      aux comp.comp_body []

    let report_not_unboxed unboxed_variables =
      List.iter unboxed_variables ~f:(fun v ->
          warn1
            (Printf.sprintf
               "Variable %s has the Optional type, but it wasn't unboxed. \
                Probably, you should match it before using it."
               (Name.as_string (get_id v)))
            warning_level_not_unboxed
            (ER.get_loc (get_rep v)))

    let run (cmod : cmodule) (cg : CG.cg) (_rlibs : lib_entry list) =
      let matched_args = collect_option_args_matches cmod cg in
      let optional_fields = collect_optional_fields cmod in
      List.rev cmod.contr.ccomps
      |> List.iter ~f:(fun comp ->
             collect_not_unboxed optional_fields comp matched_args
             |> report_not_unboxed);
      pure ()
  end

  (* ************************************** *)
  (* ******** Interface to Checker ******** *)
  (* ************************************** *)

  let contr_sanity (cg : CG.cg) (cmod : cmodule) (rlibs : lib_entry list)
      (elibs : libtree list) =
    let%bind () = basic_sanity cmod in
    let%bind () = CheckShadowing.shadowing_libentries rlibs in
    let%bind () = forallM ~f:CheckShadowing.shadowing_libtree elibs in
    let%bind () = CheckShadowing.shadowing_cmod cmod in
    let%bind () = CheckHashingBuiltinsUsage.in_libentries rlibs in
    let%bind () = CheckHashingBuiltinsUsage.in_cmod cmod in
    let%bind () = CheckUnboxing.run cmod cg rlibs in
    DCD.dc_cmod cmod elibs;
    pure ()

  let lmod_sanity (lmod : lmodule) (rlibs : lib_entry list)
      (elibs : libtree list) =
    let%bind () = CheckShadowing.shadowing_libentries rlibs in
    let%bind () = forallM ~f:CheckShadowing.shadowing_libtree elibs in
    let%bind () = CheckShadowing.shadowing_lmod lmod in
    pure ()
end
