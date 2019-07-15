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

open TypeUtil
open Syntax
open ErrorUtils
open MonadUtil
open Utils

open ContractUtil.MessagePayload
open Core.Result.Let_syntax

module ScillaSanityChecker
    (SR : Rep)
    (ER : sig
       include Rep
       val get_type : rep -> PlainTypes.t inferred_type
     end) = struct

  module SER = SR
  module EER = ER
  module EISyntax = ScillaSyntax (SR) (ER)
  module TU = TypeUtilities
  module SCU = ContractUtil.ScillaContractUtil (SR) (ER)

  open EISyntax
  open SCU

  (* Warning level to use when contract loads/stores entire Maps. *)
  let warning_level_map_load_store = 1
  (* Warning level to use when warning about shadowing of contract parameters and fields. *)
  let warning_level_name_shadowing = 2

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
            if is_mem_id i rem
            then
              e @ mk_error1 (Core.sprintf "Identifier %s used more than once\n" (get_id i)) (gloc @@ get_rep i)
            else e
          in
            recurser rem e'
        | [] -> e
      in
        recurser ilist []
    in

    (* No repeating names for params. *)
    let e = check_duplicate_ident ER.get_loc (List.map (fun (i, _) -> i) contr.cparams) in
    (* No repeating field names. *)
    let e = e @ check_duplicate_ident ER.get_loc (List.map (fun (i, _, _) -> i) contr.cfields) in
    (* No repeating component names. *)
    let e = e @ check_duplicate_ident SR.get_loc (List.map (fun c -> c.comp_name) contr.ccomps) in
    (* No repeating component parameter names. *)
    let e = List.fold_left
      (fun e t -> e @ check_duplicate_ident ER.get_loc (List.map (fun (i, _) -> i) t.comp_params))
       e contr.ccomps 
    in

    (* Message literals must either be for "send" or "event" and well formed. *)
    let check_message b msg e =
      (* Use location of "b" to represent the location of msg. *)
      let eloc = ER.get_loc @@ get_rep b in

      (* No repeating message field. *)
      let e = e @ check_duplicate_ident (fun _ -> eloc) (List.map (fun (s, _) -> SR.mk_id_string s) msg) in

      (* Either "_tag" or "_eventname" must be present. *)
      let e = if (List.exists (fun (s, _) -> s = tag_label) msg)
      then
        (* This is a "send" Message. Ensure "_amount" and "_recipient" provided. *)
        if List.exists (fun (s, _) -> s = amount_label) msg &&
           List.exists (fun (s, _) -> s = recipient_label) msg 
        then e 
        else e @ mk_error1 ("Missing " ^ amount_label ^ " or " ^ recipient_label ^ " in Message\n") eloc
      else
        (* It must be an event or an exception. *)
        if List.exists (fun (s, _) -> s = eventname_label || s = exception_label) msg
        then e
        else e @ mk_error1 ("Invalid message construct.") eloc
      in
        pure e (* as required by "fold_over_messages" *)
    in
    let%bind e = fold_over_messages cmod ~init:e ~f:check_message in

    (* Component parameters cannot have names as that of implicit ones. *)
    let e = List.fold_left (fun e c -> 
      match List.find_opt (fun (s, _) -> get_id s = amount_label || get_id s = sender_label) c.comp_params with
      | Some (s, _) ->
        e @ mk_error1 (Core.sprintf "Parameter %s in %s %s cannot be explicit.\n" 
                         (get_id s)
                         (component_type_to_string c.comp_type)
                         (get_id c.comp_name)) 
                      (SR.get_loc @@ get_rep c.comp_name)
      | None -> e
      ) e contr.ccomps in

    (* Contract parameters cannot have names of implicit ones. *)
    let e = 
      match (List.find_opt (fun (s, _) ->
          (get_id s = ContractUtil.creation_block_label) || (get_id s = ContractUtil.scilla_version_label)
          || (get_id s = ContractUtil.this_address_label)
        ) contr.cparams) with
      | Some (s, _) ->
        e @ mk_error1 (Core.sprintf "Contract parameter %s cannot be explicit.\n" (get_id s))
            (ER.get_loc @@ get_rep s) 
      | None -> e
    in

    (* Look for any statement that is loading/storing a full Map and warn. *)
    let check_typ_warn s =
      let t = (ER.get_type (get_rep s)).tp in
      let lc = ER.get_loc (get_rep s) in
      (match t with
      | MapType _
      (* The result of a <- a[][], i.e., "a" is an Option type. *)
      | ADT("Option", [MapType _]) ->
        warn1 "Consider using in-place Map access" warning_level_map_load_store lc;
      | _ -> ()
      )
    in
    List.iter (fun comp ->
      let rec stmt_iter stmts =
        List.iter (fun (stmt, _) ->
          match stmt with
          (* Recursion basis. *)
          | Load (_, s) | Store (s, _) | MapGet (s, _, _, _)->
            check_typ_warn s
          | MapUpdate (_, _, vopt) ->
            (match vopt with
            | Some s -> check_typ_warn s
            | None -> ()
            )
          (* Recurse through match statements. *)
          | MatchStmt (_, pat_stmts) ->
            List.iter (fun (_, stmts) ->
              stmt_iter stmts
            ) pat_stmts;
          | _ -> ()
        ) stmts;
      in
      stmt_iter comp.comp_body;
    ) cmod.contr.ccomps;

    if e = [] then pure () else fail e

  (* ************************************** *)
  (* ********* One Message Checker ******** *)
  (* ************************************** *)

  (* Check that all "send" arguments are List {Message} of length 0 or 1.
   * This check must be removed once https://github.com/Zilliqa/scilla/issues/387 is complete.
   *)
  type num_msg =
    | Count of int
    | Many
    | MArg of (num_msg list -> (num_msg, scilla_error list) result)

  let lub_num_msg n1 n2 = match n1, n2 with
    | Count i1, Count i2 -> Count (max i1 i2)
    | _ -> Many

  (* lattice element printer for debugging. *)
  let num_msg_str = function | Count i -> Printf.sprintf "Count(%d)" i | Many -> "Many" | MArg _ -> "MArg"


  (* Specialize AssocDictionary for one_msg_checker. *)
  type one_msg_env = num_msg AssocDictionary.dict
  let mk_env () = AssocDictionary.make_dict ()
  let add_env env id (nm : num_msg) = AssocDictionary.insert (get_id id) nm env
  let add_env_list env idl (nml : num_msg list) =
    List.fold_left2 (fun accenv id nm -> add_env accenv id nm) env idl nml
  let rem_env env id : one_msg_env = AssocDictionary.remove (get_id id) env
  let filter_env env ~f : one_msg_env = AssocDictionary.filter ~f env
  let append_env env env' : one_msg_env =
    let lenv' = AssocDictionary.to_list env' in
    List.fold_left (fun acc (k, v) -> AssocDictionary.insert k v acc) env lenv'

  let lookup_env env id loc : (num_msg, scilla_error list) result =
    (* Lookup "id" and raise an error if not found. *)
    match AssocDictionary.lookup (get_id id) env with
    | Some nm -> pure nm
    | None -> fail1 (Printf.sprintf "Analysis one_msg: %s not in environment" (get_id id)) loc

  (* env printer for debugging. *)
  let string_env env =
    let l = AssocDictionary.to_list env in
    List.fold_left (fun s (n, v) -> s ^ Printf.sprintf "%s : %s\n" n (num_msg_str v)) "one_msg_env:\n" l

  (* This doesn't do much except check for a List {Message}, tagged Count(i),
   * matched with a Cons constructor, in which case, the bound variable will
   * have Count(i-1) tagged to it *)
  let rec pattern_checker m p env =
    match p with
    | Wildcard -> env
    | Binder i -> add_env env i m 
    | Constructor (cn, pl) ->
      (match cn, m with
      | "Cons", Count i when i > 0 ->
        let env' = pattern_checker Many (List.nth pl 0) env in
        let env'' = pattern_checker (Count (i-1)) (List.nth pl 1) env' in
        env''
      | _ ->
        (* Just bind all identifiers to Many. *)
        List.fold_left (fun acc_env p' ->
          pattern_checker Many p' acc_env
        ) env pl
      )

  let rec expr_checker (e, erep) env = match e with
    | Literal _ ->
      (* Msg literals are not built until Eval. *)
      pure Many
    | Var i ->
      lookup_env env i (ER.get_loc (get_rep i))
    | Let (i, _, lhs, rhs) ->
      let%bind n = expr_checker lhs env in
      let env' = add_env env i n in
      expr_checker rhs env'
    | Message _ ->
      (* We don't care about single messages (i.e., not in a list). *)
      pure Many
    | Fun (formal, _, body) ->
      pure @@ MArg (fun n ->
        let env' = add_env_list env [formal] n in
        expr_checker body env'
      )
    | App (f, actuals) ->
      let%bind n = lookup_env env f (ER.get_loc (get_rep f)) in
        (* Apply each argument, one at a time. *)
        foldM ~f:(fun acc actual ->
          let%bind n' = lookup_env env actual (ER.get_loc (get_rep actual)) in
          (match acc with
            | MArg f' ->
              f' [n']
            | _ -> pure Many)
        ) ~init:n actuals
    | Constr (cname, _, actuals) ->
      let msg_list_t = ADT("List", [PrimTypes.msg_typ]) in
      if (ER.get_type erep).tp = msg_list_t
      then
        if cname = "Nil" then pure (Count 0)
        else (* Cons *)
          let c = List.nth actuals 1 in
          let%bind n = lookup_env env c (ER.get_loc (get_rep c))in
          (match n with
          | Count i -> pure (Count (i+1))
          | _ -> pure Many)
      else
        pure Many
    | MatchExpr (m, clauses) ->
      let%bind mn = lookup_env env m (ER.get_loc (get_rep m)) in
        foldM ~f:(fun acc (p, e') ->
          let env' = pattern_checker mn p env in
          let%bind n = expr_checker e' env' in
          pure @@ lub_num_msg acc n
        ) ~init:(Count 0) clauses
    | Builtin _ -> pure Many
    | Fixpoint _ -> pure Many
    | TFun (_, e') -> expr_checker e' env
    | TApp (tf, _) ->
      lookup_env env tf (ER.get_loc (get_rep tf))

  let rec stmt_checker env stmts =
    match stmts with
    | [] -> pure ()
    | (s, srep) :: sts -> (match s with
      | Load (x, _) | MapGet (x, _, _, _) | ReadFromBC (x, _)->
        let env' = add_env env x Many in
        stmt_checker env' sts
      | Store _ | MapUpdate _
      | AcceptPayment | CreateEvnt _ | Throw _ -> stmt_checker env sts
      | CallProc (p, actuals) ->
        let%bind actuals' = mapM ~f:(fun actual ->
          lookup_env env actual (ER.get_loc (get_rep actual))
        ) actuals in
        let%bind p' = lookup_env env p (SR.get_loc (get_rep p)) in
        (match p' with
        | MArg f' ->
          let%bind _ = f' actuals' in
          pure ()
        | _ -> pure ())
      | Bind (x, e) ->
        let%bind n = expr_checker e env in
        let env' = add_env env x n in
        stmt_checker env' sts
      | MatchStmt (m, clauses) ->
        let%bind _ = 
          let%bind mn = lookup_env env m (ER.get_loc (get_rep m)) in
            foldM ~f:(fun _ (p, stmts') ->
              let env' = pattern_checker mn p env in
              stmt_checker env' stmts'
            ) ~init:() clauses
        in
        stmt_checker env sts
      | SendMsgs ms ->
        let%bind ns = lookup_env env ms (ER.get_loc (get_rep ms)) in
        (match ns with
        | Count i when i <= 1 -> stmt_checker env sts
        (* We do not raise an error because we do not track List{Message} through other ADTs,
         * so we can't say for sure that the contract is sending more than one message.
         * Rest assured, there's a run-time check against this. *)
        | _ -> warn1 "Unable to validate that send arguments is null or single message" 1 (SR.get_loc srep);
          stmt_checker env sts
        )
    )

  let library_checker elib env =
    foldM ~f:(fun aenv lentry ->
      match lentry with
      | LibTyp _ -> pure aenv
      | LibVar (x, _, e) ->
        let%bind n = expr_checker e aenv in
        pure @@ add_env aenv x n
    ) ~init:env elib.lentries

  let one_msg_checker (cmod : cmodule) rlibs elibs =
    (* Bind folds to Many as they aren't much useful. *)
    let env_rec = List.fold_left (fun acc le ->
        match le with
        | LibVar (i, _, _) -> add_env acc i Many
        | LibTyp _ -> acc
      ) (mk_env()) rlibs
    in
    (* First scan the external library functions. *)
    let%bind env_elibs =
      let rec recurser elibl =
        foldM ~f:(fun acc_env elib ->
          let%bind dep_env = recurser elib.deps in
          let%bind lib_env = library_checker elib.libn dep_env in
          (* Retain only env entries from elib. *)
          let lib_env' = filter_env lib_env ~f:(fun name ->
            List.exists (function | LibTyp _ -> false | LibVar (i, _, _) -> get_id i = name) elib.libn.lentries
          ) in
          let acc_env' = append_env acc_env lib_env' in
          pure acc_env'
        ) ~init:env_rec elibl
      in
      recurser elibs
    in
    (* Scan contract library if it exists. *)
    let%bind env_libs =
      match cmod.libs with
      | Some lib ->
        let%bind lib_env = library_checker lib env_elibs in
        pure lib_env
      | None -> pure env_elibs
    in

    (* Bind contract parameters to Many. *)
    let env = List.fold_left (fun acc (p, _) ->
        add_env acc p Many
      ) env_libs (SCU.append_implict_contract_params cmod.contr.cparams)
    in
    (* Check all components. *)
    let%bind _ = foldM ~f:(fun accenv cp ->
        match cp.comp_type with
        | CompTrans ->
          (* Bind transition parameters to Many. *)
          let params = SCU.append_implict_comp_params cp.comp_params in
          let env' = List.fold_left (fun acc (p, _) ->
              add_env acc p Many
            ) accenv params
          in
          let%bind () = stmt_checker env' cp.comp_body in
          pure accenv
        | CompProc ->
          let check_proc_clos = MArg (fun nmlist ->
            let params' = fst @@ Core.List.unzip @@ SCU.append_implict_comp_params cp.comp_params in
            let nmlist' = [Many; Many] @ nmlist in (* For the two appeneded implicit comp params above. *)
            (* Analyze the body *)
            let env' = add_env_list accenv params' nmlist' in
            let%bind _ = stmt_checker env' cp.comp_body in
            pure Many (* This doesn't matter because the procedure doesn't return a value. *)
          ) in
          (* bind check_proc_clos to this procedure. *)
          pure @@ add_env accenv cp.comp_name check_proc_clos
      ) ~init:env cmod.contr.ccomps
    in
    pure ()

  (* ************************************** *)
  (* ********* Warn name shadowing ******** *)
  (* ************************************** *)

  let warn_shadowing cmod =

    (* A utility function that checks if "id" is shadowing cparams, cfields or pnames. *)
    let check_warn_redef cparams cfields pnames id =
      if List.mem (get_id id) cparams
      then (
        warn1 (Printf.sprintf "Name %s shadows a contract parameter." (get_id id))
        warning_level_name_shadowing
        (ER.get_loc (get_rep id));
      ) else if List.mem (get_id id) cfields
      then (
        warn1 (Printf.sprintf "Name %s shadows a field declaration." (get_id id))
        warning_level_name_shadowing
        (ER.get_loc (get_rep id));
      ) else if List.mem (get_id id) pnames
      then (
        warn1 (Printf.sprintf "Name %s shadows a transition parameter." (get_id id))
        warning_level_name_shadowing
        (ER.get_loc (get_rep id));
      )
    in

    let cparams = List.map (fun (p, _) -> get_id p) cmod.contr.cparams in
    (* Check if a field shadows any contract parameter. *)
    List.iter (fun (f, _, _) -> check_warn_redef cparams [] [] f) cmod.contr.cfields;
  
    let cfields = List.map (fun (f, _, _) -> get_id f) cmod.contr.cfields in

    (* Go through each component. *)
    List.iter (fun c ->

    (* 1. If a parameter name shadows one of cparams or cfields, warn. *)
      List.iter (fun (p, _) -> check_warn_redef cparams cfields [] p) c.comp_params;
      let pnames = List.map (fun (p, _) -> get_id p) c.comp_params in

      (* Check for shadowing in patterns. *)
      let rec pattern_iter = function
        | Wildcard -> ()
        | Binder i -> check_warn_redef cparams cfields pnames i
        | Constructor (_, plist) ->
          List.iter (fun pat -> pattern_iter pat) plist
      in

      (* Check for shadowing in expressions. *)
      let rec expr_iter (e, _) =
        match e with
        | Literal _ | Builtin _ | Constr _ | App _ | Message _ 
        | Var _ | TApp _ -> ()
        | Let (i, _, e_lhs, e_rhs) ->
          check_warn_redef cparams cfields pnames i;
          expr_iter e_lhs;
          expr_iter e_rhs
        | Fun (i, _, e_body)
        | Fixpoint (i, _, e_body)
        | TFun (i, e_body) ->
          (* "i" being a type variable shouldn't be shadowing contract parameters,
            fields or component parameters. This is just a conservative check. *)
          check_warn_redef cparams cfields pnames i;
          expr_iter e_body
        | MatchExpr (_, clauses) ->
          List.iter (fun (pat, mbody) ->
            pattern_iter pat;
            expr_iter mbody
          ) clauses
      in

      (* Check for shadowing in statements. *)
      let rec stmt_iter stmts =
        List.iter (fun (s, _) ->
          match s with
          | Load (x, _) | MapGet (x, _, _, _) | ReadFromBC (x, _) ->
            check_warn_redef cparams cfields pnames x;
          | Store _ | MapUpdate _ | SendMsgs _
          | AcceptPayment | CreateEvnt _ | Throw _
          | CallProc _ ->
            ()
          | Bind (x, e) ->
            check_warn_redef cparams cfields pnames x;
            expr_iter e
          | MatchStmt (_, clauses) ->
            List.iter (fun (pat, mbody) ->
              pattern_iter pat;
              stmt_iter mbody
            ) clauses
        ) stmts
      in
      (* Go through all statements and see if any of cparams, cfields or pnames are redefined. *)
      stmt_iter c.comp_body
    ) cmod.contr.ccomps

  (* ************************************** *)
  (* ******** Interface to Checker ******** *)
  (* ************************************** *)

  let contr_sanity (cmod : cmodule) (rlibs : lib_entry list) (elibs : libtree list) =
    let%bind _ = basic_sanity cmod in
    warn_shadowing cmod;
    one_msg_checker cmod rlibs elibs

end
