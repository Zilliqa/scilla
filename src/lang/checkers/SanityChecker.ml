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
  module TU = TypeUtilities (SR) (ER)
  module SCU = ContractUtil.ScillaContractUtil (SR) (ER)

  open EISyntax
  open SCU

  (* Warning level to use when contract loads/stores entire Maps. *)
  let warning_level_map_load_store = 1

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
            if (List.exists (fun x -> get_id x = get_id i) rem)
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
    (* No repeating transition names. *)
    let e = e @ check_duplicate_ident SR.get_loc (List.map (fun t -> t.tname) contr.ctrans) in
    (* No repeating transition parameter names. *)
    let e = List.fold_left
      (fun e t -> e @ check_duplicate_ident ER.get_loc (List.map (fun (i, _) -> i) t.tparams))
       e contr.ctrans 
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
        (* This is an "event" message, and must have "_eventname" field. *)
        if List.exists (fun (s, _) -> s = eventname_label) msg
        then e
        else e @ mk_error1 ("Missing " ^ eventname_label ^ " field in message\n") eloc
      in
        pure e (* as required by "fold_over_messages" *)
    in
    let%bind e = fold_over_messages cmod ~init:e ~f:check_message in

    (* Transition parameters cannot have names as that of implicit ones. *)
    let e = List.fold_left (fun e t -> 
      match List.find_opt (fun (s, _) -> get_id s = amount_label || get_id s = sender_label) t.tparams with
      | Some (s, _) ->
        e @ mk_error1 (Core.sprintf "Parameter %s in transition %s cannot be explicit.\n" 
                          (get_id s) (get_id t.tname)) 
                      (SR.get_loc @@ get_rep t.tname)
      | None -> e
      ) e contr.ctrans in

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
    List.iter (fun trans ->
      let rec stmt_iter stmts =
        List.iter (fun (stmt, _) ->
          match stmt with
          (* Recursion basis. *)
          | Load (_, s) | Store (s, _) ->
            let t = (ER.get_type (get_rep s)).tp in
            let lc = ER.get_loc (get_rep s) in
            (match t with
            | MapType _ ->
              warn1 "Consider using in-place Map access" warning_level_map_load_store lc;
            | _ -> ()
            )
          (* Recurse through match statements. *)
          | MatchStmt (_, pat_stmts) ->
            List.iter (fun (_, stmts) ->
              stmt_iter stmts
            ) pat_stmts;
          | _ -> ()
        ) stmts;
      in
      stmt_iter trans.tbody;
    ) cmod.contr.ctrans;

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
    | MArg of (num_msg -> (num_msg, scilla_error list) result)

  let lub_num_msg n1 n2 = match n1, n2 with
    | Count i1, Count i2 -> Count (max i1 i2)
    | _ -> Many

  (* lattice element printer for debugging. *)
  let num_msg_str = function | Count i -> Printf.sprintf "Count(%d)" i | Many -> "Many" | MArg _ -> "MArg"


  (* Specialize AssocDictionary for one_msg_checker. *)
  type one_msg_env = num_msg AssocDictionary.dict
  let mk_env () = AssocDictionary.make_dict ()
  let add_env env id (nm : num_msg) = AssocDictionary.insert (get_id id) nm env
  let rem_env env id : one_msg_env = AssocDictionary.remove (get_id id) env
  let lookup_env env id : (num_msg, scilla_error list) result =
    (* Lookup "id" and raise an error if not found. *)
    match AssocDictionary.lookup (get_id id) env with
    | Some nm -> pure nm
    | None -> fail1 (Printf.sprintf "Analysis one_msg: %s not in environment" (get_id id)) (ER.get_loc (get_rep id))

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
      lookup_env env i
    | Let (i, _, lhs, rhs) ->
      let%bind n = expr_checker lhs env in
      let env' = add_env env i n in
      expr_checker rhs env'
    | Message _ ->
      (* We don't care about single messages (i.e., not in a list). *)
      pure Many
    | Fun (formal, _, body) ->
      pure @@ MArg (fun n ->
        let env' = add_env env formal n in
        expr_checker body env'
      )
    | App (f, actuals) ->
      let%bind n = lookup_env env f in
        (* Apply each argument, one at a time. *)
        foldM ~f:(fun acc actual ->
          let%bind n' = lookup_env env actual in
          (match acc with
            | MArg f' ->
              f' n'
            | _ -> pure Many)
        ) ~init:n actuals
    | Constr (cname, _, actuals) ->
      let msg_list_t = ADT("List", [PrimTypes.msg_typ]) in
      if (ER.get_type erep).tp = msg_list_t
      then
        if cname = "Nil" then pure (Count 0)
        else (* Cons *)
          let c = List.nth actuals 1 in
          let%bind n = lookup_env env c in
          (match n with
          | Count i -> pure (Count (i+1))
          | _ -> pure Many)
      else
        pure Many
    | MatchExpr (m, clauses) ->
      let%bind mn = lookup_env env m in
        foldM ~f:(fun acc (p, e') ->
          let env' = pattern_checker mn p env in
          let%bind n = expr_checker e' env' in
          pure @@ lub_num_msg acc n
        ) ~init:(Count 0) clauses
    | Builtin _ -> pure Many
    | Fixpoint _ -> pure Many
    | TFun (_, e') -> expr_checker e' env
    | TApp (tf, _) ->
      lookup_env env tf

  let rec stmt_checker env stmts =
    match stmts with
    | [] -> pure ()
    | (s, srep) :: sts -> (match s with
      | Load (x, _) | MapGet (x, _, _, _) | ReadFromBC (x, _)->
        let env' = add_env env x Many in
        stmt_checker env' sts
      | Store _ | MapUpdate _
      | AcceptPayment | CreateEvnt _ | Throw _ ->
        stmt_checker env sts
      | Bind (x, e) ->
        let%bind n = expr_checker e env in
        let env' = add_env env x n in
        stmt_checker env' sts
      | MatchStmt (m, clauses) ->
        let%bind _ = 
          let%bind mn = lookup_env env m in
            foldM ~f:(fun _ (p, stmts') ->
              let env' = pattern_checker mn p env in
              stmt_checker env' stmts'
            ) ~init:() clauses
        in
        stmt_checker env sts
      | SendMsgs ms ->
        let%bind ns = lookup_env env ms in
        (match ns with
        | Count i when i <= 1 -> stmt_checker env sts
        (* We do not raise an error because we do not track List{Message} through other ADTs,
         * so we can't say for sure that the contract is sending more than one message.
         * Rest assured, there's a run-time check against this. *)
        | _ -> warn1 "Unable to validate that send arguments is null or single message" 1 (SR.get_loc srep);
          stmt_checker env sts
        )
    )

  let one_msg_checker (cmod : cmodule) rlibs elibs =
    let elib_entries = List.fold_left (fun acc lib -> acc @ lib.lentries) [] elibs in
    (* Bind folds to Many as they aren't much useful. *)
    let env_rec = List.fold_left (fun acc le ->
        match le with
        | LibVar (i, _) -> add_env acc i Many
        | LibTyp _ -> acc
      ) (mk_env()) rlibs
    in
    (* First scan the library functions. *)
    let%bind env_libs = match cmod.libs with
      | Some lib ->
        foldM ~f:(fun aenv lentry ->
          match lentry with
          | LibTyp _ -> pure aenv
          | LibVar (x, e) ->
            let%bind n = expr_checker e aenv in
            pure @@ add_env aenv x n
        ) ~init:env_rec (elib_entries @ lib.lentries)
      | None -> pure @@ mk_env ()
    in
    (* Bind contract parameters to Many. *)
    let env = List.fold_left (fun acc (p, _) ->
        add_env acc p Many
      ) env_libs (SCU.append_implict_contract_params cmod.contr.cparams)
    in
    (* Check all transitions. *)
    foldM ~f:(fun _ tr ->
        (* Bind transition parameters. *)
      let env' = List.fold_left (fun acc (p, _) ->
          add_env acc p Many
        ) env (SCU.append_implict_trans_params tr.tparams)
      in
      stmt_checker env' tr.tbody
    ) ~init:() cmod.contr.ctrans

  (* ************************************** *)
  (* ******** Interface to Checker ******** *)
  (* ************************************** *)

  let contr_sanity (cmod : cmodule) (rlibs : lib_entry list) (elibs : library list) =
    let%bind _ = basic_sanity cmod in
    one_msg_checker cmod rlibs elibs

  (* ************************************** *)
  (* ******* Library name conflicts ******* *)
  (* ************************************** *)

  (* If a name defined in a library is again defined later in another one, error. *)
  let check_library_nameclashes elibs =
    (* Iterate through each library, noting down names seen. *)
    let%bind _ = foldM ~f:(fun acc elib ->
      foldM ~f:(fun acc lentry ->
        match lentry with
        | LibVar (name, _) ->
          (match List.find_opt 
            (* Is this a name clash with an identifier from another library? *)
            (fun (i, libi) -> (get_id i) = (get_id name) && (get_id libi) <> (get_id elib.lname)) acc with
          | Some (name', elib') ->
            (* We've seen this name in another library, report error. *)
            fail1 (Printf.sprintf "Identifier %s in library %s was defined earlier in library %s" 
                    (get_id name') (get_id elib.lname) (get_id elib'))
                  (ER.get_loc (get_rep name))
            (* Name not seen before, add it to our list of tracked names. *)
          | None -> pure @@ (name, elib.lname) :: acc
          )
        | LibTyp _ -> pure acc
      ) ~init:acc elib.lentries
    ) ~init:[] elibs
    in pure ()

end
