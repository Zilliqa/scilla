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
open ContractUtil.MessagePayload
open Core_kernel.Result.Let_syntax

module ScillaSanityChecker
    (SR : Rep) (ER : sig
      include Rep

      val get_type : rep -> PlainTypes.t inferred_type
    end) =
struct
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
              if is_mem_id i rem then
                e
                @ mk_error1
                    (Core_kernel.sprintf "Identifier %s used more than once\n"
                       (get_id i))
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
        (List.map (fun (i, _) -> i) contr.cparams)
    in
    (* No repeating field names. *)
    let e =
      e
      @ check_duplicate_ident ER.get_loc
          (List.map (fun (i, _, _) -> i) contr.cfields)
    in
    (* No repeating component names. *)
    let e =
      e
      @ check_duplicate_ident SR.get_loc
          (List.map (fun c -> c.comp_name) contr.ccomps)
    in
    (* No repeating component parameter names. *)
    let e =
      List.fold_left
        (fun e t ->
          e
          @ check_duplicate_ident ER.get_loc
              (List.map (fun (i, _) -> i) t.comp_params))
        e contr.ccomps
    in

    (* Message literals must either be for "send" or "event" and well formed. *)
    let check_message eloc msg e =
      (* No repeating message field. *)
      let e =
        e
        @ check_duplicate_ident
            (fun _ -> eloc)
            (List.map (fun (s, _) -> SR.mk_id_string s) msg)
      in

      (* Either "_tag" or "_eventname" must be present. *)
      let e =
        if List.exists (fun (s, _) -> s = tag_label) msg then
          (* This is a "send" Message. Ensure "_amount" and "_recipient" provided. *)
          if
            List.exists (fun (s, _) -> s = amount_label) msg
            && List.exists (fun (s, _) -> s = recipient_label) msg
          then e
          else
            e
            @ mk_error1
                ( "Missing " ^ amount_label ^ " or " ^ recipient_label
                ^ " in Message\n" )
                eloc
        else if
          (* It must be an event or an exception. *)
          List.exists
            (fun (s, _) -> s = eventname_label || s = exception_label)
            msg
        then e
        else e @ mk_error1 "Invalid message construct." eloc
      in
      pure e
      (* as required by "fold_over_messages" *)
    in
    let%bind e = fold_over_messages cmod ~init:e ~f:check_message in

    (* Component parameters cannot have names as that of implicit ones. *)
    let e =
      List.fold_left
        (fun e c ->
          match
            List.find_opt
              (fun (s, _) -> get_id s = amount_label || get_id s = sender_label)
              c.comp_params
          with
          | Some (s, _) ->
              e
              @ mk_error1
                  (Core_kernel.sprintf
                     "Parameter %s in %s %s cannot be explicit.\n" (get_id s)
                     (component_type_to_string c.comp_type)
                     (get_id c.comp_name))
                  (SR.get_loc @@ get_rep c.comp_name)
          | None -> e)
        e contr.ccomps
    in

    (* Contract parameters cannot have names of implicit ones. *)
    let e =
      match
        List.find_opt
          (fun (s, _) ->
            get_id s = ContractUtil.creation_block_label
            || get_id s = ContractUtil.scilla_version_label
            || get_id s = ContractUtil.this_address_label)
          contr.cparams
      with
      | Some (s, _) ->
          e
          @ mk_error1
              (Core_kernel.sprintf "Contract parameter %s cannot be explicit.\n"
                 (get_id s))
              (ER.get_loc @@ get_rep s)
      | None -> e
    in

    (* Look for any statement that is loading/storing a full Map and warn. *)
    let check_typ_warn s =
      let t = (ER.get_type (get_rep s)).tp in
      let lc = ER.get_loc (get_rep s) in
      match t with
      | MapType _
      (* The result of a <- a[][], i.e., "a" is an Option type. *)
      | ADT (Ident ("Option", _), [ MapType _ ]) ->
          warn1 "Consider using in-place Map access"
            warning_level_map_load_store lc
      | _ -> ()
    in
    List.iter
      (fun comp ->
        let rec stmt_iter stmts =
          List.iter
            (fun (stmt, _) ->
              match stmt with
              (* Recursion basis. *)
              | Load (_, s) | Store (s, _) | MapGet (s, _, _, _) ->
                  check_typ_warn s
              | MapUpdate (_, _, vopt) -> (
                  match vopt with Some s -> check_typ_warn s | None -> () )
              (* Recurse through match statements. *)
              | MatchStmt (_, pat_stmts) ->
                  List.iter (fun (_, stmts) -> stmt_iter stmts) pat_stmts
              | _ -> ())
            stmts
        in
        stmt_iter comp.comp_body)
      cmod.contr.ccomps;

    if e = [] then pure () else fail e

  (* ************************************** *)
  (* ******** Check name shadowing ******** *)
  (* ************************************** *)

  module CheckShadowing = struct
    (* A utility function that checks if "id" is shadowing cparams, cfields or pnames. *)
    let check_warn_redef cparams cfields pnames id =
      if List.mem (get_id id) cparams then
        warn1
          (Printf.sprintf "Name %s shadows a contract parameter." (get_id id))
          warning_level_name_shadowing
          (ER.get_loc (get_rep id))
      else if List.mem (get_id id) cfields then
        warn1
          (Printf.sprintf "Name %s shadows a field declaration." (get_id id))
          warning_level_name_shadowing
          (ER.get_loc (get_rep id))
      else if List.mem (get_id id) pnames then
        warn1
          (Printf.sprintf "Name %s shadows a transition parameter." (get_id id))
          warning_level_name_shadowing
          (ER.get_loc (get_rep id))

    (* Check for shadowing in patterns. *)
    let pattern_iter pat cparams cfields pnames =
      (* Check if any variable bound in this pattern shadows cparams/cfields/pnames *)
      let rec outer_scope_iter = function
        | Wildcard -> ()
        | Binder i -> check_warn_redef cparams cfields pnames i
        | Constructor (_, plist) ->
            List.iter (fun pat -> outer_scope_iter pat) plist
      in
      outer_scope_iter pat;
      (* Check for shadowing of names within this pattern and warn that it is
       * deprecated. This will be disallowed (i.e., an error) in future versions.
       * https://github.com/Zilliqa/scilla/issues/687. To close this Issue:
       * Make this an error by just using fail1 below instead of warn1. *)
      let bounds = get_pattern_bounds pat in
      match Core_kernel.List.find_a_dup ~compare:compare_id bounds with
      | Some v ->
          warn1
            (Printf.sprintf
               "Deprecated: variable %s shadows a previous binding in the same \
                pattern."
               (get_id v))
            warning_level_name_shadowing
            (ER.get_loc (get_rep v));
          pure ()
      | None -> pure ()

    (* Check for shadowing in expressions. *)
    let rec expr_iter (e, _) cparams cfields pnames =
      match e with
      | Literal _ | Builtin _ | Constr _ | App _ | Message _ | Var _ | TApp _ ->
          pure ()
      | Let (i, _, e_lhs, e_rhs) ->
          check_warn_redef cparams cfields pnames i;
          let%bind _ = expr_iter e_lhs cparams cfields pnames in
          expr_iter e_rhs cparams cfields pnames
      | Fun (i, _, e_body) | Fixpoint (i, _, e_body) | TFun (i, e_body) ->
          (* "i" being a type variable shouldn't be shadowing contract parameters,
             fields or component parameters. This is just a conservative check. *)
          check_warn_redef cparams cfields pnames i;
          expr_iter e_body cparams cfields pnames
      | MatchExpr (_, clauses) ->
          iterM
            ~f:(fun (pat, mbody) ->
              let%bind _ = pattern_iter pat cparams cfields pnames in
              expr_iter mbody cparams cfields pnames)
            clauses

    let shadowing_libentries lentries =
      iterM
        ~f:(fun lentry ->
          match lentry with
          | LibTyp _ -> pure ()
          | LibVar (_, _, vexp) -> expr_iter vexp [] [] [])
        lentries

    let rec shadowing_libtree ltree =
      let%bind _ = iterM ~f:(fun dep -> shadowing_libtree dep) ltree.deps in
      shadowing_libentries ltree.libn.lentries

    let shadowing_cmod (cmod : cmodule) =
      (* Check for match pattern shadowing in library functions. *)
      let%bind _ =
        match cmod.libs with
        | Some lib -> shadowing_libentries lib.lentries
        | None -> pure ()
      in

      let cparams = List.map (fun (p, _) -> get_id p) cmod.contr.cparams in

      (* Check for shadowing in contract constraint *)
      let%bind _ = expr_iter cmod.contr.cconstraint cparams [] [] in

      (* Check if a field shadows any contract parameter. *)
      let%bind _ =
        iterM
          ~f:(fun (f, _, finit_expr) ->
            check_warn_redef cparams [] [] f;
            expr_iter finit_expr cparams [] [])
          cmod.contr.cfields
      in

      let cfields = List.map (fun (f, _, _) -> get_id f) cmod.contr.cfields in

      (* Go through each component. *)
      iterM
        ~f:(fun c ->
          (* 1. If a parameter name shadows one of cparams or cfields, warn. *)
          List.iter
            (fun (p, _) -> check_warn_redef cparams cfields [] p)
            c.comp_params;
          let pnames = List.map (fun (p, _) -> get_id p) c.comp_params in

          (* Check for shadowing in statements. *)
          let rec stmt_iter stmts =
            iterM
              ~f:(fun (s, _) ->
                match s with
                | Load (x, _) | MapGet (x, _, _, _) | ReadFromBC (x, _) ->
                    check_warn_redef cparams cfields pnames x;
                    pure ()
                | Store _ | MapUpdate _ | SendMsgs _ | AcceptPayment
                | CreateEvnt _ | Throw _ | CallProc _ ->
                    pure ()
                | Bind (x, e) ->
                    check_warn_redef cparams cfields pnames x;
                    expr_iter e cparams cfields pnames
                | MatchStmt (_, clauses) ->
                    iterM
                      ~f:(fun (pat, mbody) ->
                        let%bind _ = pattern_iter pat cparams cfields pnames in
                        stmt_iter mbody)
                      clauses)
              stmts
          in
          (* Go through all statements and see if any of cparams, cfields or pnames are redefined. *)
          stmt_iter c.comp_body)
        cmod.contr.ccomps

    let shadowing_lmod (lmod : lmodule) =
      (* Check for match pattern shadowing in library functions. *)
      shadowing_libentries lmod.libs.lentries
  end

  (* ************************************** *)
  (* ******** Interface to Checker ******** *)
  (* ************************************** *)

  let contr_sanity (cmod : cmodule) (rlibs : lib_entry list)
      (elibs : libtree list) =
    let%bind _ = basic_sanity cmod in
    let%bind _ = CheckShadowing.shadowing_libentries rlibs in
    let%bind _ = iterM ~f:CheckShadowing.shadowing_libtree elibs in
    let%bind _ = CheckShadowing.shadowing_cmod cmod in
    pure ()

  let lmod_sanity (lmod : lmodule) (rlibs : lib_entry list)
      (elibs : libtree list) =
    let%bind _ = CheckShadowing.shadowing_libentries rlibs in
    let%bind _ = iterM ~f:CheckShadowing.shadowing_libtree elibs in
    let%bind _ = CheckShadowing.shadowing_lmod lmod in
    pure ()
end
