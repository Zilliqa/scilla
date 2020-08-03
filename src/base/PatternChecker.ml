(*
 * Copyright (c) 2018 - present. 
 * Zilliqa, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(* This file implements pattern match checks for
 * - Exhaustiveness: All possible patterns are covered
 * - Reachability: All provided patterns can be reached
 * Reference: ML pattern match compilation and partial evaluation - Peter Sestoft
 *)

open Literal
open Syntax
open Core_kernel
open ErrorUtils
open MonadUtil
open Result.Let_syntax
open TypeUtil
open Datatypes
open PatternUtil
open Exp_descriptions
open Decision_Tree

module ScillaPatternchecker
    (SR : Rep) (ER : sig
      include Rep

      val get_type : rep -> PlainTypes.t inferred_type
    end) =
struct
  module SPR = SR
  module EPR = ER

  (* TODO: Change this to CanonicalLiteral = Literals based on canonical names. *)
  module PCLiteral = FlattenedLiteral
  module PCType = PCLiteral.LType
  module PCIdentifier = PCType.TIdentifier
  module UncheckedPatternSyntax = ScillaSyntax (SR) (ER) (PCLiteral)
  module CheckedPatternSyntax = ScillaSyntax (SPR) (EPR) (PCLiteral)
  module TU = TypeUtilities
  open UncheckedPatternSyntax
  open TU

  let wrap_pmcheck_err e ?(opt = "") = wrap_err e "patternmatch checking" ~opt

  let wrap_pmcheck_serr s ?(opt = "") = wrap_serr s "patternmatch checking" ~opt

  let pm_check_clauses t clauses =
    let reachable = Array.create ~len:(List.length clauses) false in
    let static_match c_name span dsc =
      match dsc with
      | Pos (dsc_c_name, _) -> if String.(c_name = dsc_c_name) then Yes else No
      | Neg c_names ->
          if List.mem c_names c_name ~equal:String.( = ) then No
          else if List.length c_names = span - 1 then Yes
          else Maybe
    in
    let rec traverse_clauses dsc i rest_clauses =
      match rest_clauses with
      | [] ->
          fail0 @@ "Non-exhaustive pattern match."
          (* TODO, Issue #210: Give counter-example based on dsc *)
      | (p1, _) :: rest_clauses' -> match_pattern p1 t dsc [] [] i rest_clauses'
    and traverse_pattern ctx sps i rest_clauses =
      match sps with
      | [] ->
          (* Pattern can be matched *)
          let _ = reachable.(i) <- true in
          let e =
            match List.nth clauses i with
            | Some (_, e) -> e
            | None ->
                raise
                  (mk_internal_error
                     (sprintf "Pattern index %d too high (or low)" i))
          in
          pure @@ Success e
      | ([], [], []) :: sps_rest ->
          traverse_pattern (pos_ctx ctx) sps_rest i rest_clauses
      | (p1 :: ps, t1 :: ts, dsc1 :: dscs) :: sps_rest ->
          match_pattern p1 t1 dsc1 ctx
            ((ps, ts, dscs) :: sps_rest)
            i rest_clauses
      | _ -> fail0 @@ "Internal error - pattern match uses incorrect arity"
    and match_pattern p t dsc ctx sps_rest i rest_clauses =
      match p with
      | Wildcard | Binder _ ->
          traverse_pattern (augment_ctx ctx dsc) sps_rest i rest_clauses
      | Constructor (c_name, sps_cons) -> (
          let arity () = List.length sps_cons in
          let get_t_args () =
            constr_pattern_arg_types t
              (PCIdentifier.as_string c_name)
              ~lc:(SR.get_loc (PCIdentifier.get_rep c_name))
          in
          let get_dsc_args dsc =
            match dsc with
            | Pos (_, args) -> args
            | Neg _ -> List.init (arity ()) ~f:(fun _ -> Neg [])
          in
          let success () =
            let%bind t_args = get_t_args () in
            traverse_pattern
              ((PCIdentifier.as_string c_name, []) :: ctx)
              ((sps_cons, t_args, get_dsc_args dsc) :: sps_rest)
              i rest_clauses
          in
          let failure new_dsc =
            traverse_clauses
              (build_dsc ctx new_dsc sps_rest)
              (i + 1) rest_clauses
          in
          let%bind adt, _ =
            DataTypeDictionary.lookup_constructor
              ~sloc:(SR.get_loc (PCIdentifier.get_rep c_name))
              (PCIdentifier.as_string c_name)
          in
          let span = List.length adt.tconstr in
          match static_match (PCIdentifier.as_string c_name) span dsc with
          | Yes -> success ()
          | No -> failure dsc
          | Maybe ->
              let%bind s_tree = success () in
              let%bind f_tree =
                failure (add_neg dsc (PCIdentifier.as_string c_name))
              in
              pure @@ IfEq (t, c_name, s_tree, f_tree) )
    in
    let%bind decision_tree = traverse_clauses (Neg []) 0 clauses in
    match Array.findi reachable ~f:(fun _ r -> not r) with
    | None -> pure @@ decision_tree (* All patterns reachable *)
    | Some (i, _) -> fail0 @@ sprintf "Pattern %d is unreachable." (i + 1)

  (* TODO, Issue #270: look up relevant pattern in clauses and report it *)

  let lift_msg_payloads sps =
    List.map sps ~f:(fun (s, p) ->
        let lifted_p =
          match p with
          | MLit l -> CheckedPatternSyntax.MLit l
          | MVar (Ident (vs, r)) ->
              CheckedPatternSyntax.MVar (PCIdentifier.mk_id vs r)
        in
        (s, lifted_p))

  let rec lift_pattern p =
    match p with
    | Wildcard -> CheckedPatternSyntax.Wildcard
    | Binder (Ident (s, r)) ->
        CheckedPatternSyntax.Binder (PCIdentifier.mk_id s r)
    | Constructor (s, sps) ->
        CheckedPatternSyntax.Constructor
          (s, List.map sps ~f:(fun sp -> lift_pattern sp))

  let rec pm_check_expr erep =
    let e, rep = erep in
    match e with
    | Literal l -> pure @@ (CheckedPatternSyntax.Literal l, rep)
    | Var i -> pure @@ (CheckedPatternSyntax.Var i, rep)
    | Let (i, t, b, body) ->
        let%bind checked_b = wrap_pmcheck_err erep @@ pm_check_expr b in
        let%bind checked_body = pm_check_expr body in
        pure @@ (CheckedPatternSyntax.Let (i, t, checked_b, checked_body), rep)
    | Message msgs ->
        pure @@ (CheckedPatternSyntax.Message (lift_msg_payloads msgs), rep)
    | Fun (i, t, body) ->
        let%bind checked_body = pm_check_expr body in
        pure @@ (CheckedPatternSyntax.Fun (i, t, checked_body), rep)
    | App (f, args) -> pure @@ (CheckedPatternSyntax.App (f, args), rep)
    | Constr (c, t, args) ->
        pure @@ (CheckedPatternSyntax.Constr (c, t, args), rep)
    | MatchExpr ((Ident (_, r) as x), clauses) ->
        let t = ER.get_type r in
        let msg = sprintf " of type %s" (PCType.pp_typ t.tp) in
        wrap_pmcheck_err erep ~opt:msg
        @@ let%bind () = Result.ignore_m @@ pm_check_clauses t.tp clauses in
           let%bind checked_clauses =
             mapM
               ~f:(fun (p, e) ->
                 let%bind checked_e = pm_check_expr e in
                 pure @@ (lift_pattern p, checked_e))
               clauses
           in
           pure @@ (CheckedPatternSyntax.MatchExpr (x, checked_clauses), rep)
    | Builtin (f, args) -> pure @@ (CheckedPatternSyntax.Builtin (f, args), rep)
    (* Advanced features: to be added in Scilla 0.2 *)
    | TFun (t, body) ->
        let%bind checked_body = pm_check_expr body in
        pure @@ (CheckedPatternSyntax.TFun (t, checked_body), rep)
    | TApp (i, targs) -> pure @@ (CheckedPatternSyntax.TApp (i, targs), rep)
    (* Fixpoint combinator: used to implement recursion principles *)
    | Fixpoint (i, t, body) ->
        wrap_pmcheck_err erep
        @@ let%bind checked_body = pm_check_expr body in
           pure @@ (CheckedPatternSyntax.Fixpoint (i, t, checked_body), rep)

  let rec pm_check_stmts stmts =
    match stmts with
    | [] -> pure @@ []
    | ((s, rep) as srep) :: sts ->
        let%bind checked_s =
          match s with
          | Load (i, x) -> pure @@ (CheckedPatternSyntax.Load (i, x), rep)
          | Store (i, x) -> pure @@ (CheckedPatternSyntax.Store (i, x), rep)
          | MapUpdate (m, klist, v) ->
              pure @@ (CheckedPatternSyntax.MapUpdate (m, klist, v), rep)
          | MapGet (v, m, klist, valfetch) ->
              pure @@ (CheckedPatternSyntax.MapGet (v, m, klist, valfetch), rep)
          | Bind (i, e) ->
              wrap_pmcheck_serr srep
              @@ let%bind checked_e = pm_check_expr e in
                 pure @@ (CheckedPatternSyntax.Bind (i, checked_e), rep)
          | MatchStmt ((Ident (_, r) as x), clauses) ->
              wrap_pmcheck_serr srep
              @@
              let t = ER.get_type r in
              let%bind () = Result.ignore_m @@ pm_check_clauses t.tp clauses in
              let%bind checked_clauses =
                mapM
                  ~f:(fun (p, ss) ->
                    let%bind checked_s = pm_check_stmts ss in
                    pure @@ (lift_pattern p, checked_s))
                  clauses
              in
              pure @@ (CheckedPatternSyntax.MatchStmt (x, checked_clauses), rep)
          | ReadFromBC (i, s) ->
              pure @@ (CheckedPatternSyntax.ReadFromBC (i, s), rep)
          | AcceptPayment -> pure @@ (CheckedPatternSyntax.AcceptPayment, rep)
          | Iterate (l, p) -> pure @@ (CheckedPatternSyntax.Iterate (l, p), rep)
          | SendMsgs i -> pure @@ (CheckedPatternSyntax.SendMsgs i, rep)
          | CreateEvnt i -> pure @@ (CheckedPatternSyntax.CreateEvnt i, rep)
          | CallProc (p, args) ->
              pure @@ (CheckedPatternSyntax.CallProc (p, args), rep)
          | Throw i -> pure @@ (CheckedPatternSyntax.Throw i, rep)
        in
        let%bind checked_stmts = pm_check_stmts sts in
        pure @@ (checked_s :: checked_stmts)

  let pm_check_component t =
    let { comp_type; comp_name; comp_params; comp_body } = t in
    let msg =
      sprintf "Error during pattern-match checking of %s %s:\n"
        (component_type_to_string comp_type)
        (PCIdentifier.as_error_string comp_name)
    in
    let%bind checked_body =
      wrap_with_info (msg, SR.get_loc (PCIdentifier.get_rep comp_name))
      @@ pm_check_stmts comp_body
    in
    pure
    @@ {
         CheckedPatternSyntax.comp_type;
         CheckedPatternSyntax.comp_name;
         CheckedPatternSyntax.comp_params;
         CheckedPatternSyntax.comp_body = checked_body;
       }

  let pm_check_libentries lentries =
    let%bind checked_lentries =
      mapM
        ~f:(fun entry ->
          match entry with
          | LibTyp (tname, typs) ->
              let lifted_typs =
                List.map typs ~f:(fun { cname; c_arg_types } ->
                    {
                      CheckedPatternSyntax.cname;
                      CheckedPatternSyntax.c_arg_types;
                    })
              in
              pure @@ CheckedPatternSyntax.LibTyp (tname, lifted_typs)
          | LibVar (entryname, t, lexp) ->
              let msg =
                sprintf "Error during pattern-match checking of library %s:\n"
                  (PCIdentifier.as_error_string entryname)
              in
              let%bind checked_lexp =
                wrap_with_info (msg, ER.get_loc (PCIdentifier.get_rep entryname))
                @@ pm_check_expr lexp
              in
              pure @@ CheckedPatternSyntax.LibVar (entryname, t, checked_lexp))
        lentries
    in
    pure @@ checked_lentries

  let pm_check_library l =
    let { lname = libname; lentries } = l in
    let%bind checked_lentries = pm_check_libentries lentries in
    pure
    @@ {
         CheckedPatternSyntax.lname = libname;
         CheckedPatternSyntax.lentries = checked_lentries;
       }

  let rec pm_check_libtree ltree =
    let%bind deps = mapM ltree.deps ~f:pm_check_libtree in
    let%bind l = pm_check_library ltree.libn in
    pure { CheckedPatternSyntax.libn = l; CheckedPatternSyntax.deps }

  let pm_check_fields fs =
    mapM
      ~f:(fun (i, t, e) ->
        let msg =
          sprintf "Error during pattern-match checking of field %s:\n"
            (PCIdentifier.as_error_string i)
        in
        let%bind checked_e =
          wrap_with_info (msg, ER.get_loc (PCIdentifier.get_rep i))
          @@ pm_check_expr e
        in
        pure @@ (i, t, checked_e))
      fs

  let pm_check_contract c =
    let { cname; cparams; cconstraint; cfields; ccomps } = c in
    let%bind checked_flds = pm_check_fields cfields in
    let%bind checked_constraint = pm_check_expr cconstraint in
    let%bind checked_comp = mapM ~f:(fun c -> pm_check_component c) ccomps in
    pure
    @@ {
         CheckedPatternSyntax.cname;
         CheckedPatternSyntax.cparams;
         CheckedPatternSyntax.cconstraint = checked_constraint;
         CheckedPatternSyntax.cfields = checked_flds;
         CheckedPatternSyntax.ccomps = checked_comp;
       }

  let pm_check_lmodule lm rlibs elibs =
    let { smver; elibs = mod_elibs; libs } = lm in
    let%bind checked_rlibs = pm_check_libentries rlibs in
    let%bind checked_elibs = mapM elibs ~f:pm_check_libtree in

    let%bind checked_libs = pm_check_library libs in
    pure
      ( {
          CheckedPatternSyntax.smver;
          CheckedPatternSyntax.elibs = mod_elibs;
          CheckedPatternSyntax.libs = checked_libs;
        },
        checked_rlibs,
        checked_elibs )

  let pm_check_module md rlibs elibs =
    let { smver = mod_smver; libs; elibs = mod_elibs; contr } = md in
    let { cname = ctr_cname; cparams; cconstraint; cfields; ccomps } = contr in
    let init_msg =
      sprintf "Type error(s) in contract %s:\n"
        (PCIdentifier.as_error_string ctr_cname)
    in
    wrap_with_info (init_msg, dummy_loc)
    @@ let%bind checked_rlibs = pm_check_libentries rlibs in
       let%bind checked_elibs = mapM elibs ~f:pm_check_libtree in

       let%bind checked_lib, emsgs =
         match libs with
         | Some l -> (
             match pm_check_library l with
             | Ok c_lib -> Ok (Some c_lib, [])
             | Error msg -> Ok (None, msg) )
         | None -> Ok (None, [])
       in

       let%bind checked_constraint, emsgs' =
         match pm_check_expr cconstraint with
         | Ok ckd_constraint -> Ok (ckd_constraint, emsgs)
         | Error msg ->
             Ok
               ( ( CheckedPatternSyntax.Literal PCLiteral.false_lit,
                   EPR.dummy_rep ),
                 emsgs @ msg )
       in

       let%bind checked_fields, emsgs'' =
         match pm_check_fields cfields with
         | Error msg -> Ok ([], emsgs' @ msg)
         | Ok ckd_fields -> Ok (ckd_fields, emsgs')
       in

       let%bind c_comps, emsgs''' =
         foldM ~init:([], emsgs'') ccomps ~f:(fun (comps_acc, msg_acc) cp ->
             match pm_check_component cp with
             | Error msg -> Ok (comps_acc, msg_acc @ msg)
             | Ok ckd_comp -> Ok (ckd_comp :: comps_acc, msg_acc))
       in
       let checked_comps = List.rev c_comps in

       if List.is_empty emsgs''' (* Return pure environment *) then
         pure
           ( {
               CheckedPatternSyntax.smver = mod_smver;
               CheckedPatternSyntax.libs = checked_lib;
               CheckedPatternSyntax.elibs = mod_elibs;
               CheckedPatternSyntax.contr =
                 {
                   CheckedPatternSyntax.cname = ctr_cname;
                   CheckedPatternSyntax.cparams;
                   CheckedPatternSyntax.cconstraint = checked_constraint;
                   CheckedPatternSyntax.cfields = checked_fields;
                   CheckedPatternSyntax.ccomps = checked_comps;
                 };
             },
             checked_rlibs,
             checked_elibs ) (* Return error messages *)
       else fail @@ emsgs'''
end
