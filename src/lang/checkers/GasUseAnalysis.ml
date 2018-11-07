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

(* Gas Usage Analysis for Scilla contracts. *)

open TypeUtil
open Syntax
open ErrorUtils
open MonadUtil
open Polynomial
open Core.Result.Let_syntax

module ScillaGUA
    (SR : Rep)
    (ER : sig
       include Rep
       val get_type : rep -> PlainTypes.t inferred_type
     end) = struct

  module SER = SR
  module EER = ER
  module GUASyntax = ScillaSyntax (SR) (ER)
  module TU = TypeUtilities (SR) (ER)
  module Gas = Gas.ScillaGas (SR) (ER)

  open GUASyntax

  (* Every size or gas use will depend on an ident (arg or free var) or on a constant. *)
  type baseref =
    | Var of ER.rep ident
    (* Constant of type and size. *)
    | Const of typ * int

  type sizeref =
    (* For sizes of PrimTypes, function value (and instantiations for TVars?). *)
    | Base of baseref
    (* For List and Map types. *)
    | Length of sizeref
    (* For components of Lists, Maps and Option. *)
    | Component of sizeref
    (* For first and second component of Pair *)
    | Fst of sizeref | Snd of sizeref
    (* Constructing a List / Map or Option from. *)
    | Container of sizeref
    (* Constructing a Pair from. *)
    | PairS of sizeref * sizeref
    (* Lambda for unknown sizeref (from applying higher order functions). 
     * TODO: Use "sizeref" instead of "ident" to handle applying wrapped functions,
             where for example `x = fst arg` and we're applying `x`. *)
    | SApp of ER.rep ident * sizeref list

  (* A variable / quantity on which gas use depends. *)
  type guref =
    (* Gas use depends on size of a value *)
    | SizeOf of sizeref
    (* Gas use depends on which Branch "i" of `match ident with` is taken. *)
    | BranchTaken of int * sizeref
    (* Applying a higher order argument (function) and its arguments. *)
    (* TODO: Use "sizeref" instead of "ident" to handle applying wrapped functions,
             where for example `x = fst arg` and we're applying `x`. *)
    | GApp of ER.rep ident * guref list

  (* Gas use and result sizeref of functions (or executions)
   * The identifier list specifies the arguments which must be substituted.
   * Signatures that contain "GApp/SApp" will (recursively) be substituted for
   * the final signature to not have these lambdas. *)
  type signature = (ER.rep ident list) * sizeref * guref polynomial

  module GUAEnv = struct
    open Utils.AssocDictionary
    (* A map from identifier strings to their signatures. *)
    type t = signature dict

    (* Make an empty environment. *)
    let mk = make_dict

    (* In env, add mapping id => s *)
    let addS env id s =
      insert id s env

    (* In env, resolve id => s and return s (fails if cannot resolve). *)
    let resolvS ?lopt:(lopt = None) env id =
      match lookup id env with
      | Some s -> pure s
      | None ->
        let sloc = (match lopt with
        | Some l -> ER.get_loc l
        | None -> dummy_loc) in
          fail1 (Printf.sprintf
            "Couldn't resolve the identifier in gas use analysis: \"%s\".\n" id)
            sloc

  end

  (* Given a signature, substitute actual arguments into the formal parameters. *)
  let substitute_actuals f (params, ressize, gup) actuals =
    if List.length params != List.length actuals then
      fail1 "Number of actual arguments and formal parameters mismatch" (ER.get_loc (get_rep f))
    else
      (* replace param with actual in baseref. *)
      let baseref_replacer param actual b' = 
        match b' with
        | Var i -> if (get_id i) = (get_id param) then Var (actual) else b'
        | Const _ -> b'
      in
      (* replace param with actual in sizeref. *)
      let sizeref_replacer param actual s' =
        let rec replacer s =
          match s with
          | Base b -> Base (baseref_replacer param actual b)
          | Length s' -> Length (replacer s')
          | Component s' -> Component (replacer s')
          | Fst s' -> Fst (replacer s') | Snd s' -> Snd (replacer s')
          | Container s' -> Container (replacer s')
          | PairS (s1', s2') -> PairS (replacer s1', replacer s2')
          | SApp (id, srlist) ->
            let id' = if (get_id id) = (get_id param) then actual else id in
            let srlist' = List.map (fun v -> replacer v) srlist in
            SApp (id', srlist')
        in
        replacer s'
      in
      (* replace param with actual in guref. *)
      let guref_replacer param actual v' =
        let rec replacer v = 
          match v with
          | SizeOf s -> SizeOf (sizeref_replacer param actual s)
          | BranchTaken (i, sr) -> BranchTaken (i, sizeref_replacer param actual sr)
          | GApp (id, gurlist) ->
            let id' = if (get_id id) = (get_id param) then actual else id in
            let gurlist' = List.map (fun v -> replacer v) gurlist in
            GApp (id', gurlist')
        in
        replacer v'
      in
      (* replace param with actual in a signature. *)
      let polynomial_replacer param actual pol =
        let term_replacer ter =
          let (coef, gurlist) = ter in
            (coef, List.map (fun (v, i1) -> (guref_replacer param actual v, i1)) gurlist)
        in
          List.map (fun t -> term_replacer t) pol
      in
      let ressize' = List.fold_left2 (fun sr param actual -> sizeref_replacer param actual sr) ressize params actuals in
      let gup' = List.fold_left2 (fun pol param actual -> polynomial_replacer param actual pol) gup params actuals in
      pure ([], ressize', gup')

  (* Expand all SApps in sr which have a bound signature in genv. *)
  let expand_sapps _ sr =
    (* TODO *)
    pure sr

  (* Expand all GApps in pol which have a bound signature in genv. *)
  let expand_gapps _ pol =
    (* TODO *)
    pure pol

  (* For a pattern "pat" match on "msref", add binders to genv. *)
  let rec bind_pattern genv msref pat =
    match pat with
    | Wildcard -> pure genv
    | Binder i -> pure @@ GUAEnv.addS genv (get_id i) ([], msref, empty_pn)
    | Constructor (cname, plist) ->
      (match cname with
      | "True" | "False" | "Nil" | "None" -> pure @@ genv
      | "Cons" | "Some" ->
        (* TypeChecker will ensure that plist has unit length. *)
        let arg = List.nth plist 0 in
        bind_pattern genv (Component(msref)) arg
      | "Pair" ->
        (* TypeChecker will ensure that plist has two elements. *)
        let arg0 = List.nth plist 0 in
        let arg1 = List.nth plist 1 in
        let%bind genv' = bind_pattern genv (Fst(msref)) arg0 in
        let%bind genv'' = bind_pattern genv' (Snd(msref)) arg1 in
        pure genv''
      | _ -> fail0 (Printf.sprintf "Unsupported constructor %s in gas analysis." cname))

  (* Return gas use and result sizeref polynomials of evaluating an expression. *)
  let rec gua_expr genv (erep : expr_annot) =
    let%bind c = Gas.expr_static_cost erep in
    let cc = const_pn c in
    let (e, rep) = erep in
    match e with
    | Literal l ->
      let%bind ss = Gas.literal_cost l in
      let t = (ER.get_type rep).tp in
      pure @@ ([], Base(Const(t, ss)), cc)
    | Var i ->
      let%bind (args, ressize, gup) = GUAEnv.resolvS genv (get_id i) ~lopt:(Some(get_rep i)) in
      pure (args, ressize, (add_pn gup cc))
    | Fun (arg, _, body) ->
      (* Add a sizeref for arg into env for the body to reference to. *)
      let b = Base(Var(arg)) in
      let (p : signature) = ([], b, empty_pn) in
      let genv' = GUAEnv.addS genv (get_id arg) p in
      let%bind (sargs, rsize, rgas) = gua_expr genv' body in
      (* We have signature for the body, just add b as a parameter to that function signature. *)
      pure (arg::sargs, rsize, (add_pn rgas cc))
    | App (f, actuals) ->
      (* if we have a signature for f, substitute it, otherwise build a lambda. *)
      let%bind (params, ressize, gup) = GUAEnv.resolvS genv (get_id f) in
      if params = [] then
        (* We don't have the signature for f, so create a lambda. *)
        let srparams = List.map (fun i -> Base(Var(i))) actuals in
        let u = SApp(f, srparams) in
        let guparams = List.map (fun i -> SizeOf(Base(Var(i)))) actuals in
        let v = single_simple_pn (GApp (f, guparams)) in
        pure ([], u, (add_pn v cc))
      else
        (* Subtitute actuals into the parameters of the signature. *)
        let%bind (a, ressize, gup) = substitute_actuals f (params, ressize, gup) actuals in
        (* Expand all lambdas that we can. *)
        let%bind ressize' = expand_sapps genv ressize in
        let%bind gup' =  expand_gapps genv gup in
        pure (a, ressize', (add_pn gup' cc))
    | Builtin (_, actuals) ->
      (* TODO: *)
      let p = List.fold_left (fun acc v -> add_pn acc [(10, [(SizeOf(Base(Var(v))), 1)])]) empty_pn actuals in
      (* How to encode size of builtin result? *)
      let s = Base(Const(PrimTypes.int32_typ, 1)) in
      pure ([], s, (add_pn p cc))
    | Let (i, _, lhs, rhs) ->
      let%bind lhs_sig = gua_expr genv lhs in
      let genv' = GUAEnv.addS genv (get_id i) lhs_sig in
      let%bind rhs_sig = gua_expr genv' rhs in
      (* gas consumption is sum of both. *)
      let (args, ressize, gup) = rhs_sig in
      let (_, _, gup') = lhs_sig in
      (* Gas cost for this let expression is sum of LHS cost and RHS cost. *)
      let p = add_pn gup gup' in
      pure (args, ressize, (add_pn p cc))
    | Constr (cname, _, actuals) ->
      let%bind ressize = 
        (match cname with
        | "True" | "False" -> pure @@ Base(Const(ADT("Bool", []), 0))
        | "Nil" -> pure @@ Base(Const(ADT("List", []), 0))
        | "None" -> pure @@ Base(Const(ADT("Option", []), 0))
        | "Cons" | "Some" ->
          (* TypeChecker will ensure that actuals has unit length. *)
          let arg = List.nth actuals 0 in
          let%bind (_, compsize, _) = GUAEnv.resolvS genv (get_id arg) in
          pure @@ Container(compsize)
        | "Pair" ->
          (* TypeChecker will ensure that actuals has two elements. *)
          let arg0 = List.nth actuals 0 in
          let arg1 = List.nth actuals 1 in
          let%bind (_, compsize0, _) = GUAEnv.resolvS genv (get_id arg0) in
          let%bind (_, compsize1, _) = GUAEnv.resolvS genv (get_id arg1) in
          pure @@ PairS(compsize0, compsize1)
        | _ -> fail1 (Printf.sprintf "Unsupported constructor %s in gas analysis." cname)
                     (ER.get_loc rep)
        )
      in
        pure ([], ressize, cc)
    | MatchExpr (x, clauses) ->
      let%bind (_, xsize, _) = GUAEnv.resolvS genv (get_id x) in
      (* TODO: How to express sizeref of result across branches?
       *       Using the result sizeref of the last branch now. *)
      let%bind (res, _) = foldM ~f:(fun ((_, _, apn), i) (pat, branch) ->
        let%bind genv' = bind_pattern genv xsize pat in
        let%bind (_, bsize, bgu) = gua_expr genv' branch in
        (* combine branch gas use with other branches using "i" as a weight. *)
        let weight = BranchTaken (i, xsize) in
        let bpn = mul_pn bgu (single_simple_pn weight) in
        pure (([], bsize, add_pn apn bpn), i+1)
      ) ~init:(([], xsize, empty_pn), 0) clauses in
      pure res
    | Fixpoint (f, _, _) ->
      fail1 (Printf.sprintf "Fixpoint %s not supported yet." (get_id f)) (ER.get_loc (get_rep f))
    | TFun (tvar, _) ->
      fail1 (Printf.sprintf "Type variable %s not supported yet." (get_id tvar)) (ER.get_loc (get_rep tvar))
    | TApp (tf, _) ->
      fail1 (Printf.sprintf "Type instantiation %s not supported yet." (get_id tf)) (ER.get_loc (get_rep tf))
    | Message _ ->
      fail0 "Messages not supported yet."

  (* A simple wrapper to analyze an isolated expression. *)
  let gua_expr_wrapper erep =
    let genv = GUAEnv.mk () in
    gua_expr genv erep

  end