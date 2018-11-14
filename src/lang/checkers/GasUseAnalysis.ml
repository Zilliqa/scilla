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
       val mk_id : loc ident -> typ -> rep ident
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
    (* An input coming from the block chain state. *)
    | BCState of string

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
    (* To represent a message, each sizeref its components. *)
    | MsgS of sizeref list
    (* Depends on which Branch "i" of `match ident with` is taken. *)
    | BranchTaken of int * sizeref
    (* Result of a builtin. *)
    | BApp of string * sizeref list
    (* Lambda for unknown sizeref (from applying higher order functions). 
     * TODO: Use "sizeref" instead of "ident" to handle applying wrapped functions,
             where for example `x = fst arg` and we're applying `x`. *)
    | SApp of ER.rep ident * sizeref list

  (* A variable / quantity on which gas use depends. *)
  type guref =
    (* Gas use depends on size of a value *)
    | SizeOf of sizeref
    (* Applying a higher order argument (function) and its arguments. *)
    (* TODO: Use "sizeref" instead of "ident" to handle applying wrapped functions,
             where for example `x = fst arg` and we're applying `x`. *)
    | GApp of ER.rep ident * sizeref list
    (* Gas usage polynomial which is known and needs to be expanded. 
     * When a GApp resolves successfully, we replace it with GPol. *)
    | GPol of guref polynomial

  (* Gas use and result sizeref of functions (or executions)
   * The identifier list specifies the arguments which must be substituted.
   * Signatures that contain "GApp/SApp" will (recursively) be substituted for
   * the final signature to not have these lambdas. *)
  type signature = (ER.rep ident list) * sizeref * guref polynomial

  (* Given a baseref, print a string for it. *)
  let sprint_baseref = function
    | Var i -> (get_id) i
    | Const (t, i) -> Printf.sprintf "Value of %s with size %d" (pp_typ t) i
    | BCState s -> "BlockChain: " ^ s

  (* Given a size reference, print a description for it. *)
  let rec sprint_sizeref = function
    | Base b -> sprint_baseref b
    (* For List and Map types. *)
    | Length sr'-> "Length of: " ^ (sprint_sizeref sr')
    (* For components of Lists, Maps and Option. *)
    | Component sr' -> "Component of: " ^ (sprint_sizeref sr')
    (* For first and second component of Pair *)
    | Fst sr' -> "First component of the Pair: " ^ (sprint_sizeref sr')
    | Snd sr' -> "Second component of the Pair: " ^ (sprint_sizeref sr')
    (* Constructing a List / Map or Option from. *)
    | Container sr' -> "Container around: " ^ (sprint_sizeref sr')
    (* Constructing a Pair from. *)
    | PairS (sr1, sr2) -> "Pair of (" ^ (sprint_sizeref sr1) ^ ")(" ^ (sprint_sizeref sr2) ^ ")"
    | MsgS srlist -> "Message (" ^
      (List.fold_left (fun acc sr -> acc ^ (if acc = "" then "" else ",") ^ (sprint_sizeref sr)) "" srlist) ^ ")"
    | BranchTaken (i, s) -> (Printf.sprintf "Probability of branch %d in MatchExpr %s" i (sprint_sizeref s))
    | BApp (b, srlist) -> "Result of builtin " ^ b ^ "(" ^
      (List.fold_left (fun acc sr -> acc ^ (if acc = "" then "" else ",") ^ (sprint_sizeref sr)) "" srlist) ^ ")"
    | SApp (id, srlist) -> "SApp " ^ (get_id id) ^ "( " ^
      (List.fold_left (fun acc sr -> acc ^ (if acc = "" then "" else ",") ^ (sprint_sizeref sr)) "" srlist) ^ ")"

  (* Given a gas use reference, print a description for it. *)
  let rec sprint_guref = function
    | SizeOf s -> sprint_sizeref s
    | GApp (id, gurlist) -> "Cost of calling " ^ (get_id id) ^ "(" ^
      (List.fold_left (fun acc gur -> acc ^ (if acc = "" then "" else ", ") ^ (sprint_sizeref gur)) "" gurlist) ^ ")"
    | GPol pn -> "GPol(" ^ (sprint_gup  pn) ^ ")"
  and sprint_gup pn =
    let sym_map : (guref, string) Hashtbl.t = Hashtbl.create 16 in
    let syms = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" in
    let next_sim_idx = ref 0 in
    let pols = sprint_pn pn ~f:(fun gur -> 
      match Hashtbl.find_opt sym_map gur with
      | Some c -> c
      | None ->
        let i = (!next_sim_idx) in
        let _ = next_sim_idx := !next_sim_idx + 1 in
        let c = String.sub syms (i mod 52) 1 in
        let _ = Hashtbl.add sym_map gur c in
        c
    ) in
    let legs = 
      if Hashtbl.length sym_map = 0 then "" else
      "\nLegend:\n" ^
        (Hashtbl.fold (fun k v acc -> acc ^ "\n" ^ v ^ ": " ^ (sprint_guref k)) sym_map "")
    in
      (pols ^ legs)

  let sprint_signature (params, _, pn) =
    let args = 
      if (params = []) then "" else 
      "Parameter list: " ^ 
        (List.fold_left (fun acc p -> acc ^ (get_id p) ^ " ") "( " params) ^ ")\n"
    in
    (args ^ sprint_gup pn)

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

    let pp env =
      let l = to_list env in
      List.fold_left (fun acc (k, sign) ->
        acc ^
          "Signature for " ^ k ^ ": " ^ (sprint_signature sign) ^ "----\n"
      ) "" l

  end

  (* Given a signature, substitute actual arguments into the formal parameters.
   * This function does not resolve the actuals into their sizerefs (only name substitution).
   * TODO: Currently unused. This may be useful in evaluating MatchExpr where each
   *       branch returns a function and we want to normalize all the formal args
   *       with a common name and merge. See comment in MatchExpr.
   *)
  let substitute_actuals f (params, ressize, gup) actuals =
    if List.length params != List.length actuals then
      fail1 "Number of actual arguments and formal parameters mismatch" (ER.get_loc (get_rep f))
    else
      (* replace param with actual in baseref. *)
      let baseref_replacer param actual b = 
        match b with
        | Var i when (get_id i) = (get_id param) -> Var actual
        | _ -> b
      in
      (* replace param with actual in sizeref. *)
      let sizeref_replacer param actual s' =
        let rec replacer s =
          match s with
          | Base b -> Base (baseref_replacer param actual b)
          | Length s' -> Length (replacer s')
          | Component s' -> Component (replacer s')
          | Fst s' -> Fst (replacer s')
          | Snd s' -> Snd (replacer s')
          | Container s' -> Container (replacer s')
          | PairS (s1', s2') -> PairS (replacer s1', replacer s2')
          | BranchTaken (i, sr) -> BranchTaken (i, replacer sr)
          | MsgS srlist ->
            let srlist' = List.map (fun v -> replacer v) srlist in
            MsgS (srlist')
          | BApp (b, srlist) ->
            let srlist' = List.map (fun v -> replacer v) srlist in
            BApp (b, srlist')
          | SApp (id, srlist) ->
            let id' = if (get_id id) = (get_id param) then actual else id in
            let srlist' = List.map (fun v -> replacer v) srlist in
            SApp (id', srlist')
        in
        replacer s'
      in
      (* replace param with actual in guref. *)
      let rec guref_replacer param actual v =
        match v with
        | SizeOf s ->
          let r = (sizeref_replacer param actual s) in
          SizeOf r
        | GApp (id, gurlist) ->
          let id' = if (get_id id) = (get_id param) then actual else id in
          let gurlist' = List.map (fun v -> sizeref_replacer param actual v) gurlist in
          GApp (id', gurlist')
        | GPol p -> GPol (polynomial_replacer param actual p)
      and
      (* replace param with actual in a signature. *)
      polynomial_replacer param actual pol =
        var_replace_pn pol ~f:(fun gur -> guref_replacer param actual gur)
      in
      let ressize' = List.fold_left2 (fun sr param actual ->
        sizeref_replacer param actual sr
      ) ressize params actuals in
      let gup' = List.fold_left2 (fun pol param actual ->
        polynomial_replacer param actual pol
      ) gup params actuals in
      (* We have substitued parameters with actual arguments.
         So the first component of the result is empty. *)
      pure ([], ressize', gup')

  (* replace param with actual in sizeref. *)
  let substitute_resolved_actual_sizeref param actual s' =
    (* replace param with actual in baseref. *)
    let substitute_resolved_actual_baseref param actual b = 
      match b with
      | Var i when (get_id i) = (get_id param) -> pure actual
      | _ -> pure @@ Base b
    in
    let rec replacer s =
      match s with
      | Base b -> substitute_resolved_actual_baseref param actual b
      | Length s' ->
        let%bind r = replacer s' in
        pure @@ Length (r)
      | Component s' ->
        let%bind r = replacer s' in
        pure @@ Component r
      | Fst s' ->
        let%bind r = replacer s' in
        pure @@ Fst r
      | Snd s' ->
        let%bind r = replacer s' in
        pure @@ Snd r
      | Container s' ->
        let%bind r = replacer s' in
        pure @@ Container r
      | PairS (s1', s2') ->
        let%bind r1 = replacer s1' in
        let%bind r2 = replacer s2' in
        pure @@ PairS (r1, r2)
      | BranchTaken (i, sr) ->
        let%bind r = replacer sr in
        pure @@ BranchTaken (i, r)
      | MsgS srlist ->
        let%bind srlist' = mapM ~f:(fun v -> replacer v) srlist in
        pure @@ MsgS (srlist')
      | BApp (b, srlist) ->
        let%bind srlist' = mapM ~f:(fun v -> replacer v) srlist in
        pure @@ BApp (b, srlist')
      | SApp (id, srlist) ->
        let%bind id' = if (get_id id) = (get_id param) then
          (* We know what to do only when "actual" is Base(Var(_)). *)
          (match actual with
          | Base(Var(id'')) -> pure id''
          (* See TODO in definition of sizeref where SApp should have sizeref instead of ident. *)
          | _ ->fail1 "Functions cannot be wrapped in ADTs" (ER.get_loc (get_rep id))
          )
        else pure id in
        (* We don't do anything with id as that will be expanded (not just resolved). *)
        let%bind srlist' = mapM ~f:(fun v -> replacer v) srlist in
        pure @@ SApp (id', srlist')
    in
      replacer s'

  (* replace param with actual in guref. *)
  let rec substitute_resolved_actual_guref param actual v =
    match v with
    | SizeOf s ->
      let%bind r = (substitute_resolved_actual_sizeref param actual s) in
      pure @@ SizeOf r
    | GApp (id, gurlist) ->
      let%bind id' = if (get_id id) = (get_id param)
      then
        (* We know what to do only when "actual" is Base(Var(_)). *)
        (match actual with
        | Base(Var(id'')) -> pure id''
        (* See TODO in definition of sizeref where SApp should have guref instead of ident. *)
        | _ ->fail1 "Functions cannot be wrapped in ADTs" (ER.get_loc (get_rep id))
        )
      else pure id in
      (* We don't do anything with id as that will be expanded (not just resolved). *)
      let%bind gurlist' = mapM ~f:(fun v -> substitute_resolved_actual_sizeref param actual v) gurlist in
      pure @@ GApp (id', gurlist')
    | GPol p ->
      let%bind p' = polynomial_replacer param actual p in
      pure @@ GPol (p')
  and
  (* replace param with actual in a gu polynomial. *)
  polynomial_replacer param actual pol =
    let exception Resolv_error of scilla_error list in
    let pol' = 
      try 
        (* This circus with exceptions is to avoid having
          * Polynomial utilities deal with our "result". *)
        pure @@ var_replace_pn pol ~f:(fun gur ->
          (match substitute_resolved_actual_guref param actual gur with
          | Ok (gur') -> gur'
          | Error s -> raise (Resolv_error s))
        )
      with
      | Resolv_error s -> fail s
      | Polynomial_error s -> fail0 s
    in
      pol'

  let substitute_resolved_actuals_sizeref_list ressize params actuals =
    if List.length params != List.length actuals then
      fail0 "Number of actual arguments and formal parameters mismatch in sizeref substitution."
    else
      List.fold_left2 (fun sr' param actual ->
        let%bind sr = sr' in
        substitute_resolved_actual_sizeref param actual sr
      ) (pure ressize) params actuals

  let substitute_resolved_actuals_guref_list gup params actuals =
    if List.length params != List.length actuals then
      fail0 "Number of actual arguments and formal parameters mismatch in guref substitution."
    else
      List.fold_left2 (fun pol' param actual ->
        let%bind pol = pol' in
        polynomial_replacer param actual pol
      ) (pure gup) params actuals

  (* Resolve and expand variables and lambdas (whenever possible) in sr and pol. *)
  let resolve_expand genv sr pol =
      let baseref_resolver b = 
        match b with
        | Var i -> 
          let%bind (_, resr, _) =
            GUAEnv.resolvS genv (get_id i) ~lopt:(Some(get_rep i)) in
          pure resr
        | Const _ | BCState _ -> pure @@ Base(b)
      in
      (* replace param with actual in sizeref. *)
      let sizeref_resolver s' =
        let rec resolver s =
          match s with
          | Base b -> baseref_resolver b
          | Length s' ->
            let%bind r = resolver s' in
            pure @@ Length (r)
          | Component s' ->
            let%bind r = resolver s' in
            pure @@ Component r
          | Fst s' ->
            let%bind r = resolver s' in
            pure @@ Fst r
          | Snd s' ->
            let%bind r = resolver s' in
            pure @@ Snd r
          | Container s' ->
            let%bind r = resolver s' in
            pure @@ Container r
          | PairS (s1', s2') ->
            let%bind r1 = resolver s1' in
            let%bind r2 = resolver s2' in
            pure @@ PairS (r1, r2)
          | BranchTaken (i, sr) ->
            let%bind r = resolver sr in
            pure @@ BranchTaken (i, r)
          | MsgS srlist ->
            let%bind srlist' = mapM ~f:(fun v -> resolver v) srlist in
            pure @@ MsgS (srlist')
          | BApp (b, srlist) ->
            let%bind srlist' = mapM ~f:(fun v -> resolver v) srlist in
            pure @@ BApp (b, srlist')
          | SApp (id, srlist) ->
            let%bind (args, sr, _) = GUAEnv.resolvS genv (get_id id) ~lopt:(Some(get_rep id)) in
            if args = []
            then
              (* No known expansion  *)
              let%bind srlist' = mapM ~f:(fun v -> resolver v) srlist in
              pure @@ SApp (id, srlist')
            else
              let%bind sr' = substitute_resolved_actuals_sizeref_list sr args srlist in
              resolver sr'
        in
        resolver s'
      in
      (* replace param with actual in guref. *)
      let rec guref_resolver v =
        match v with
        | SizeOf s ->
          let%bind r = (sizeref_resolver s) in
          pure @@ SizeOf r
        | GApp (id, gurlist) ->
          let%bind (args, _, gup) = GUAEnv.resolvS genv (get_id id) ~lopt:(Some(get_rep id)) in
          if args = []
          then
            (* No known expansion  *)
            let%bind gurlist' = mapM ~f:(fun v -> sizeref_resolver v) gurlist in
            pure @@ GApp (id, gurlist')
          else
            let%bind sr' = substitute_resolved_actuals_guref_list gup args gurlist in
            let%bind p = polynomial_resolver sr' in
            pure @@ GPol p
        | GPol p ->
          let%bind p' = polynomial_resolver p in
          pure @@ GPol p'
      and
      (* replace param with actual in a signature. *)
      polynomial_resolver pol =
        let exception Resolv_error of scilla_error list in
        let pol' = 
          try 
            (* This circus with exceptions is to avoid having
              * Polynomial utilities deal with our "result". *)
            pure @@ var_replace_pn pol ~f:(fun gur ->
              (match guref_resolver gur with
              | Ok (gur') -> gur'
              | Error s -> raise (Resolv_error s))
            )
          with
          | Resolv_error s -> fail s
          | Polynomial_error s -> fail0 s
        in
          pol'
      in
      let%bind sr' = sizeref_resolver sr in
      let%bind pol' =  polynomial_resolver pol in
      pure (sr', pol')

  (* For a pattern "pat" match on "msref", add binders to genv. *)
  let rec bind_pattern genv msref pat =
    match pat with
    | Wildcard -> pure genv
    | Binder i -> pure @@ GUAEnv.addS genv (get_id i) ([], msref, empty_pn)
    | Constructor (cname, plist) ->
      (match cname with
      | "True" | "False" | "Nil" | "None" -> pure @@ genv
      | "Some" ->
        (* TypeChecker will ensure that plist has unit length. *)
        let arg = List.nth plist 0 in
        bind_pattern genv (Component(msref)) arg
      | "Cons" ->
        (* TypeChecker will ensure that plist has two elements. *)
        let arg0 = List.nth plist 0 in
        let arg1 = List.nth plist 1 in
        let%bind genv' = bind_pattern genv (Component(msref)) arg0 in
        let%bind genv'' = bind_pattern genv' (msref) arg1 in
        pure genv''
      | "Pair" ->
        (* TypeChecker will ensure that plist has two elements. *)
        let arg0 = List.nth plist 0 in
        let arg1 = List.nth plist 1 in
        let%bind genv' = bind_pattern genv (Fst(msref)) arg0 in
        let%bind genv'' = bind_pattern genv' (Snd(msref)) arg1 in
        pure genv''
      | _ -> fail0 (Printf.sprintf "Unsupported constructor %s in gas analysis." cname))


  (* built-in op costs are propotional to size of data they operate on. *)
  (* TODO: Have all numbers in one place. Integrate with Gas.ml *)
  let builtin_cost op params =

    let open PrimTypes in
    let ops = get_id op in
    let opl = ER.get_loc (get_rep op) in
    (* Types of our paramters. *)
    let tparams = List.map (fun p -> (ER.get_type (get_rep p)).tp) params in

    let tvar a = TypeVar (a) in
    (* Make a simple identifier of type 'A *)
    let si a = ER.mk_id (mk_ident a) (tvar "'A") in
     (* Make a simple polynomial from string a *)
    let sp a = single_simple_pn (SizeOf(Base(Var(si a)))) in
    let arg_err s = "Incorrect arguments to builtin %s" ^ s in
    let ressize op actuals =
      (* These must be resolved in the caller when expanding the lambdas. *)
      let srparams = List.map (fun i -> Base(Var(i))) actuals in
      BApp (op, srparams)
    in

    match ops with
    | "eq" ->
      if List.length params <> 2 then fail1 (arg_err ops) opl else
      (* TODO: Use max(a, b)? This one is ok for now for a worst case report. *)
      let%bind pn =
        let t = List.nth tparams 0 in
        (match t with
          | PrimType _ when is_int_type t ->
            (match (int_width t) with
             | Some 32 | Some 64 -> pure @@ const_pn 4
             | Some 128 -> pure @@ const_pn 8
             | Some 256 -> pure @@ const_pn 16
             | _ -> fail1 (arg_err ops) opl
             )
          (*  eq (a, b) = a. *)
          | PrimType _ when t = string_typ -> pure @@ sp "a"
          | PrimType _ when t = bnum_typ -> pure @@ const_pn 32
          (*  eq (a, b) = a. *)
          | PrimType _ when t = bystr_typ -> pure @@ sp "a"
          | _ ->
            (match bystrx_width t with
            | Some w -> pure @@ const_pn w
            | None -> fail1 (arg_err ops) opl
            )
        )
      in
      pure ([si "a"; si "b"], ressize ops params, pn)
    | "concat" -> (* concat(a, b) = a + b *)
      if List.length params <> 2 then fail1 (arg_err ops) opl else
      pure ([si "a"; si "b"], ressize ops params, add_pn (sp "a") (sp "b"))
    | "substr" -> (* substr(a, o, l) = a *)
      if List.length params <> 3 then fail1 (arg_err ops) opl else
      pure ([si "a"; si "o"; si "l"], ressize ops params, sp "a")
    | "blt" | "badd" -> (* blt/badd(a, b) = 32 *)
      if List.length params <> 2 then fail1 (arg_err ops) opl else
      pure ([si "a"; si "b"], ressize ops params, const_pn 32)
    | "dist" -> (* dist(a, b) = 32 *)
      if List.length params <> 2 then fail1 (arg_err ops) opl else
      pure ([si "a"; si "b"], ressize ops params, const_pn 32)
    | "to_bystr" -> (* to_bystr(a) = a *)
      if List.length params <> 1 then fail1 (arg_err ops) opl else
      pure ([si "a"], ressize ops params, sp "a")
    | "sha256hash" | "ripemd160hash" | "keccak256hash" ->
      (* hash(a) = a * 15. TODO: Support functions in polynomial. *)
      if List.length params <> 1 then fail1 (arg_err ops) opl else
      pure ([si "a"], ressize ops params, mul_pn (sp "a") (const_pn 15))
    | "schnorr_gen_key_pair" ->
      if List.length params <> 0 then fail1 (arg_err ops) opl else
      pure ([si "a"; si "b"], ressize ops params, const_pn 20)
    | "schnorr_sign" -> (* sign(a) = (a * 15) + 350 *)
      (* TODO: Support functions in polynomial. *)
      if List.length params <> 3 then fail1 (arg_err ops) opl else
      pure ([si "a"; si "b"; si "m"], ressize ops params, add_pn (mul_pn (sp "a") (const_pn 15)) (const_pn 350))
    | "schnorr_verify" -> (* sign(a) = (a * 15) + 250 *)
      (* TODO: Support functions in polynomial. *)
      if List.length params <> 3 then fail1 (arg_err ops) opl else
      pure ([si "a"; si "b"; si "m"], ressize ops params, add_pn (mul_pn (sp "a") (const_pn 15)) (const_pn 250))
    | "contains" | "get" -> (* contains/get(m, key) = 1 *)
      if List.length params <> 2 then fail1 (arg_err ops) opl else
      pure ([si "m"; si "key"], ressize ops params, const_pn 1)
    | "put" -> (* put(m, key, value) = 1 + Length (m) *)
      if List.length params <> 3 then fail1 (arg_err ops) opl else
      let pol = single_simple_pn (SizeOf(Length(Base(Var(si "m"))))) in
      pure ([si "m"; si "key"; si "val"], ressize ops params, add_pn pol (const_pn 1))
    | "remove" -> (* remove(m, key) = 1 + Length (m) *)
      if List.length params <> 2 then fail1 (arg_err ops) opl else
      let pol = single_simple_pn (SizeOf(Length(Base(Var(si "m"))))) in
      pure ([si "m"; si "key"], ressize ops params, add_pn pol (const_pn 1))
    | "to_list" | "size" -> (* 1 + length (m) *)
      if List.length params <> 1 then fail1 (arg_err ops) opl else
      let pol = single_simple_pn (SizeOf(Length(Base(Var(si "m"))))) in
      pure ([si "m"], ressize ops params, add_pn pol (const_pn 1))
    | "add" | "sub" | "mul" | "div" | "rem" | "lt" |
      "to_int32" | "to_int64" | "to_int128" | "to_int256" |
      "to_uint32" | "to_uint64" | "to_uint128" | "to_uint256" ->
      if List.length params <> 2 && List.length params <> 1 then fail1 (arg_err ops) opl else
      let base = match ops with | "mul" | "div" | "rem" -> 20 | _ -> 4 in
      let%bind c = 
        (match int_width (List.nth tparams 0) with
         | Some 32 | Some 64 -> pure base
         | Some 128 -> pure @@ base * 2
         | Some 256 -> pure @@ base * 4
         | _ -> fail1 (arg_err ops) opl
        ) in
      pure ([si "a"; si "b"], ressize ops params, const_pn c)
    | _ -> fail1 ("Unknown builtin " ^ ops) opl

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
      (* We have signature for the body, just add arg as a parameter to that function signature. *)
      pure (arg::sargs, rsize, (add_pn rgas cc))
    | App (f, actuals) ->
      (* Build a lambda for "f". It will be expanded next (along with inner lambdas if possible). *)
      let srparams = List.map (fun i -> Base(Var(i))) actuals in
      let u = SApp(f, srparams) in
      let v = single_simple_pn (GApp (f, srparams)) in
      (* Expand all lambdas that we can. *)
      let%bind (ressize', gup') = resolve_expand genv u v in
      (* TODO: Return value having no arguments implies partial application not supported. *)
      pure ([], ressize', (add_pn gup' cc))
    | Builtin (b, actuals) ->
      (* Handle builtin-s like we handle function application. *)
      let%bind bsig = builtin_cost b actuals in
      let genv' = GUAEnv.addS genv (get_id b) bsig in
      (* We have the function signature ready, apply and expand it. *)
      (* Build a lambda for "f". It will be expanded next (along with inner lambdas if possible). *)
      let srparams = List.map (fun i -> Base(Var(i))) actuals in
      let u = SApp(b, srparams) in
      let v = single_simple_pn (GApp (b, srparams)) in
      (* Expand all lambdas that we can. *)
      let%bind (ressize', gup') = resolve_expand genv' u v in
      (* TODO: Return value having no arguments implies partial application not supported. *)
      pure ([], ressize', (add_pn gup' cc))
    | Let (i, _, lhs, rhs) ->
      let%bind lhs_sig = gua_expr genv lhs in
      let genv' = GUAEnv.addS genv (get_id i) lhs_sig in
      let%bind rhs_sig = gua_expr genv' rhs in
      (* gas consumption is sum of both. *)
      let (args, ressize, gup) = rhs_sig in
      let (lhs_args, _, gup') = lhs_sig in
      (* Gas cost for this let expression is sum of LHS cost (if not a function) and RHS cost. *)
      let p = if lhs_args <> [] then gup else add_pn gup gup' in
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
          let%bind (_, compsize, _) = GUAEnv.resolvS genv (get_id arg) ~lopt:(Some(get_rep arg)) in
          pure @@ Container(compsize)
        | "Pair" ->
          (* TypeChecker will ensure that actuals has two elements. *)
          let arg0 = List.nth actuals 0 in
          let arg1 = List.nth actuals 1 in
          let%bind (_, compsize0, _) = GUAEnv.resolvS genv (get_id arg0) ~lopt:(Some(get_rep arg0)) in
          let%bind (_, compsize1, _) = GUAEnv.resolvS genv (get_id arg1) ~lopt:(Some(get_rep arg1)) in
          pure @@ PairS(compsize0, compsize1)
        | _ -> fail1 (Printf.sprintf "Unsupported constructor %s in gas analysis." cname)
                     (ER.get_loc rep)
        )
      in
        pure ([], ressize, cc)
    | MatchExpr (x, clauses) ->
      let%bind (_, xsize, _) = GUAEnv.resolvS genv (get_id x) ~lopt:(Some(get_rep x)) in
      (*  1. TODO: How to express sizeref of result across branches?
       *     Currently we're using the result sizeref of the last branch. 
       *  2. If the return type of the MatchExpr is a function then the
       *     arguments (first component of the signature) cannot be
       *     ignored as we're doing now. 
       *     TODO: Normalize argument names and merge them. This can be done
       *     having a common set of actuals names for all branches.
       *     (Use substitute_actuals to replace with a common set of param names).
       *)
      let%bind ((args, ressize, gup), _) = foldM ~f:(fun ((_, _, apn), i) (pat, branch) ->
        let%bind genv' = bind_pattern genv xsize pat in
        let%bind (_, bsize, bgu) = gua_expr genv' branch in
        (* combine branch gas use with other branches using "i" as a weight. *)
        let weight = SizeOf (BranchTaken (i, xsize)) in
        let bpn = mul_pn bgu (single_simple_pn weight) in
        pure (([], bsize, add_pn apn bpn), i+1)
      ) ~init:(([], xsize, empty_pn), 0) clauses in
      pure (args, ressize, add_pn gup cc)
    | Fixpoint (f, _, _) ->
      fail1 (Printf.sprintf "Fixpoint %s not supported." (get_id f)) (ER.get_loc (get_rep f))
    | TFun (_, body) ->
      (* Nothing to do except analyzing the body. *)
      let%bind (sargs, rsize, rgas) = gua_expr genv body in
      pure (sargs, rsize, (add_pn rgas cc))      
    | TApp (tf, _) ->
      (* Just return the signature of tf. *)
      GUAEnv.resolvS genv (get_id tf) ~lopt:(Some(get_rep tf))
    | Message plist ->
       (* Similar to "Literal", we only spend a small cost (cc) for message creation
        * but charge based on size of message in SendStmt. *)
        let%bind splist = mapM ~f:(fun (_, pl) ->
          (match pl with
          | MTag s -> pure @@ Base(Const(PrimTypes.string_typ, String.length s))
          | MLit l ->
            let%bind lt = TU.literal_type l in
            let%bind lc = Gas.literal_cost l in
            pure @@ Base(Const(lt, lc))
          | MVar i ->
            let%bind (_, irs, _) = GUAEnv.resolvS genv (get_id i) ~lopt:(Some(get_rep i)) in
            pure irs
          )
        ) plist in
        pure ([], MsgS(splist), cc)

  (* Hardcode signature for folds. *)
  let analyze_folds genv =
    (*  list_foldr: forall 'A . forall 'B . g:('A -> 'B -> 'B) -> b:'B -> a:(List 'A) -> 'B *)
    let a = ER.mk_id (mk_ident "a") (ADT("List", [TypeVar("'A")])) in
    let g = ER.mk_id (mk_ident "g") (FunType(TypeVar("'A"), FunType(TypeVar("'B"), TypeVar("'B")))) in
    let b = ER.mk_id (mk_ident "b") (TypeVar("'B")) in
    let lendep = SizeOf(Length(Base(Var(a)))) in
    let gapp = GApp(g, [Component(Base(Var(a))); Base(Var(b))]) in
    (* Gas use polynomial = Length(a) * GApp(g, [Component(a), b]) *)
    let gupol = mul_pn (single_simple_pn lendep) (single_simple_pn gapp) in
    let ressize = SApp(g, [Base(Var(a));Base(Var(b))]) in
    let list_foldr_signature = ([g;b;a], ressize, gupol) in
    let genv' = GUAEnv.addS genv "list_foldr" list_foldr_signature in
    (* list_foldl: forall 'A . forall 'B . g:('B -> 'A -> 'B) -> b:'B -> a:(List 'A) -> 'B *)
    let gapp' = GApp(g, [Base(Var(b)); Component(Base(Var(a)));]) in
    let gupol' = mul_pn (single_simple_pn lendep) (single_simple_pn gapp') in
    let ressize' = SApp(g, [Base(Var(b));Base(Var(a))]) in
    let list_foldl_signature = ([g;b;a], ressize', gupol') in
    let gapp'' = GUAEnv.addS genv' "list_foldl" list_foldl_signature in
    gapp''

  (* Expand polynomials that contain GPol. *)
  let expand_parameters pol =
    expand_parameters_pn pol ~f:(function | SizeOf _ | GApp _ -> None | GPol p -> Some p)

  (* Return gas use and result sizeref polynomials of evaluating a sequence of statements. *)
  let rec gua_stmt genv gupol (stmts : stmt_annot list) =
    match stmts with
    | [] -> pure gupol
    | (s, sloc) :: sts ->
      (match s with
        | Load (x, r) ->
          (* The cost of load depends on the size of the state variable. *)
          let gupol' = add_pn gupol (single_simple_pn (SizeOf(Base(Var(r))))) in
          let signx = ([], Base(Var(r)), empty_pn) in
          let genv' = GUAEnv.addS genv (get_id x) signx in
          gua_stmt genv' gupol' sts
        | Store (_, r) ->
          (* The cost of store depends on the original size and the new size *)
          (* TODO: Incorporate actual cost from Gas.ml which has max() and subtract. *)
          let gupol' = add_pn gupol (single_simple_pn (SizeOf(Base(Var(r))))) in
          gua_stmt genv gupol' sts
        | Bind (x, e) ->
          let%bind signe = gua_expr genv e in
          let genv' = GUAEnv.addS genv (get_id x) signe in
          (* Simple constant const for binding. *)
          let gupol' = add_pn gupol (const_pn 1) in
          gua_stmt genv' gupol' sts
        | MapUpdate(_, klist, ropt) ->
          let nindices = (const_pn (List.length klist)) in
          let c = (match ropt with
          | Some i -> single_simple_pn (SizeOf(Base(Var(i)))) (* update *)
          | None -> empty_pn) (* delete *)in
          let gupol' = add_pn nindices c in
          let gupol'' = add_pn gupol gupol' in
          gua_stmt genv gupol'' sts
        | MapGet(x, m, klist, fetchval) ->
          let nindices = (const_pn (List.length klist)) in
          let (sign, pol) =
            if fetchval then 
              let ressize = List.fold_left (fun acc _ -> Component(acc)) (Base(Var(m))) klist in
              ([], ressize, empty_pn), (add_pn nindices (single_simple_pn (SizeOf(ressize))))
            else
              (* TODO: How to represent result of `exists` in map? ?*)
              ([], Base(Var(m)), empty_pn), nindices
          in
          let genv' = GUAEnv.addS genv (get_id x) sign in
          let gupol' = add_pn pol gupol in
          gua_stmt genv' gupol' sts
        | ReadFromBC (x, bf) ->
          (* Bind x to blockchain variable bf *)
          let signx = ([], Base(BCState(bf)), empty_pn) in
          (* Constant cost of 1 to load a blockchain variable. *)
          let gupol' = add_pn gupol (const_pn 1) in
          let genv' = GUAEnv.addS genv (get_id x) signx in
          gua_stmt genv' gupol' sts
        | MatchStmt (x, clauses) ->
          let%bind (_, xsize, _) = GUAEnv.resolvS genv (get_id x) ~lopt:(Some(get_rep x)) in
          let num_clauses = const_pn (List.length clauses) in
          let%bind (gupol', _) = foldM ~f:(fun (apn, i) (pat, branch) ->
            let%bind genv' = bind_pattern genv xsize pat in
            let%bind bgu = gua_stmt genv' empty_pn branch in
            (* combine branch gas use with other branches using "i" as a weight. *)
            let weight = SizeOf (BranchTaken (i, xsize)) in
            let bpn = mul_pn bgu (single_simple_pn weight) in
            pure (add_pn apn bpn, i+1)
          ) ~init:(add_pn gupol num_clauses, 0) clauses in
          gua_stmt genv gupol' sts
        | AcceptPayment ->
          (* Constant cost of 1. *)
          gua_stmt genv (add_pn gupol (const_pn 1)) sts
        | SendMsgs i | CreateEvnt i->
          (* We can at best convey the size of i *)
          let%bind (_, s, _) = GUAEnv.resolvS genv (get_id i) ~lopt:(Some(get_rep i)) in
          let gupol' = add_pn gupol (single_simple_pn (SizeOf(s))) in
          gua_stmt genv gupol' sts
        | _ -> fail1 "Unsupported statement" (SR.get_loc sloc)
      )

  (* Bind identifiers to just sizeref wrappers of themselves. *)
  let identity_bind_ident_list genv idlist =
    List.fold_left (fun acc_genv i ->
        let i' = Base(Var(i)) in
        GUAEnv.addS acc_genv (get_id i) ([], i', empty_pn)
      ) genv idlist

  let gua_transition genv (trans : transition) =
    let open PrimTypes in
    let si a t = ER.mk_id (mk_ident a) t in
    let all_params =[
        ((si ContractUtil.MessagePayload.sender_label (bystrx_typ 20)), (bystrx_typ 20));
        ((si ContractUtil.MessagePayload.amount_label uint128_typ), uint128_typ)
      ] 
      @ trans.tparams in
    (* Add params to the environment. *)
    let genv' = identity_bind_ident_list genv 
      (List.map (fun (i, _) -> i) all_params) in
    (* TODO: Add bytesize of message.json. *)
    gua_stmt genv' empty_pn trans.tbody

  (* Bind lib entries to signatures. *)
  let gua_libentries genv (lel : lib_entry list) =
    foldM ~f:(fun genv le ->
      let%bind esig = gua_expr genv le.lexp in
      pure @@ GUAEnv.addS genv (get_id le.lname) esig
    ) ~init:(genv) lel

  let gua_module (cmod : cmodule) (elibs : library list) =
    (* Get bindings for folds *)
    let genv_folds = analyze_folds (GUAEnv.mk ()) in

    (* Analyze external libraries first *)
    let%bind genv_elibs =
      foldM ~f:(fun genv lib -> gua_libentries genv lib) ~init:genv_folds 
        (List.map (fun l -> l.lentries) elibs)
    in

    (* Analyze contract libraries *)
    let%bind genv_lib =
      match cmod.libs with
      | Some l -> gua_libentries genv_elibs l.lentries
      | None -> pure @@ GUAEnv.mk()
    in

    (* Bind contract parameters. *)
    let si a t = ER.mk_id (mk_ident a) t in
    let all_cparams =[
        ((si ContractUtil.creation_block_label PrimTypes.bnum_typ), PrimTypes.bnum_typ);
      ]
      @ cmod.contr.cparams
    in
    let genv_cparams = identity_bind_ident_list genv_lib
      (List.map (fun (i, _) -> i) all_cparams) in

    (* Bind state variables. *)
    (* TODO: account for the cost of evaluating state initializers. *)
    let genv_cfields = identity_bind_ident_list genv_cparams
      (List.map (fun (i, _, _) -> i) cmod.contr.cfields) in
    (* Analyse each transition and report it's gas use polynomial. *)
    let%bind pols = mapM ~f:(fun tr ->
      let%bind pol = gua_transition genv_cfields tr in
      pure @@ (tr.tname, expand_parameters pol)
    ) cmod.contr.ctrans in
    pure pols

  (* A simple wrapper to analyze an isolated expression. *)
  let gua_expr_wrapper erep =
    let genv = GUAEnv.mk () in
    let genv' = analyze_folds genv in
    let%bind (params, sr, pol) = gua_expr genv' erep in
    (* Expand all parameters (GPol) in the polynomial. *)
    let pol' = expand_parameters pol in
    Printf.printf "Gas usage polynomial:\n%s\n\n" (sprint_signature (params, sr, pol'));
    pure (params, sr, pol')

  end
