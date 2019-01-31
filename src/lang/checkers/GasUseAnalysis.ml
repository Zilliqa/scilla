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
    (* Numerical constant *)
    | Const of int
    (* An input coming from the block chain state. *)
    | BCState of string

  type sizeref =
    (* For sizes of PrimTypes, function value. *)
    | Base of baseref
    (* For List and Map types. *)
    | Length of sizeref
    (* For Elements of Lists and Maps. *)
    | Element of sizeref
    (* A polynomial function of other sizerefs *)
    | SPol of sizeref polynomial
    (* An abstract maximum across branches. *)
    (* TODO: Remove this constructor, we're not using it anymore. *)
    | MaxB of sizeref list
    (* Arbitrary math function of sizerefs. *)
    | MFun of string * sizeref list
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
    | Const i -> Printf.sprintf "%d" i
    | BCState s -> "BlockChain: " ^ s

  (* Given a size reference, print a description for it. *)
  let rec sprint_sizeref = function
    | Base b -> sprint_baseref b
    (* For List and Map types. *)
    | Length sr'-> "Length of: " ^ (sprint_sizeref sr')
    (* For Elements of Lists and Maps. *)
    | Element sr' -> "Element of: " ^ (sprint_sizeref sr')
    | SPol pn -> sprint_pn pn ~f:(fun sr -> "(" ^ sprint_sizeref sr ^ ")")
    | MaxB srlist ->  "Max (" ^
      (List.fold_left (fun acc sr -> acc ^ (if acc = "" then "" else ",") ^ (sprint_sizeref sr)) "" srlist) ^ ")"
    | MFun (s, srlist) -> s ^ " (" ^
      (List.fold_left (fun acc sr -> acc ^ (if acc = "" then "" else ",") ^ (sprint_sizeref sr)) "" srlist) ^ ")"
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

  let sprint_signature (params, sr, pn) =
    let args = 
      if (params = []) then "" else 
      "Parameter list: " ^ 
        (List.fold_left (fun acc p -> acc ^ (get_id p) ^ " ") "( " params) ^ ")\n"
    in
    (args ^ "Gas use polynomial:\n" ^ sprint_gup pn ^ "\nResult size: " ^ (sprint_sizeref sr))

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

  let rec sizeref_to_guref sr = match sr with
    | Base (Const c) -> GPol (const_pn c)
    | Base _ | Length _| Element _ | MaxB _ | MFun _ | BApp _ | SApp _ -> SizeOf (sr)
    | SPol p ->
      let term_replacer t =
        let (coef, vars) = t in
        let vars' = List.map (fun (v, p) ->
          let v' = sizeref_to_guref v in
          (v', p)
        ) vars in
        (coef, vars')
      in
      let gpol = List.map (fun t -> term_replacer t) p in
      GPol (gpol)

  (* Expand polynomials that contain GPol. *)
  let rec expand_parameters pol =
    expand_parameters_pn pol ~f:(function
    | SizeOf sr ->
      (match sr with
      | Base (Const c) -> Some(const_pn c)
      | _ -> Some (single_simple_pn (sizeref_to_guref sr)))
    | GApp _ -> None (* TODO *)
    | GPol p -> Some (expand_parameters p))

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
      let baseref_replacer param_actual_map b =
        match b with
        | Var i ->
          (match List.assoc_opt (get_id i) param_actual_map with
          | Some act -> Var act
          | None -> b)
        | _ -> b
      in
      (* replace param with actual in sizeref. *)
      let sizeref_replacer param_actual_map s' =
        let rec replacer s =
          match s with
          | Base b -> Base (baseref_replacer param_actual_map b)
          | Length s' -> Length (replacer s')
          | Element s' -> Element (replacer s')
          | SPol sp -> SPol (polynomial_replacer sp)
          | BApp (b, srlist) ->
            let srlist' = List.map (fun v -> replacer v) srlist in
            BApp (b, srlist')
          | MaxB srlist ->
            let srlist' = List.map (fun v -> replacer v) srlist in
            MaxB (srlist')
          | MFun (s, srlist) ->
            let srlist' = List.map (fun v -> replacer v) srlist in
            MFun (s, srlist')
          | SApp (id, srlist) ->
            let id' =
              (match List.assoc_opt (get_id id) param_actual_map with
              | Some id'' -> id''
              | None -> id)
            in
            let srlist' = List.map (fun v -> replacer v) srlist in
            SApp (id', srlist')
        and polynomial_replacer pol =
          var_replace_pn pol ~f:(fun sr -> replacer sr)
        in
        replacer s'

      in
      (* replace param with actual in guref. *)
      let rec guref_replacer param_actual_map v =
        match v with
        | SizeOf s ->
          let r = (sizeref_replacer param_actual_map s) in
          SizeOf r
        | GApp (id, gurlist) ->
          let id' =
            (match List.assoc_opt (get_id id) param_actual_map with
            | Some id'' -> id''
            | None -> id)
          in
          let gurlist' = List.map (fun v -> sizeref_replacer param_actual_map v) gurlist in
          GApp (id', gurlist')
        | GPol p -> GPol (polynomial_replacer param_actual_map p)
      and
      (* replace param with actual in a signature. *)
      polynomial_replacer param_actual_map pol =
        var_replace_pn pol ~f:(fun gur -> guref_replacer param_actual_map gur)
      in
      let param_actual_map = List.combine params actuals in
      let ressize' = sizeref_replacer param_actual_map ressize in
      let gup' = polynomial_replacer param_actual_map gup in
      (* We have substitued parameters with actual arguments.
         So the first Element of the result is empty. *)
      pure ([], ressize', gup')

  (* replace param with actual in sizeref. *)
  let substitute_resolved_actual_sizeref param_actual_map s' =
    (* replace param with actual in baseref. *)
    let substitute_resolved_actual_baseref param_actual_map b =
      match b with
      | Var i ->
        (match List.assoc_opt (get_id i) param_actual_map with
        | Some act -> pure @@ act
        | None -> pure @@ Base b)
      | _ -> pure @@ Base b
    in
    let rec replacer s =
      match s with
      | Base b -> substitute_resolved_actual_baseref param_actual_map b
      | Length s' ->
        let%bind r = replacer s' in
        pure @@ Length (r)
      | Element s' ->
        let%bind r = replacer s' in
        pure @@ Element r
      | SPol sp ->
        let%bind p' = polynomial_replacer sp in
        pure @@ SPol (p')
      | BApp (b, srlist) ->
        let%bind srlist' = mapM ~f:(fun v -> replacer v) srlist in
        pure @@ BApp (b, srlist')
      | MaxB srlist ->
        let%bind srlist' = mapM ~f:(fun v -> replacer v) srlist in
        pure @@ MaxB srlist'
      | MFun (s, srlist) ->
        let%bind srlist' = mapM ~f:(fun v -> replacer v) srlist in
        pure @@ MFun (s, srlist')
      | SApp (id, srlist) ->
        let%bind id' =
          (match List.assoc_opt (get_id id) param_actual_map with
          | Some act ->
            (* We know what to do only when "actual" is Base(Var(_)). *)
            (match act with
            | Base(Var(id'')) -> pure id''
            (* See TODO in definition of sizeref where SApp should have sizeref instead of ident. *)
            | _ ->fail1 "Functions cannot be wrapped in ADTs" (ER.get_loc (get_rep id))
            )
          | None -> pure id)
        in
        (* We don't do anything with id as that will be expanded (not just resolved). *)
        let%bind srlist' = mapM ~f:(fun v -> replacer v) srlist in
        pure @@ SApp (id', srlist')
    and
    (* replace param with actual in a sizeref polynomial. *)
    polynomial_replacer pol =
      let exception Resolv_error of scilla_error list in
      let pol' = 
        try 
          (* This circus with exceptions is to avoid having
            * Polynomial utilities deal with our "result". *)
          pure @@ var_replace_pn pol ~f:(fun sr ->
            (match replacer sr with
            | Ok (sr') -> sr'
            | Error s -> raise (Resolv_error s))
          )
        with
        | Resolv_error s -> fail s
        | Polynomial_error s -> fail0 s
      in
        pol'
    in
      replacer s'

  (* replace param with actual in guref. *)
  let rec substitute_resolved_actual_guref param_actual_map v =
    match v with
    | SizeOf s ->
      let%bind r = (substitute_resolved_actual_sizeref param_actual_map s) in
      pure @@ SizeOf r
    | GApp (id, gurlist) ->
      let%bind id' =
        (match List.assoc_opt (get_id id) param_actual_map with
        | Some act ->
          (* We know what to do only when "actual" is Base(Var(_)). *)
          (match act with
          | Base(Var(id'')) -> pure id''
          (* See TODO in definition of sizeref where SApp should have guref instead of ident. *)
          | _ ->fail1 "Functions cannot be wrapped in ADTs" (ER.get_loc (get_rep id))
          )
        | None -> pure id)
      in
      (* We don't do anything with id as that will be expanded (not just resolved). *)
      let%bind gurlist' = mapM ~f:(fun v -> substitute_resolved_actual_sizeref param_actual_map v) gurlist in
      pure @@ GApp (id', gurlist')
    | GPol p ->
      let%bind p' = polynomial_replacer param_actual_map p in
      pure @@ GPol (p')
  and
  (* replace param with actual in a gu polynomial. *)
  polynomial_replacer param_actual_map pol =
    let exception Resolv_error of scilla_error list in
    let pol' = 
      try 
        (* This circus with exceptions is to avoid having
          * Polynomial utilities deal with our "result". *)
        pure @@ var_replace_pn pol ~f:(fun gur ->
          (match substitute_resolved_actual_guref param_actual_map gur with
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
      substitute_resolved_actual_sizeref (List.combine params actuals) ressize

  let substitute_resolved_actuals_guref_list gup params actuals =
    if List.length params != List.length actuals then
      fail0 "Number of actual arguments and formal parameters mismatch in guref substitution."
    else
      polynomial_replacer (List.combine params actuals) gup

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
          | Element s' ->
            let%bind r = resolver s' in
            pure @@ Element r
          | SPol sp ->
            let%bind p' = polynomial_resolver sp in
            pure @@ SPol p'
          | BApp (b, srlist) ->
            let%bind srlist' = mapM ~f:(fun v -> resolver v) srlist in
            pure @@ BApp (b, srlist')
          | MaxB srlist ->
            let%bind srlist' = mapM ~f:(fun v -> resolver v) srlist in
            pure @@ MaxB srlist'
          | MFun (s, srlist) ->
            let%bind srlist' = mapM ~f:(fun v -> resolver v) srlist in
            pure @@ MFun (s, srlist')
          | SApp (id, srlist) ->
            let%bind (args, sr, _) = GUAEnv.resolvS genv (get_id id) ~lopt:(Some(get_rep id)) in
            if args = []
            then
              (* No known expansion  *)
              let%bind srlist' = mapM ~f:(fun v -> resolver v) srlist in
              pure @@ SApp (id, srlist')
            else
              let args' = List.map (fun i -> get_id i) args in
              let%bind sr' = substitute_resolved_actuals_sizeref_list sr args' srlist in
              resolver sr'
        (* replace param with actual in a signature. *)
        and polynomial_resolver pol =
          let exception Resolv_error of scilla_error list in
          let pol' = 
            try 
              (* This circus with exceptions is to avoid having
                * Polynomial utilities deal with our "result". *)
              pure @@ var_replace_pn pol ~f:(fun sr ->
                (match resolver sr with
                | Ok (sr') -> sr'
                | Error s -> raise (Resolv_error s))
              )
            with
            | Resolv_error s -> fail s
            | Polynomial_error s -> fail0 s
          in
            pol'
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
            let args' = List.map (fun i -> get_id i) args in
            let%bind sr' = substitute_resolved_actuals_guref_list gup args' gurlist in
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
        bind_pattern genv (msref) arg
      | "Cons" ->
        (* TypeChecker will ensure that plist has two elements. *)
        let arg0 = List.nth plist 0 in
        let arg1 = List.nth plist 1 in
        let%bind genv' = bind_pattern genv (Element(msref)) arg0 in
        let%bind genv'' = bind_pattern genv' (msref) arg1 in
        pure genv''
      | "Pair" ->
        (* TypeChecker will ensure that plist has two elements. *)
        let arg0 = List.nth plist 0 in
        let arg1 = List.nth plist 1 in
        (* Bind both to the original element. *)
        let%bind genv' = bind_pattern genv (msref) arg0 in
        let%bind genv'' = bind_pattern genv' (msref) arg1 in
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
    let arg_err s = "Incorrect arguments to builtin " ^ s in
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
          | PrimType _ when is_int_type t || is_uint_type t ->
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
    | "to_bystr" -> (* to_bystr(a) = a *)
      if List.length params <> 1 then fail1 (arg_err ops) opl else
      pure ([si "a"], ressize ops params, sp "a")
    | "sha256hash" | "ripemd160hash" | "keccak256hash" ->
      (* hash(a) = a * 15. TODO: Support functions in polynomial. *)
      if List.length params <> 1 then fail1 (arg_err ops) opl else
      pure ([si "a"], ressize ops params, mul_pn (sp "a") (const_pn 15))
    | "ec_gen_key_pair" ->
      if List.length params <> 0 then fail1 (arg_err ops) opl else
      pure ([], ressize ops params, const_pn 20)
    | "schnorr_sign" -> (* sign(m) = (m * 15) + 350 *)
      (* TODO: Support functions in polynomial. *)
      if List.length params <> 3 then fail1 (arg_err ops) opl else
      pure ([si "a"; si "b"; si "m"], ressize ops params, add_pn (mul_pn (sp "m") (const_pn 15)) (const_pn 350))
    | "schnorr_verify" -> (* sign(m) = (m * 15) + 250 *)
      (* TODO: Support functions in polynomial. *)
      if List.length params <> 3 then fail1 (arg_err ops) opl else
      pure ([si "a"; si "m"; si "b"], ressize ops params, add_pn (mul_pn (sp "m") (const_pn 15)) (const_pn 250))
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
        let arg0 = List.nth tparams 0 in
        (* Check if this is ByStrX -> Uint256 when X <= 32. *)
        if (match bystrx_width arg0 with | Some w when w <= 32 -> true | _ -> false) && ops = "to_uint256"
          then pure (base * 4) else
        (match int_width (List.nth tparams 0) with
         | Some 32 | Some 64 -> pure base
         | Some 128 -> pure @@ base * 2
         | Some 256 -> pure @@ base * 4
         | _ -> fail1 (arg_err ops) opl
        ) in
      let sig_args =
        (match ops with | "add" | "sub" | "mul" | "div" | "rem" | "lt" -> [si "a"; si "b"] | _ -> [si "a"])
      in
      pure (sig_args, ressize ops params, const_pn c)
    | _ -> fail1 ("Unknown builtin " ^ ops) opl

  (* Return gas use and result sizeref polynomials of evaluating an expression. *)
  let rec gua_expr genv (erep : expr_annot) =
    let%bind c = Gas.expr_static_cost erep in
    let cc = const_pn c in
    let (e, rep) = erep in
    match e with
    | Literal l ->
      let%bind ss = Gas.literal_cost l in
      pure @@ ([], Base(Const(ss)), cc)
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
        | "True" | "False" -> pure @@ Base(Const(1))
        | "Nil" -> pure @@ Base(Const(1))
        | "None" -> pure @@ Base(Const(1))
        | "Some" ->
          (* TypeChecker will ensure that actuals has unit length. *)
          let arg = List.nth actuals 0 in
          let%bind (_, compsize, _) = GUAEnv.resolvS genv (get_id arg) ~lopt:(Some(get_rep arg)) in
          pure @@ compsize
        | "Pair" | "Cons" ->
          (* TypeChecker will ensure that actuals has two elements. *)
          let arg0 = List.nth actuals 0 in
          let arg1 = List.nth actuals 1 in
          let%bind (_, compsize0, _) = GUAEnv.resolvS genv (get_id arg0) ~lopt:(Some(get_rep arg0)) in
          let%bind (_, compsize1, _) = GUAEnv.resolvS genv (get_id arg1) ~lopt:(Some(get_rep arg1)) in
          let compsize0' = single_simple_pn compsize0 in
          let compsize1' = single_simple_pn compsize1 in
          pure @@ SPol(add_pn compsize0' compsize1')
        | _ -> fail1 (Printf.sprintf "Unsupported constructor %s in gas analysis." cname)
                     (ER.get_loc rep)
        )
      in
        pure ([], ressize, cc)
    | MatchExpr (x, clauses) ->
      let%bind (_, xsize, _) = GUAEnv.resolvS genv (get_id x) ~lopt:(Some(get_rep x)) in
      (*   TODO: If the return type of the MatchExpr is a function then
       *     the arguments (first Element of the signature) cannot be
       *     ignored as we're doing now. 
       *     TODO: Normalize argument names and merge them. This can be done
       *     having a common set of actuals names for all branches.
       *     (Use substitute_actuals to replace with a common set of param names).
       *)
      if List.length clauses > 1 then
        let%bind (args, ressize, gup) = foldM ~f:(fun (_, asizes, apn) (pat, branch) ->
          let%bind genv' = bind_pattern genv xsize pat in
          let%bind (_, bsize, bpn) = gua_expr genv' branch in
          let rsize =
            (match bsize with
            | SPol sp -> add_pn sp asizes
            | Base (Const c) -> add_pn asizes (const_pn c)
            | _ -> add_pn asizes (single_simple_pn bsize)
            ) in
          pure ([], rsize, add_pn apn bpn)
        ) ~init:([], empty_pn, empty_pn) clauses in
        pure (args, SPol (ressize), add_pn gup cc)
      else
        let (pat, branch) = (List.nth clauses 0) in
        let%bind genv' = bind_pattern genv xsize pat in
        let%bind (args, bsize, bgu) = gua_expr genv' branch in
        pure (args, bsize, add_pn bgu cc)
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
        let%bind splist = foldM ~f:(fun acc (_, pl) ->
          (match pl with
          | MTag s -> 
            let sr = Base(Const(String.length s)) in
            let sr' = single_simple_pn sr in
            pure (add_pn acc sr')
          | MLit l ->
            let%bind lc = Gas.literal_cost l in
            pure (add_pn acc (const_pn lc))
          | MVar i ->
            let%bind (_, irs, _) = GUAEnv.resolvS genv (get_id i) ~lopt:(Some(get_rep i)) in
            pure (add_pn acc (single_simple_pn  irs))
          )
        ) ~init:empty_pn plist in
        pure ([], SPol(splist), cc)

  (* Hardcode signature for folds. *)
  let analyze_folds genv =
    (*  list_foldr: forall 'A . forall 'B . g:('A -> 'B -> 'B) -> b:'B -> a:(List 'A) -> 'B *)
    let a = ER.mk_id (mk_ident "a") (ADT("List", [TypeVar("'A")])) in
    let g = ER.mk_id (mk_ident "g") (FunType(TypeVar("'A"), FunType(TypeVar("'B"), TypeVar("'B")))) in
    let b = ER.mk_id (mk_ident "b") (TypeVar("'B")) in
    let lendep = SizeOf(Length(Base(Var(a)))) in
    let gapp = GApp(g, [Element(Base(Var(a))); Base(Var(b))]) in
    (* Gas use polynomial = Length(a) * GApp(g, [Element(a), b]) *)
    let gupol = mul_pn (single_simple_pn lendep) (single_simple_pn gapp) in
    let ressize = SApp(g, [Base(Var(a));Base(Var(b))]) in
    let list_foldr_signature = ([g;b;a], ressize, gupol) in
    let genv' = GUAEnv.addS genv "list_foldr" list_foldr_signature in
    (* list_foldl: forall 'A . forall 'B . g:('B -> 'A -> 'B) -> b:'B -> a:(List 'A) -> 'B *)
    let gapp' = GApp(g, [Base(Var(b)); Element(Base(Var(a)));]) in
    let gupol' = mul_pn (single_simple_pn lendep) (single_simple_pn gapp') in
    let ressize' = SApp(g, [Base(Var(b));Base(Var(a))]) in
    let list_foldl_signature = ([g;b;a], ressize', gupol') in
    let gapp'' = GUAEnv.addS genv' "list_foldl" list_foldl_signature in
    gapp''

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
          let%bind (_, rs, _) = GUAEnv.resolvS genv (get_id r) in
          let gupol' = add_pn gupol (single_simple_pn (SizeOf (rs))) in
          gua_stmt genv gupol' sts
        | Bind (x, e) ->
          let%bind (a, s, p) = gua_expr genv e in
          let genv' = GUAEnv.addS genv (get_id x) (a, s, p) in
          (* Simple constant const for binding. *)
          let gupol' = add_pn gupol (const_pn 1) in
          (* if a is empty, accumulate the cost of executing expr. *)
          let gupol'' = if a = [] then add_pn gupol' p else gupol' in
          gua_stmt genv' gupol'' sts
        | MapUpdate(_, klist, ropt) ->
          let nindices = (const_pn (List.length klist)) in
          let%bind c = (match ropt with
          | Some i ->
            let%bind (_, rs, _) = GUAEnv.resolvS genv (get_id i) in
            pure @@ single_simple_pn (SizeOf(rs)) (* update *)
          | None -> pure @@ empty_pn) (* delete *)in
          let gupol' = add_pn nindices c in
          let gupol'' = add_pn gupol gupol' in
          gua_stmt genv gupol'' sts
        | MapGet(x, m, klist, fetchval) ->
          let nindices = (const_pn (List.length klist)) in
          let (sign, pol) =
            if fetchval then 
              let ressize = List.fold_left (fun acc _ -> Element(acc)) (Base(Var(m))) klist in
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
          let%bind gupol' = foldM ~f:(fun apn (pat, branch) ->
            let%bind genv' = bind_pattern genv xsize pat in
            let%bind bpn = gua_stmt genv' empty_pn branch in
            pure (add_pn apn bpn)
          ) ~init:(add_pn gupol num_clauses) clauses in
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
      match le with
      | LibVar (lname, lexp) ->
        let%bind esig = gua_expr genv lexp in
        pure @@ GUAEnv.addS genv (get_id lname) esig
      | LibTyp _ -> pure genv
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
    Printf.printf "Gas usage signature:\n%s\n\n" (sprint_signature (params, sr, pol'));
    pure (params, sr, pol')

  end
