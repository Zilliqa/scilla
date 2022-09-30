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

open Core.Result.Let_syntax
open TypeUtil
open Literal
open Syntax
open ErrorUtils
open MonadUtil
open Polynomials.Polynomial

module ScillaGUA
    (SR : Rep) (ER : sig
      include Rep

      val get_type : rep -> PlainTypes.t inferred_type
      val mk_rep : loc -> PlainTypes.t inferred_type -> rep
    end) =
struct
  module SER = SR
  module EER = ER
  module GUALiteral = GlobalLiteral
  module GUAType = GUALiteral.LType
  module GUAIdentifier = GUAType.TIdentifier
  module GUAName = GUAIdentifier.Name
  module GUASyntax = ScillaSyntax (SR) (ER) (GUALiteral)
  module TU = TypeUtilities
  module Gas = Gas.ScillaGas (SR) (ER)
  open GUAIdentifier
  open GUASyntax

  let guaname_of_string = GUAName.parse_simple_name
  let mk_gua_id i = mk_id @@ guaname_of_string i

  let mk_typed_id i t =
    mk_id (guaname_of_string i)
      (ER.mk_rep dummy_loc (PlainTypes.mk_qualified_type t))

  let mk_typed_id_from_name i t =
    mk_id i (ER.mk_rep dummy_loc (PlainTypes.mk_qualified_type t))

  type sizeref =
    (* Refer to the size of a variable. *)
    | Base of ER.rep GUAIdentifier.t
    (* For Lengths of Lists and Maps. *)
    | Length of sizeref
    (* For Elements of Lists and Maps. *)
    | Element of sizeref
    (* For Lists and Maps. *)
    | Container of sizeref * sizeref
    (* A polynomial function of other sizerefs *)
    | SPol of sizeref polynomial
    (* An abstract maximum across branches. *)
    (* TODO: Remove this constructor, we're not using it anymore. *)
    | MaxB of sizeref list
    (* Arbitrary math function of sizerefs. *)
    | MFun of string * sizeref list
    (* Result of a builtin. *)
    | BApp of builtin * sizeref list
    (* The growth of accummulator (a recurrence) in list_foldr.
     * The semantics is similar to SApp, except that, the ressize of
     * applying "GUAIdentifier.t" is taken as a recurence and solved for the
     * length of the second sizeref (accumulator) actual. The first
     * sizeref actual is Element(list being folded). *)
    | RFoldAcc of ER.rep GUAIdentifier.t * sizeref * sizeref
    (* Same as RFoldAcc, but for list_foldl:
     * order of the two sizeref actuals are reversed. *)
    | LFoldAcc of ER.rep GUAIdentifier.t * sizeref * sizeref
    (* Lambda for unknown sizeref (from applying higher order functions). 
     * TODO: Use "sizeref" instead of "GUAIdentifier.t" to handle applying wrapped functions,
             where for example `x = fst arg` and we're applying `x`. *)
    | SApp of ER.rep GUAIdentifier.t * sizeref list
    (* When we cannot determine the size *)
    | Intractable of string

  (* A variable / quantity on which gas use depends. *)
  type guref =
    (* Gas use depends on size of a value *)
    | SizeOf of sizeref
    (* Applying a higher order argument (function) and its arguments. *)
    (* TODO: Use "sizeref" instead of "GUAIdentifier.t" to handle applying wrapped functions,
             where for example `x = fst arg` and we're applying `x`. *)
    | GApp of ER.rep GUAIdentifier.t * sizeref list
    (* Gas usage polynomial which is known and needs to be expanded. 
     * When a GApp resolves successfully, we replace it with GPol. *)
    | GPol of guref polynomial

  (* Gas use and result sizeref of functions (or executions)
   * The identifier list specifies the arguments which must be substituted.
   * Signatures that contain "GApp/SApp" will (recursively) be substituted for
   * the final signature to not have these lambdas. *)
  type signature = ER.rep GUAIdentifier.t list * sizeref * guref polynomial

  (* Given a size reference, print a description for it. *)
  let rec sprint_sizeref = function
    | Base v -> as_error_string v
    (* For Lengths of Lists and Maps. *)
    | Length sr' -> "Length of: " ^ sprint_sizeref sr'
    (* For Elements of Lists and Maps. *)
    | Element sr' -> "Element of: " ^ sprint_sizeref sr'
    (* For Lists and Maps. *)
    | Container (len, elm) ->
        "Container (" ^ sprint_sizeref len ^ ", " ^ sprint_sizeref elm ^ ")"
    | SPol pn -> sprint_pn pn ~f:(fun sr -> sprint_sizeref sr)
    | MaxB srlist ->
        "Max ("
        ^ List.fold_left
            (fun acc sr ->
              acc ^ (if acc = "" then "" else ",") ^ sprint_sizeref sr)
            "" srlist
        ^ ")"
    | MFun (s, srlist) ->
        s ^ " ("
        ^ List.fold_left
            (fun acc sr ->
              acc ^ (if acc = "" then "" else ",") ^ sprint_sizeref sr)
            "" srlist
        ^ ")"
    | BApp (b, srlist) ->
        "Result of builtin " ^ pp_builtin b ^ "("
        ^ List.fold_left
            (fun acc sr ->
              acc ^ (if acc = "" then "" else ",") ^ sprint_sizeref sr)
            "" srlist
        ^ ")"
    | SApp (id, srlist) ->
        "SApp " ^ as_error_string id ^ "( "
        ^ List.fold_left
            (fun acc sr ->
              acc ^ (if acc = "" then "" else ",") ^ sprint_sizeref sr)
            "" srlist
        ^ ")"
    | RFoldAcc (id, lel, acc) ->
        "RFoldAcc " ^ as_error_string id ^ " (" ^ sprint_sizeref lel ^ ", "
        ^ sprint_sizeref acc ^ ")"
    | LFoldAcc (id, lel, acc) ->
        "LFoldAcc " ^ as_error_string id ^ " (" ^ sprint_sizeref lel ^ ", "
        ^ sprint_sizeref acc ^ ")"
    | Intractable s -> "Cannot determine size: " ^ s

  (* Given a gas use reference, print a description for it. *)
  let rec sprint_guref = function
    | SizeOf s -> sprint_sizeref s
    | GApp (id, gurlist) ->
        "Cost of calling " ^ as_error_string id ^ "("
        ^ List.fold_left
            (fun acc gur ->
              acc ^ (if acc = "" then "" else ", ") ^ sprint_sizeref gur)
            "" gurlist
        ^ ")"
    | GPol pn -> "GPol(" ^ sprint_gup pn ^ ")"

  and sprint_gup pn =
    let sym_map : (guref, string) Hashtbl.t = Hashtbl.create 16 in
    let syms = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" in
    let next_sim_idx = ref 0 in
    let pols =
      sprint_pn pn ~f:(fun gur ->
          match Hashtbl.find_opt sym_map gur with
          | Some c -> c
          | None ->
              let i = !next_sim_idx in
              let _ = next_sim_idx := !next_sim_idx + 1 in
              let c = String.sub syms (i mod 52) 1 in
              let _ = Hashtbl.add sym_map gur c in
              c)
    in
    let legs =
      if Hashtbl.length sym_map = 0 then ""
      else
        "\nLegend:\n"
        ^ Hashtbl.fold
            (fun k v acc -> acc ^ "\n" ^ v ^ ": " ^ sprint_guref k)
            sym_map ""
    in
    pols ^ legs

  let sprint_signature (params, sr, pn) =
    let args =
      if params = [] then ""
      else
        "Parameter list: "
        ^ List.fold_left
            (fun acc p -> acc ^ as_error_string p ^ " ")
            "( " params
        ^ ")\n"
    in
    args ^ "Gas use polynomial:\n" ^ sprint_gup pn ^ "\nResult size: "
    ^ sprint_sizeref sr

  module GUAEnv = struct
    open AssocDictionary

    (* A map from identifier strings to their signatures. *)
    type t = signature dict

    (* Make an empty environment. *)
    let mk = make_dict

    (* In env, add mapping id => s *)
    let addS env id s = insert id s env

    (* In env, resolve id => s and return s (fails if cannot resolve). *)
    let resolvS ?(lopt = None) env id =
      match lookup id env with
      | Some s -> pure s
      | None ->
          let sloc =
            match lopt with Some l -> ER.get_loc l | None -> dummy_loc
          in
          fail1 ~kind:"Couldn't resolve the identifier in gas use analysis"
            ~inst:id sloc

    (* retain only those entries "k" for which "f k" is true. *)
    let filterS env ~f = filter ~f env

    (* is "id" in the environment. *)
    let existsS env id =
      match lookup id env with Some _ -> true | None -> false

    (* add entries from env' into env. *)
    let appendS env env' =
      let kv = to_list env' in
      List.fold_left (fun acc (k, v) -> addS acc k v) env kv

    let pp env =
      let l = to_list env in
      List.fold_left
        (fun acc (k, sign) ->
          acc ^ "Signature for " ^ k ^ ": " ^ sprint_signature sign ^ "----\n")
        "" l
  end

  (* Expand polynomials that contain GPol. *)
  let rec expand_sizeref_pol pol =
    expand_parameters_pn pol ~f:(function
      | SPol p -> Some (expand_sizeref_pol p)
      | _ -> None)

  let sizeref_to_pol sr =
    match sr with SPol p -> expand_sizeref_pol p | _ -> single_simple_pn sr

  let rec sizeref_to_guref sr =
    match sr with
    | Base _ | Length _ | Element _ | Container _ | MaxB _ | MFun _ | BApp _
    | RFoldAcc _ | LFoldAcc _ | SApp _ | Intractable _ ->
        SizeOf sr
    | SPol p ->
        let term_replacer t =
          let coef, vars = t in
          let vars' =
            List.map
              (fun (v, p) ->
                let v' = sizeref_to_guref v in
                (v', p))
              vars
          in
          (coef, vars')
        in
        let gpol = List.map (fun t -> term_replacer t) p in
        GPol gpol

  (* Expand polynomials that contain GPol. *)
  let rec expand_guref_pol pol =
    expand_parameters_pn pol ~f:(function
      | SizeOf sr -> Some (single_simple_pn (sizeref_to_guref sr))
      | GApp _ -> None (* TODO *)
      | GPol p -> Some (expand_guref_pol p))

  (* Considering sr as a recurrence polynomial with 
   * accarg as the accumulator, growing Length(ls) times,
   * return the final sizeref. *)
  (* TODO: Make this robust and all correct. *)
  let solve_sizeref_rec sr accarg ls =
    (* If sr doesn't depend on accarg, then there's no growth at all. *)
    let rec deps_on_accarg = function
      | Base a -> get_id accarg = get_id a
      | Length sr | Element sr -> deps_on_accarg sr
      | Container (sr1, sr2) | RFoldAcc (_, sr1, sr2) | LFoldAcc (_, sr1, sr2)
        ->
          deps_on_accarg sr1 || deps_on_accarg sr2
      | SPol pol ->
          let in_pol p =
            List.exists
              (fun (_, vplist) ->
                List.exists (fun (v, _) -> deps_on_accarg v) vplist)
              p
          in
          in_pol pol
      | MaxB srl | MFun (_, srl) | BApp (_, srl) | SApp (_, srl) ->
          List.exists (fun sr -> deps_on_accarg sr) srl
      | Intractable _ -> false
    in
    if not (deps_on_accarg sr) then sr
    else
      match sr with
      | SPol p -> (
          (* Find a "analyzable" Container term in the polynomial. *)
          let cterms, oterms =
            Core.List.partition_tf p ~f:(fun (coef, vplist) ->
                match vplist with
                (* We can only analyze "Length(accarg) + C" *)
                | [
                 ( Container
                     (SPol [ cpol; (1, [ (Length (Base lenvar), 1) ]) ], _),
                   _ );
                ]
                  when is_const_term cpol
                       && get_id lenvar = get_id accarg
                       && coef = 1 ->
                    true
                | _ -> false)
          in
          match cterms with
          | [] -> sr (* No Container terms, and hence doesn't grow. *)
          | [
              ( _,
                [
                  (Container (SPol [ cpol; (1, [ (Length _, 1) ]) ], elmsize), _);
                ] );
            ]
          | [
              ( _,
                [
                  (Container (SPol [ (1, [ (Length _, 1) ]); cpol ], elmsize), _);
                ] );
            ] ->
              (* We can analyze only when there's just one Container term in the polynomial. *)
              (* The result size is "Length(accarg) + Length(ls) * C" *)
              let pol = mul_pn [ cpol ] (single_simple_pn @@ Length ls) in
              let cterm' = (1, [ (Container (SPol pol, elmsize), 1) ]) in
              SPol (cterm' :: oterms)
          | _ -> Intractable "Unable to solve recurrence.")
      | Container (SPol [ cpol; (1, [ (Length (Base lenvar), 1) ]) ], elmsize)
      | Container (SPol [ (1, [ (Length (Base lenvar), 1) ]); cpol ], elmsize)
        ->
          if is_const_term cpol && get_id lenvar = get_id accarg then
            let pol = mul_pn [ cpol ] (single_simple_pn @@ Length ls) in
            Container (SPol pol, elmsize)
          else Intractable "Unable to solve recurrence."
      | _ -> Intractable "Unable to solve recurrence."

  (* Combine polynomials from two match branches. *)
  let match_combine_pn p1 p2 =
    let cf t1 t2 =
      match (t1, t2) with
      | ( (coef1, [ (Container (SPol l1, elm1), pow1) ]),
          (coef2, [ (Container (SPol l2, elm2), pow2) ]) )
        when elm1 = elm2 && pow1 = pow2 ->
          (* If we have two identical containers, pick the max possible container size. *)
          (* TODO: For nested containers, should we have a recursive call? *)
          Some
            ( max coef1 coef2,
              [ (Container (SPol (max_combine_pn l1 l2), elm1), pow1) ] )
      | (coef1, _), (coef2, _) when eq_term ~coef:false t1 t2 ->
          (* We have two identical terms with only different co-efficients. *)
          Some (if coef1 > coef2 then t1 else t2)
      | _ -> None
    in
    combine_pn ~cf p1 p2

  (* Given a signature, substitute actual arguments into the formal parameters.
   * This function does not resolve the actuals into their sizerefs (only name substitution).
   * TODO: Currently unused. This may be useful in evaluating MatchExpr where each
   *       branch returns a function and we want to normalize all the formal args
   *       with a common name and merge. See comment in MatchExpr.
   *)
  let substitute_actuals f (params, ressize, gup) actuals =
    if List.length params != List.length actuals then
      fail1 ~kind:"Number of actual arguments and formal parameters mismatch"
        ?inst:None
        (ER.get_loc (get_rep f))
    else
      (* replace param with actual in sizeref. *)
      let sizeref_replacer param_actual_map s' =
        (* If id is to be replaced, give the replacement, else just return back id. *)
        let lookup_actual id =
          match List.assoc_opt (get_id id) param_actual_map with
          | Some act -> act
          | None -> id
        in
        let rec replacer s =
          match s with
          | Base b -> Base (lookup_actual b)
          | Length s' -> Length (replacer s')
          | Element s' -> Element (replacer s')
          | Container (len, elm) -> Container (replacer len, replacer elm)
          | SPol sp -> SPol (polynomial_replacer sp)
          | BApp (b, srlist) ->
              let srlist' = List.map (fun v -> replacer v) srlist in
              BApp (b, srlist')
          | MaxB srlist ->
              let srlist' = List.map (fun v -> replacer v) srlist in
              MaxB srlist'
          | MFun (s, srlist) ->
              let srlist' = List.map (fun v -> replacer v) srlist in
              MFun (s, srlist')
          | SApp (id, srlist) ->
              let id' = lookup_actual id in
              let srlist' = List.map (fun v -> replacer v) srlist in
              SApp (id', srlist')
          | RFoldAcc (id, ls, accbase) ->
              let id' = lookup_actual id in
              RFoldAcc (id', replacer ls, replacer accbase)
          | LFoldAcc (id, accbase, ls) ->
              let id' = lookup_actual id in
              LFoldAcc (id', replacer accbase, replacer ls)
          | Intractable s -> Intractable s
        and polynomial_replacer pol =
          var_replace_pn pol ~f:(fun sr -> replacer sr)
        in
        replacer s'
      in

      (* replace param with actual in guref. *)
      let rec guref_replacer param_actual_map v =
        match v with
        | SizeOf s ->
            let r = sizeref_replacer param_actual_map s in
            SizeOf r
        | GApp (id, gurlist) ->
            let id' =
              match List.assoc_opt (get_id id) param_actual_map with
              | Some id'' -> id''
              | None -> id
            in
            let gurlist' =
              List.map (fun v -> sizeref_replacer param_actual_map v) gurlist
            in
            GApp (id', gurlist')
        | GPol p -> GPol (polynomial_replacer param_actual_map p)
      and (* replace param with actual in a signature. *)
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
    (* If id has an actual that is a Base(_), return it, else fail. *)
    let lookup_base_replacement id =
      match List.assoc_opt (get_id id) param_actual_map with
      | Some act -> (
          (* We know what to do only when "actual" is Base(_). *)
          match act with
          | Base id'' -> pure id''
          (* See TODO in definition of sizeref where SApp should have sizeref instead of ident. *)
          | _ ->
              fail1 ~kind:"Functions cannot be wrapped in ADTs" ?inst:None
                (ER.get_loc (get_rep id)))
      | None -> pure id
    in
    let rec replacer s =
      match s with
      | Base b -> (
          match List.assoc_opt (get_id b) param_actual_map with
          | Some act -> pure @@ act
          | None -> pure @@ Base b)
      | Length s' ->
          let%bind r = replacer s' in
          pure @@ Length r
      | Element s' ->
          let%bind r = replacer s' in
          pure @@ Element r
      | Container (len, elm) ->
          let%bind len' = replacer len in
          let%bind elm' = replacer elm in
          pure @@ Container (len', elm')
      | SPol sp ->
          let%bind p' = polynomial_replacer sp in
          pure @@ SPol (expand_sizeref_pol p')
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
          let%bind id' = lookup_base_replacement id in
          let%bind srlist' = mapM ~f:(fun v -> replacer v) srlist in
          pure @@ SApp (id', srlist')
      | RFoldAcc (id, ls, acc) ->
          let%bind id' = lookup_base_replacement id in
          let%bind ls' = replacer ls in
          let%bind accbase' = replacer acc in
          pure @@ RFoldAcc (id', ls', accbase')
      | LFoldAcc (id, accbase, ls) ->
          let%bind id' = lookup_base_replacement id in
          let%bind ls' = replacer ls in
          let%bind accbase' = replacer accbase in
          pure @@ LFoldAcc (id', accbase', ls')
      | Intractable s -> pure @@ Intractable s
    and
        (* replace param with actual in a sizeref polynomial. *)
        polynomial_replacer pol =
      let exception Resolv_error of scilla_error list in
      let pol' =
        (* This circus with exceptions is to avoid having
            * Polynomial utilities deal with our "result". *)
        try
          pure
          @@ var_replace_pn pol ~f:(fun sr ->
                 match replacer sr with
                 | Ok sr' -> sr'
                 | Error s -> raise (Resolv_error s))
        with
        | Resolv_error s -> fail s
        | Polynomial_error s -> fail0 ~kind:s ?inst:None
      in
      pol'
    in
    replacer s'

  (* replace param with actual in guref. *)
  let rec substitute_resolved_actual_guref param_actual_map v =
    match v with
    | SizeOf s ->
        let%bind r = substitute_resolved_actual_sizeref param_actual_map s in
        pure @@ SizeOf r
    | GApp (id, gurlist) ->
        let%bind id' =
          match List.assoc_opt (get_id id) param_actual_map with
          | Some act -> (
              (* We know what to do only when "actual" is Base(_). *)
              match act with
              | Base id'' -> pure id''
              (* See TODO in definition of sizeref where SApp should have guref instead of ident. *)
              | _ ->
                  fail1 ~kind:"Functions cannot be wrapped in ADTs" ?inst:None
                    (ER.get_loc (get_rep id)))
          | None -> pure id
        in
        (* We don't do anything with id as that will be expanded (not just resolved). *)
        let%bind gurlist' =
          mapM
            ~f:(fun v -> substitute_resolved_actual_sizeref param_actual_map v)
            gurlist
        in
        pure @@ GApp (id', gurlist')
    | GPol p ->
        let%bind p' = polynomial_replacer param_actual_map p in
        pure @@ GPol p'

  and (* replace param with actual in a gu polynomial. *)
      polynomial_replacer param_actual_map pol =
    let exception Resolv_error of scilla_error list in
    let pol' =
      (* This circus with exceptions is to avoid having
          * Polynomial utilities deal with our "result". *)
      try
        pure
        @@ var_replace_pn pol ~f:(fun gur ->
               match substitute_resolved_actual_guref param_actual_map gur with
               | Ok gur' -> gur'
               | Error s -> raise (Resolv_error s))
      with
      | Resolv_error s -> fail s
      | Polynomial_error s -> fail0 ~kind:s ?inst:None
    in
    pol'

  let substitute_resolved_actuals_sizeref_list ressize params actuals =
    if List.length params != List.length actuals then
      fail0
        ~kind:
          "Number of actual arguments and formal parameters mismatch in \
           sizeref substitution."
        ?inst:None
    else
      substitute_resolved_actual_sizeref (List.combine params actuals) ressize

  let substitute_resolved_actuals_guref_list gup params actuals =
    if List.length params != List.length actuals then
      fail0
        ~kind:
          "Number of actual arguments and formal parameters mismatch in guref \
           substitution."
        ?inst:None
    else polynomial_replacer (List.combine params actuals) gup

  (* Resolve and expand variables and lambdas (whenever possible) in sr and pol. *)
  let resolve_expand genv sr pol =
    (* replace param with actual in sizeref. *)
    let sizeref_resolver s' =
      let rec resolver s =
        match s with
        | Base v ->
            let%bind _, resr, _ =
              GUAEnv.resolvS genv (as_string v) ~lopt:(Some (get_rep v))
            in
            pure resr
        | Length s' ->
            let%bind r = resolver s' in
            pure @@ Length r
        | Element s' ->
            let%bind r = resolver s' in
            pure @@ Element r
        | Container (len, elm) ->
            let%bind len' = resolver len in
            let%bind elm' = resolver elm in
            pure @@ Container (len', elm')
        | SPol sp ->
            let%bind p' = polynomial_resolver sp in
            pure @@ SPol (expand_sizeref_pol p')
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
            let%bind args, sr, _ =
              GUAEnv.resolvS genv (as_string id) ~lopt:(Some (get_rep id))
            in
            if args = [] then
              (* No known expansion  *)
              let%bind srlist' = mapM ~f:(fun v -> resolver v) srlist in
              pure @@ SApp (id, srlist')
            else
              let args' = List.map (fun i -> get_id i) args in
              let%bind sr' =
                substitute_resolved_actuals_sizeref_list sr args' srlist
              in
              resolver sr'
        | RFoldAcc (id, ls, accbase) | LFoldAcc (id, accbase, ls) ->
            let rfold = match s with RFoldAcc _ -> true | _ -> false in
            let%bind args, sr, _ =
              GUAEnv.resolvS genv (as_string id) ~lopt:(Some (get_rep id))
            in
            if args = [] then
              (* No known expansion  *)
              let%bind ls' = resolver ls in
              let%bind acc' = resolver accbase in
              pure
                (if rfold then RFoldAcc (id, ls', acc')
                else LFoldAcc (id, acc', ls'))
            else
              let args' = List.map (fun i -> get_id i) args in
              let srlist =
                if rfold then [ Element ls; accbase ]
                else [ accbase; Element ls ]
              in
              (* Solve for "Length()" applications of the fold iterator. *)
              let%bind sr' =
                if List.length args' <> 2 then
                  fail0 ~kind:"Incorrect number of arguments to fold iterator"
                    ?inst:None
                else
                  (* We want the accumulator argument to the fold iterator. *)
                  let accarg =
                    if rfold then List.nth args 1 else List.nth args 0
                  in
                  pure @@ solve_sizeref_rec sr accarg ls
              in
              let%bind sr'' =
                substitute_resolved_actuals_sizeref_list sr' args' srlist
              in
              resolver sr''
        | Intractable s -> pure @@ Intractable s
      (* replace param with actual in a signature. *)
      and polynomial_resolver pol =
        let exception Resolv_error of scilla_error list in
        let pol' =
          (* This circus with exceptions is to avoid having
                * Polynomial utilities deal with our "result". *)
          try
            pure
            @@ var_replace_pn pol ~f:(fun sr ->
                   match resolver sr with
                   | Ok sr' -> sr'
                   | Error s -> raise (Resolv_error s))
          with
          | Resolv_error s -> fail s
          | Polynomial_error s -> fail0 ~kind:s ?inst:None
        in
        pol'
      in
      resolver s'
    in
    (* replace param with actual in guref. *)
    let rec guref_resolver v =
      match v with
      | SizeOf s ->
          let%bind r = sizeref_resolver s in
          pure @@ SizeOf r
      | GApp (id, gurlist) ->
          let%bind args, _, gup =
            GUAEnv.resolvS genv (as_string id) ~lopt:(Some (get_rep id))
          in
          if args = [] then
            (* No known expansion  *)
            let%bind gurlist' = mapM ~f:(fun v -> sizeref_resolver v) gurlist in
            pure @@ GApp (id, gurlist')
          else
            let args' = List.map (fun i -> get_id i) args in
            let%bind sr' =
              substitute_resolved_actuals_guref_list gup args' gurlist
            in
            let%bind p = polynomial_resolver sr' in
            pure @@ GPol p
      | GPol p ->
          let%bind p' = polynomial_resolver p in
          pure @@ GPol p'
    and (* replace param with actual in a signature. *)
        polynomial_resolver pol =
      let exception Resolv_error of scilla_error list in
      let pol' =
        (* This circus with exceptions is to avoid having
              * Polynomial utilities deal with our "result". *)
        try
          pure
          @@ var_replace_pn pol ~f:(fun gur ->
                 match guref_resolver gur with
                 | Ok gur' -> gur'
                 | Error s -> raise (Resolv_error s))
        with
        | Resolv_error s -> fail s
        | Polynomial_error s -> fail0 ~kind:s ?inst:None
      in
      pol'
    in
    let%bind sr' = sizeref_resolver sr in
    let%bind pol' = polynomial_resolver pol in
    pure (sr', pol')

  (* For a pattern "pat" match on "msref", add binders to genv. *)
  let rec bind_pattern genv msref pat =
    let open Datatypes in
    match pat with
    | Wildcard -> pure genv
    | Binder i -> pure @@ GUAEnv.addS genv (as_string i) ([], msref, empty_pn)
    | Constructor (cname, _)
      when is_true_ctr_name (get_id cname)
           || is_false_ctr_name (get_id cname)
           || is_nil_ctr_name (get_id cname)
           || is_none_ctr_name (get_id cname) ->
        pure @@ genv
    | Constructor (cname, plist) when is_some_ctr_name (get_id cname) ->
        (* TypeChecker will ensure that plist has unit length. *)
        let arg = List.nth plist 0 in
        bind_pattern genv msref arg
    | Constructor (cname, plist) when is_cons_ctr_name (get_id cname) ->
        (* TypeChecker will ensure that plist has two elements. *)
        let arg0 = List.nth plist 0 in
        let arg1 = List.nth plist 1 in
        let%bind genv' = bind_pattern genv (Element msref) arg0 in
        let%bind genv'' = bind_pattern genv' msref arg1 in
        pure genv''
    | Constructor (cname, plist) when is_pair_ctr_name (get_id cname) ->
        (* TypeChecker will ensure that plist has two elements. *)
        let arg0 = List.nth plist 0 in
        let arg1 = List.nth plist 1 in
        (* Bind both to the original element. *)
        let%bind genv' = bind_pattern genv msref arg0 in
        let%bind genv'' = bind_pattern genv' msref arg1 in
        pure genv''
    | Constructor (cname, _) ->
        fail0 ~kind:"Unsupported constructor in gas analysis"
          ~inst:(GUAIdentifier.as_error_string cname)

  (* built-in op costs are propotional to size of data they operate on. *)
  (* TODO: Have all numbers in one place. Integrate with Gas.ml *)
  let builtin_cost (ops, opl') params =
    let opl = ER.get_loc opl' in

    let open GUAType in
    (* Types of our paramters. *)
    let tparams = List.map (fun p -> (ER.get_type (get_rep p)).tp) params in

    let tvar a = TypeVar a in
    (* Make a simple identifier of type 'A *)
    let si a = mk_typed_id a (tvar "'A") in
    (* Make a simple polynomial from string a *)
    let sp a = single_simple_pn (SizeOf (Base (si a))) in
    let arg_err s = "Incorrect arguments to builtin " ^ pp_builtin s in
    let ressize op actuals =
      (* These must be resolved in the caller when expanding the lambdas. *)
      let srparams = List.map (fun i -> Base i) actuals in
      BApp (op, srparams)
    in

    match ops with
    | Builtin_eq ->
        if List.length params <> 2 then fail1 ~kind:(arg_err ops) ?inst:None opl
        else
          (* TODO: Use max(a, b)? This one is ok for now for a worst case report. *)
          let%bind pn =
            let t = List.nth tparams 0 in
            match t with
            | PrimType (Int_typ width) | PrimType (Uint_typ width) -> (
                match width with
                | Bits32 | Bits64 -> pure @@ const_pn 4
                | Bits128 -> pure @@ const_pn 8
                | Bits256 -> pure @@ const_pn 16)
            (*  eq (a, b) = a. *)
            | PrimType String_typ -> pure @@ sp "a"
            | PrimType Bnum_typ -> pure @@ const_pn 32
            (*  eq (a, b) = a. *)
            | PrimType Bystr_typ -> pure @@ sp "a"
            | PrimType (Bystrx_typ w) -> pure @@ const_pn w
            | _ -> fail1 ~kind:(arg_err ops) ?inst:None opl
          in
          pure ([ si "a"; si "b" ], ressize ops params, pn)
    | Builtin_concat ->
        (* concat(a, b) = a + b *)
        if List.length params <> 2 then fail1 ~kind:(arg_err ops) ?inst:None opl
        else
          pure ([ si "a"; si "b" ], ressize ops params, add_pn (sp "a") (sp "b"))
    | Builtin_substr ->
        (* substr(a, o, l) = a *)
        if List.length params <> 3 then fail1 ~kind:(arg_err ops) ?inst:None opl
        else pure ([ si "a"; si "o"; si "l" ], ressize ops params, sp "a")
    | Builtin_blt | Builtin_badd ->
        (* blt/badd(a, b) = 32 *)
        if List.length params <> 2 then fail1 ~kind:(arg_err ops) ?inst:None opl
        else pure ([ si "a"; si "b" ], ressize ops params, const_pn 32)
    | Builtin_to_bystr ->
        (* to_bystr(a) = a *)
        if List.length params <> 1 then fail1 ~kind:(arg_err ops) opl
        else pure ([ si "a" ], ressize ops params, sp "a")
    | Builtin_bech32_to_bystr20 | Builtin_bystr20_to_bech32 ->
        if List.length params <> 2 then fail1 ~kind:(arg_err ops) ?inst:None opl
        else
          pure
            ( [ si "prefix"; si "addr" ],
              ressize ops params,
              add_pn (sp "prefix") (sp "addr") )
    | Builtin_sha256hash | Builtin_ripemd160hash | Builtin_keccak256hash
    | Builtin_schnorr_get_address ->
        (* hash(a) = a * 15. TODO: Support functions in polynomial. *)
        if List.length params <> 1 then fail1 ~kind:(arg_err ops) ?inst:None opl
        else pure ([ si "a" ], ressize ops params, mul_pn (sp "a") (const_pn 15))
    | Builtin_schnorr_verify ->
        (* sign(m) = (m * 15) + 250 *)
        (* TODO: Support functions in polynomial. *)
        if List.length params <> 3 then fail1 ~kind:(arg_err ops) ?inst:None opl
        else
          pure
            ( [ si "a"; si "m"; si "b" ],
              ressize ops params,
              add_pn (mul_pn (sp "m") (const_pn 15)) (const_pn 250) )
    | Builtin_contains | Builtin_get ->
        (* contains/get(m, key) = 1 *)
        if List.length params <> 2 then fail1 ~kind:(arg_err ops) ?inst:None opl
        else pure ([ si "m"; si "key" ], ressize ops params, const_pn 1)
    | Builtin_put ->
        (* put(m, key, value) = 1 + Length (m) *)
        if List.length params <> 3 then fail1 ~kind:(arg_err ops) ?inst:None opl
        else
          let pol = single_simple_pn (SizeOf (Length (Base (si "m")))) in
          pure
            ( [ si "m"; si "key"; si "val" ],
              ressize ops params,
              add_pn pol (const_pn 1) )
    | Builtin_remove ->
        (* remove(m, key) = 1 + Length (m) *)
        if List.length params <> 2 then fail1 ~kind:(arg_err ops) ?inst:None opl
        else
          let pol = single_simple_pn (SizeOf (Length (Base (si "m")))) in
          pure
            ([ si "m"; si "key" ], ressize ops params, add_pn pol (const_pn 1))
    | Builtin_to_list | Builtin_size ->
        (* 1 + length (m) *)
        if List.length params <> 1 then fail1 ~kind:(arg_err ops) ?inst:None opl
        else
          let pol = single_simple_pn (SizeOf (Length (Base (si "m")))) in
          pure ([ si "m" ], ressize ops params, add_pn pol (const_pn 1))
    | Builtin_add | Builtin_sub | Builtin_mul | Builtin_div | Builtin_rem
    | Builtin_lt | Builtin_to_int32 | Builtin_to_int64 | Builtin_to_int128
    | Builtin_to_int256 | Builtin_to_uint32 | Builtin_to_uint64
    | Builtin_to_uint128 | Builtin_to_uint256 ->
        if List.length params <> 2 && List.length params <> 1 then
          fail1 ~kind:(arg_err ops) ?inst:None opl
        else
          let base =
            match ops with
            | Builtin_mul | Builtin_div | Builtin_rem -> 20
            | _ -> 4
          in
          let%bind c =
            let arg0 = List.nth tparams 0 in
            (* Check if this is ByStrX -> Uint256 when X <= 32. *)
            if
              (match bystrx_width arg0 with
              | Some w when w <= 32 -> true
              | _ -> false)
              && ops = Builtin_to_uint256
            then pure (base * 4)
            else
              match int_width (List.nth tparams 0) with
              | Some 32 | Some 64 -> pure base
              | Some 128 -> pure @@ (base * 2)
              | Some 256 -> pure @@ (base * 4)
              | _ -> fail1 ~kind:(arg_err ops) ?inst:None opl
          in
          let sig_args =
            match ops with
            | Builtin_add | Builtin_sub | Builtin_mul | Builtin_div
            | Builtin_rem | Builtin_lt ->
                [ si "a"; si "b" ]
            | _ -> [ si "a" ]
          in
          pure (sig_args, ressize ops params, const_pn c)
    | _ -> fail1 ~kind:"Unknown builtin" ~inst:(pp_builtin ops) opl

  (* Return gas use and result sizeref polynomials of evaluating an expression. *)
  let rec gua_expr genv (erep : expr_annot) =
    let c = 1 in
    (* A static cost *)
    let cc = const_pn c in
    let e, rep = erep in
    match e with
    | Literal l ->
        let%bind ss = Gas.literal_cost l in
        pure @@ ([], SPol (const_pn ss), cc)
    | Var i ->
        let%bind args, ressize, gup =
          GUAEnv.resolvS genv (as_string i) ~lopt:(Some (get_rep i))
        in
        pure (args, ressize, add_pn gup cc)
    | Fun (arg, _, body) ->
        (* Add a sizeref for arg into env for the body to reference to. *)
        let b = Base arg in
        let (p : signature) = ([], b, empty_pn) in
        let genv' = GUAEnv.addS genv (as_string arg) p in
        let%bind sargs, rsize, rgas = gua_expr genv' body in
        (* We have signature for the body, just add arg as a parameter to that function signature. *)
        pure (arg :: sargs, rsize, add_pn rgas cc)
    | App (f, actuals) ->
        (* Build a lambda for "f". It will be expanded next (along with inner lambdas if possible). *)
        let srparams = List.map (fun i -> Base i) actuals in
        let u = SApp (f, srparams) in
        let v = single_simple_pn (GApp (f, srparams)) in
        (* Expand all lambdas that we can. *)
        let%bind ressize', gup' = resolve_expand genv u v in
        (* TODO: Return value having no arguments implies partial application not supported. *)
        pure ([], ressize', add_pn gup' cc)
    | Builtin ((b, rep), _, actuals) ->
        (* Handle builtin-s like we handle function application. *)
        let%bind bsig = builtin_cost (b, rep) actuals in
        let genv' = GUAEnv.addS genv (pp_builtin b) bsig in
        (* We have the function signature ready, apply and expand it. *)
        (* Build a lambda for "f". It will be expanded next (along with inner lambdas if possible). *)
        let srparams = List.map (fun i -> Base i) actuals in
        let u = SApp (mk_gua_id (pp_builtin b) rep, srparams) in
        let v =
          single_simple_pn (GApp (mk_gua_id (pp_builtin b) rep, srparams))
        in
        (* Expand all lambdas that we can. *)
        let%bind ressize', gup' = resolve_expand genv' u v in
        (* TODO: Return value having no arguments implies partial application not supported. *)
        pure ([], ressize', add_pn gup' cc)
    | Let (i, _, lhs, rhs) ->
        let%bind lhs_sig = gua_expr genv lhs in
        let genv' = GUAEnv.addS genv (as_string i) lhs_sig in
        let%bind rhs_sig = gua_expr genv' rhs in
        (* gas consumption is sum of both. *)
        let args, ressize, gup = rhs_sig in
        let lhs_args, _, gup' = lhs_sig in
        (* Gas cost for this let expression is sum of LHS cost (if not a function) and RHS cost. *)
        let p = if lhs_args <> [] then gup else add_pn gup gup' in
        pure (args, ressize, add_pn p cc)
    | Constr (cname, _, actuals) ->
        let open Datatypes in
        let cname_as_name = get_id cname in
        let%bind ressize =
          if is_true_ctr_name cname_as_name || is_false_ctr_name cname_as_name
          then pure @@ SPol (const_pn 1)
          else if is_nil_ctr_name cname_as_name then
            pure @@ Container (SPol (const_pn 0), SPol (const_pn 1))
          else if is_none_ctr_name cname_as_name then pure @@ SPol (const_pn 1)
          else if is_some_ctr_name cname_as_name then
            (* TypeChecker will ensure that actuals has unit length. *)
            let arg = List.nth actuals 0 in
            let%bind _, compsize, _ =
              GUAEnv.resolvS genv (as_string arg) ~lopt:(Some (get_rep arg))
            in
            pure @@ compsize
          else if is_pair_ctr_name cname_as_name then
            (* TypeChecker will ensure that actuals has two elements. *)
            let arg0 = List.nth actuals 0 in
            let arg1 = List.nth actuals 1 in
            let%bind _, compsize0, _ =
              GUAEnv.resolvS genv (as_string arg0) ~lopt:(Some (get_rep arg0))
            in
            let%bind _, compsize1, _ =
              GUAEnv.resolvS genv (as_string arg1) ~lopt:(Some (get_rep arg1))
            in
            let compsize0' = sizeref_to_pol compsize0 in
            let compsize1' = sizeref_to_pol compsize1 in
            pure @@ SPol (add_pn compsize0' compsize1')
          else if is_cons_ctr_name cname_as_name then
            (* TypeChecker will ensure that actuals has two elements. *)
            let arg0 = List.nth actuals 0 in
            let arg1 = List.nth actuals 1 in
            let%bind _, compsize0, _ =
              GUAEnv.resolvS genv (as_string arg0) ~lopt:(Some (get_rep arg0))
            in
            let%bind _, compsize1, _ =
              GUAEnv.resolvS genv (as_string arg1) ~lopt:(Some (get_rep arg1))
            in
            match compsize1 with
            | Base _ ->
                (* Cons a b : Container((Length(b)+1), Element(b)) *)
                let el = Element compsize1 in
                let len =
                  SPol
                    (add_pn (single_simple_pn @@ Length compsize1) (const_pn 1))
                in
                pure @@ Container (len, el)
            | Container (SPol len, elm) ->
                (* Just add 1 to the length. *)
                pure @@ Container (SPol (add_pn len (const_pn 1)), elm)
            | _ ->
                (* what to do? *)
                pure
                @@ SPol
                     (add_pn (sizeref_to_pol compsize0)
                        (sizeref_to_pol compsize1))
          else
            fail1 ~kind:"Unsupported constructor in gas analysis"
              ~inst:(as_error_string cname) (ER.get_loc rep)
        in
        pure ([], ressize, cc)
    | MatchExpr (x, clauses) ->
        let%bind _, xsize, _ =
          GUAEnv.resolvS genv (as_string x) ~lopt:(Some (get_rep x))
        in
        (*   TODO: If the return type of the MatchExpr is a function then
         *     the arguments (first Element of the signature) cannot be
         *     ignored as we're doing now. 
         *     TODO: Normalize argument names and merge them. This can be done
         *     having a common set of actuals names for all branches.
         *     (Use substitute_actuals to replace with a common set of param names).
         *)
        if List.length clauses > 1 then
          let%bind args, ressize, gup =
            foldM
              ~f:(fun (_, asizes, apn) (pat, branch) ->
                let%bind genv' = bind_pattern genv xsize pat in
                let%bind _, bsize, bpn = gua_expr genv' branch in
                let rsize = match_combine_pn asizes (sizeref_to_pol bsize) in
                pure ([], rsize, add_pn apn bpn))
              ~init:([], empty_pn, empty_pn) clauses
          in
          pure (args, SPol ressize, add_pn gup cc)
        else
          let pat, branch = List.nth clauses 0 in
          let%bind genv' = bind_pattern genv xsize pat in
          let%bind args, bsize, bgu = gua_expr genv' branch in
          pure (args, bsize, add_pn bgu cc)
    | Fixpoint (f, _, _) ->
        fail1 ~kind:"Fixpoint not supported" ~inst:(as_error_string f)
          (ER.get_loc (get_rep f))
    | TFun (_, body) ->
        (* Nothing to do except analyzing the body. *)
        let%bind sargs, rsize, rgas = gua_expr genv body in
        pure (sargs, rsize, add_pn rgas cc)
    | TApp (tf, _) ->
        (* Just return the signature of tf. *)
        GUAEnv.resolvS genv (as_string tf) ~lopt:(Some (get_rep tf))
    | Message plist ->
        (* Similar to "Literal", we only spend a small cost (cc) for message creation
         * but charge based on size of message in SendStmt. *)
        let%bind splist =
          foldM
            ~f:(fun acc (_, pl) ->
              match pl with
              | MLit l ->
                  let%bind lc = Gas.literal_cost l in
                  pure (add_pn acc (const_pn lc))
              | MVar i ->
                  let%bind _, irs, _ =
                    GUAEnv.resolvS genv (as_string i) ~lopt:(Some (get_rep i))
                  in
                  pure (add_pn acc (sizeref_to_pol irs)))
            ~init:empty_pn plist
        in
        pure ([], SPol splist, cc)
    | GasExpr _ ->
        fail0 ~kind:"GasUseAnalysis: AST has explicit charges, not supported."
          ?inst:None

  (* Hardcode signature for folds. *)
  let analyze_folds genv =
    (*  list_foldr: forall 'A . forall 'B . g:('A -> 'B -> 'B) -> b:'B -> a:(List 'A) -> 'B *)
    let a =
      mk_typed_id "a" (Datatypes.DataTypeDictionary.list_typ (TypeVar "'A"))
    in
    let g =
      mk_typed_id "g"
        (FunType (TypeVar "'A", FunType (TypeVar "'B", TypeVar "'B")))
    in
    let b = mk_typed_id "b" (TypeVar "'B") in
    let lendep = SizeOf (Length (Base a)) in
    (* The final result size is after applying the fold "Length(a)" times. *)
    let ressize = RFoldAcc (g, Base a, Base b) in
    (* Worst case gas consumed depends on result size after apply "Length(a)" times. *)
    let gapp = GApp (g, [ Element (Base a); ressize ]) in
    (* Gas use polynomial = Length(a) * GApp(g, [Element(a), b]) *)
    let gupol = mul_pn (single_simple_pn lendep) (single_simple_pn gapp) in
    let list_foldr_signature = ([ g; b; a ], ressize, gupol) in
    let genv' = GUAEnv.addS genv "list_foldr" list_foldr_signature in
    (* list_foldl: forall 'A . forall 'B . g:('B -> 'A -> 'B) -> b:'B -> a:(List 'A) -> 'B *)
    (* The final result size is after applying the fold "Length(a)" times. *)
    let ressize' = LFoldAcc (g, Base b, Base a) in
    (* Worst case gas consumed depends on result size after apply "Length(a)" times. *)
    let gapp' = GApp (g, [ ressize'; Element (Base a) ]) in
    let gupol' = mul_pn (single_simple_pn lendep) (single_simple_pn gapp') in
    let list_foldl_signature = ([ g; b; a ], ressize', gupol') in
    let gapp'' = GUAEnv.addS genv' "list_foldl" list_foldl_signature in
    gapp''

  (* Return gas use and result sizeref polynomials of evaluating a sequence of statements. *)
  let rec gua_stmt genv gupol (stmts : stmt_annot list) =
    match stmts with
    | [] -> pure gupol
    | (s, sloc) :: sts -> (
        match s with
        | Load (x, r) ->
            (* The cost of load depends on the size of the state variable. *)
            let gupol' = add_pn gupol (single_simple_pn (SizeOf (Base r))) in
            let signx = ([], Base r, empty_pn) in
            let genv' = GUAEnv.addS genv (as_string x) signx in
            gua_stmt genv' gupol' sts
        | Store (_, r) ->
            (* The cost of store depends on the original size and the new size *)
            (* TODO: Incorporate actual cost from Gas.ml which has max() and subtract. *)
            let%bind _, rs, _ = GUAEnv.resolvS genv (as_string r) in
            let gupol' = add_pn gupol (single_simple_pn (SizeOf rs)) in
            gua_stmt genv gupol' sts
        | Bind (x, e) ->
            let%bind a, s, p = gua_expr genv e in
            let genv' = GUAEnv.addS genv (as_string x) (a, s, p) in
            (* Simple constant const for binding. *)
            let gupol' = add_pn gupol (const_pn 1) in
            (* if a is empty, accumulate the cost of executing expr. *)
            let gupol'' = if a = [] then add_pn gupol' p else gupol' in
            gua_stmt genv' gupol'' sts
        | MapUpdate (_, klist, ropt) ->
            let nindices = const_pn (List.length klist) in
            let%bind c =
              match ropt with
              | Some i ->
                  let%bind _, rs, _ = GUAEnv.resolvS genv (as_string i) in
                  pure @@ single_simple_pn (SizeOf rs)
                  (* update *)
              | None -> pure @@ empty_pn
              (* delete *)
            in
            let gupol' = add_pn nindices c in
            let gupol'' = add_pn gupol gupol' in
            gua_stmt genv gupol'' sts
        | MapGet (x, m, klist, fetchval) ->
            let nindices = const_pn (List.length klist) in
            let sign, pol =
              if fetchval then
                let ressize =
                  List.fold_left (fun acc _ -> Element acc) (Base m) klist
                in
                ( ([], ressize, empty_pn),
                  add_pn nindices (single_simple_pn (SizeOf ressize)) )
              else
                (* TODO: How to represent result of `exists` in map? ?*)
                (([], Base m, empty_pn), nindices)
            in
            let genv' = GUAEnv.addS genv (as_string x) sign in
            let gupol' = add_pn pol gupol in
            gua_stmt genv' gupol' sts
        | ReadFromBC (x, _) ->
            (* TODO: Look at the type and assign cost? *)
            let signx = ([], SPol (const_pn 1), empty_pn) in
            (* Constant cost of 1 to load a blockchain variable. *)
            let gupol' = add_pn gupol (const_pn 1) in
            let genv' = GUAEnv.addS genv (as_string x) signx in
            gua_stmt genv' gupol' sts
        | MatchStmt (x, clauses) ->
            let%bind _, xsize, _ =
              GUAEnv.resolvS genv (as_string x) ~lopt:(Some (get_rep x))
            in
            let num_clauses = const_pn (List.length clauses) in
            let%bind gupol' =
              foldM
                ~f:(fun apn (pat, branch) ->
                  let%bind genv' = bind_pattern genv xsize pat in
                  let%bind bpn = gua_stmt genv' empty_pn branch in
                  pure (add_pn apn bpn))
                ~init:(add_pn gupol num_clauses) clauses
            in
            gua_stmt genv gupol' sts
        | AcceptPayment ->
            (* Constant cost of 1. *)
            gua_stmt genv (add_pn gupol (const_pn 1)) sts
        | SendMsgs i | CreateEvnt i ->
            (* We can at best convey the size of i *)
            let%bind _, s, _ =
              GUAEnv.resolvS genv (as_string i) ~lopt:(Some (get_rep i))
            in
            let gupol' = add_pn gupol (single_simple_pn (SizeOf s)) in
            gua_stmt genv gupol' sts
        | _ -> fail1 ~kind:"Unsupported statement" ?inst:None (SR.get_loc sloc))

  (* Bind identifiers to just sizeref wrappers of themselves. *)
  let identity_bind_ident_list genv idlist =
    List.fold_left
      (fun acc_genv i ->
        let i' = Base i in
        GUAEnv.addS acc_genv (as_string i) ([], i', empty_pn))
      genv idlist

  let gua_component genv (comp : component) =
    let open GUAType in
    let si a t = mk_typed_id a t in
    let all_params =
      [
        ( si ContractUtil.MessagePayload.origin_label (bystrx_typ 20),
          bystrx_typ 20 );
        ( si ContractUtil.MessagePayload.sender_label (bystrx_typ 20),
          bystrx_typ 20 );
        (si ContractUtil.MessagePayload.amount_label uint128_typ, uint128_typ);
      ]
      @ comp.comp_params
    in
    (* Add params to the environment. *)
    let genv' =
      identity_bind_ident_list genv (List.map (fun (i, _) -> i) all_params)
    in
    (* TODO: Add bytesize of message.json. *)
    gua_stmt genv' empty_pn comp.comp_body

  (* Bind lib entries to signatures. *)
  let gua_libentries genv (lel : lib_entry list) =
    foldM
      ~f:(fun genv le ->
        match le with
        | LibVar (lname, _, lexp) ->
            let%bind esig = gua_expr genv lexp in
            pure @@ GUAEnv.addS genv (as_string lname) esig
        | LibTyp _ -> pure genv)
      ~init:genv lel

  let gua_module (cmod : cmodule) (elibs : libtree list) =
    (* Get bindings for folds *)
    let genv_folds = analyze_folds (GUAEnv.mk ()) in

    (* Analyze external libraries first *)
    let%bind genv_elibs =
      let rec recurser libl =
        foldM
          ~f:(fun genv lib ->
            let%bind genv_deps = recurser lib.deps in
            let%bind genv_lib = gua_libentries genv_deps lib.libn.lentries in
            (* retain only _this_ library's (and fold's) entries in env. *)
            let genv_lib' =
              GUAEnv.filterS genv_lib ~f:(fun name ->
                  List.exists
                    (function
                      | LibTyp _ -> false
                      | LibVar (i, _, _) -> as_string i = name)
                    lib.libn.lentries
                  || GUAEnv.existsS genv_folds name)
            in
            pure @@ GUAEnv.appendS genv genv_lib')
          ~init:genv_folds libl
      in
      recurser elibs
    in

    (* Analyze contract libraries *)
    let%bind genv_lib =
      match cmod.libs with
      | Some l -> gua_libentries genv_elibs l.lentries
      | None -> pure genv_elibs
    in

    (* Bind contract parameters. *)
    let si a t = mk_typed_id_from_name a t in
    let all_cparams =
      [
        (si ContractUtil.creation_block_label GUAType.bnum_typ, GUAType.bnum_typ);
      ]
      @ cmod.contr.cparams
    in
    let genv_cparams =
      identity_bind_ident_list genv_lib (List.map (fun (i, _) -> i) all_cparams)
    in

    (* Analyze contract constraints *)
    let%bind _ = gua_expr genv_lib cmod.contr.cconstraint in

    (* Bind state variables. *)
    (* TODO: account for the cost of evaluating state initializers. *)
    let genv_cfields =
      identity_bind_ident_list genv_cparams
        (List.map (fun (i, _, _) -> i) cmod.contr.cfields)
    in
    (* Analyse each component and report it's gas use polynomial. *)
    let%bind pols =
      mapM
        ~f:(fun cp ->
          let%bind pol = gua_component genv_cfields cp in
          pure @@ (cp.comp_name, expand_guref_pol pol))
        cmod.contr.ccomps
    in
    pure pols

  (* A simple wrapper to analyze an isolated expression. *)
  let gua_expr_wrapper erep =
    let genv = GUAEnv.mk () in
    let genv' = analyze_folds genv in
    let%bind params, sr, pol = gua_expr genv' erep in
    (* Expand all parameters (GPol) in the polynomial. *)
    let pol' = expand_guref_pol pol in
    Printf.printf "Gas usage signature:\n%s\n\n"
      (sprint_signature (params, sr, pol'));
    pure (params, sr, pol')
end
