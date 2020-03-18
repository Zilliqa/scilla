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

(* Sharding Analysis for Scilla contracts. *)

open Core_kernel.Result.Let_syntax
open TypeUtil
open Syntax
open ErrorUtils
open MonadUtil

module ScillaSA
    (SR : Rep) (ER : sig
      include Rep

      val get_type : rep -> PlainTypes.t inferred_type

      val mk_id : loc ident -> typ -> rep ident
    end) =
struct
  module SER = SR
  module EER = ER
  module SASyntax = ScillaSyntax (SR) (ER)
  module TU = TypeUtilities
  open SASyntax

  (* For each contract component, we keep track of the operations it performs.
     This gives us enough information to tell whether two transitions have disjoint
     footprints and thus commute. *)
  type component_operation =
    (* Read of cfield, with map keys which are comp_params if field is a map *)
    | Read of ER.rep ident * ER.rep ident list option
    | Write of ER.rep ident * ER.rep ident list option
    | AcceptMoney
    | SendMessages
    (* Top element -- in case of ambiguity, be conservative *)
    | AlwaysExclusive

  let sprint_operation op =
    let field_access field opt_keys =
      let base = get_id field in
      let keys =
        match opt_keys with
        | Some ks ->
            List.fold_left (fun acc kid -> acc ^ "[" ^ get_id kid ^ "]") "" ks
        | None -> ""
      in
      base ^ keys
    in

    match op with
    | Read (field, opt_keys) -> "Read " ^ field_access field opt_keys
    | Write (field, opt_keys) -> "Write " ^ field_access field opt_keys
    | AcceptMoney -> "AcceptMoney"
    | SendMessages -> "SendMessages"
    | AlwaysExclusive -> "AlwaysExclusive"

  module OrderedComponentOperation = struct
    type t = component_operation

    (* This is super hacky, but works *)
    let compare a b =
      let str_a = sprint_operation a in
      let str_b = sprint_operation b in
      compare str_a str_b
  end

  (* A component's summary is the set of the operations it performs *)
  module ComponentSummary = Set.Make (OrderedComponentOperation)

  let sprint_summary summ =
    let ops = ComponentSummary.elements summ in
    List.fold_left (fun acc op -> acc ^ sprint_operation op ^ "\n") "" ops

  type component_summary = ComponentSummary.t

  (* We keep track of whether identifiers in the impure part of the language
     shadow any of their component's parameters *)
  type ident_shadow_status =
    | ShadowsComponentParameter
    | ComponentParameter
    | DoesNotShadow

  type signature =
    (* ComponentSig: comp_params * component_summary *)
    | ComponentSig of (ER.rep ident * typ) list * component_summary
    | IdentSig of ident_shadow_status

  module SAEnv = struct
    open AssocDictionary

    (* A map from identifier strings to their signatures. *)
    type t = signature dict

    (* Make an empty environment. *)
    let mk = make_dict

    (* In env, add mapping id => s *)
    let addS env id s = insert id s env

    (* Return None if key doesn't exist *)
    let lookupS env id = lookup id env

    (* In env, resolve id => s and return s (fails if cannot resolve). *)
    let resolvS ?(lopt = None) env id =
      match lookup id env with
      | Some s -> pure s
      | None ->
          let sloc =
            match lopt with Some l -> ER.get_loc l | None -> dummy_loc
          in
          fail1
            (Printf.sprintf
               "Couldn't resolve the identifier in sharding analysis: %s.\n" id)
            sloc

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
          acc ^ "Signature for " ^ k ^ ": " ^ sign ^ "----\n")
        "" l
  end

  let env_bind_component senv comp (sgn : signature) =
    let i = comp.comp_name in
    SAEnv.addS senv (get_id i) sgn

  let env_bind_ident_list senv idlist (sgn : signature) =
    List.fold_left
      (fun acc_senv i -> SAEnv.addS acc_senv (get_id i) sgn)
      senv idlist

  let is_bottom_level_access m klist =
    let mt = (ER.get_type (get_rep m)).tp in
    let nindices = List.length klist in
    let map_access = nindices < TU.map_depth mt in
    not map_access

  let all_keys_are_parameters senv klist =
    let is_component_parameter k senv =
      let m = SAEnv.lookupS senv (get_id k) in
      match m with
      | None -> false
      | Some m_sig -> m_sig = IdentSig ComponentParameter
    in
    List.for_all (fun k -> is_component_parameter k senv) klist

  let map_access_can_be_summarised senv m klist =
    is_bottom_level_access m klist && all_keys_are_parameters senv klist

  let translate_op op old_params new_params =
    let old_names = List.map (fun p -> get_id p) old_params in
    let mapping = List.combine old_names new_params in
    (* The assoc will fail only if there's a bug *)
    let map_keys keys =
      List.map (fun k -> List.assoc (get_id k) mapping) keys
    in
    match op with
    | Read (f, Some keys) -> Read (f, Some (map_keys keys))
    | Write (f, Some keys) -> Write (f, Some (map_keys keys))
    | _ -> op

  let procedure_call_summary senv (proc_sig : signature) arglist =
    let can_summarise = all_keys_are_parameters senv arglist in
    if can_summarise then
      match proc_sig with
      | ComponentSig (proc_params, proc_summ) ->
          let proc_params, _ = List.split proc_params in
          pure
          @@ ComponentSummary.map
               (fun op -> translate_op op proc_params arglist)
               proc_summ
      | _ ->
          (* If this occurs, it's a bug. *)
          fail0 "Sharding analysis: procedure summary is not of the right type"
    else pure @@ ComponentSummary.singleton AlwaysExclusive

  (* Precondition: senv contains the component parameters, appropriately marked *)
  let rec sa_stmt senv summary (stmts : stmt_annot list) =
    (* Add a new identifier to the environment, keeping track of whether it
       shadows a component parameter *)
    let env_new_ident i senv =
      let id = get_id i in
      let opt_shadowed = SAEnv.lookupS senv id in
      let new_id_sig =
        match opt_shadowed with
        | None -> IdentSig DoesNotShadow
        | Some shadowed_sig -> (
            match shadowed_sig with
            | IdentSig ComponentParameter -> IdentSig ShadowsComponentParameter
            | _ -> IdentSig DoesNotShadow )
      in
      SAEnv.addS senv id new_id_sig
    in

    (* Helpers to continue after accumulating an operation *)
    let cont senv summary sts = sa_stmt senv summary sts in
    (* Perform an operation *)
    let cont_op op summary sts =
      let summary' = ComponentSummary.add op summary in
      cont senv summary' sts
    in
    (* Introduce a new identifier *)
    let cont_ident ident summary sts =
      let senv' = env_new_ident ident senv in
      cont senv' summary sts
    in
    (* Perform an operation and introduce an identifier *)
    let cont_ident_op ident op summary sts =
      let senv' = env_new_ident ident senv in
      let summary' = ComponentSummary.add op summary in
      cont senv' summary' sts
    in

    (* We can assume everything is well-typed, which makes this much easier *)
    match stmts with
    | [] -> pure summary
    | (s, sloc) :: sts -> (
        match s with
        | Load (x, f) -> cont_ident_op x (Read (f, None)) summary sts
        | Store (f, _) -> cont_op (Write (f, None)) summary sts
        | MapGet (x, m, klist, _) ->
            let op =
              if map_access_can_be_summarised senv m klist then
                Read (m, Some klist)
              else AlwaysExclusive
            in
            cont_ident_op x op summary sts
        | MapUpdate (m, klist, _) ->
            let op =
              if map_access_can_be_summarised senv m klist then
                Write (m, Some klist)
              else AlwaysExclusive
            in
            cont_op op summary sts
        | AcceptPayment -> cont_op AcceptMoney summary sts
        | SendMsgs i -> cont_op SendMessages summary sts
        | Bind (x, _) | ReadFromBC (x, _) -> cont_ident x summary sts
        | MatchStmt (x, clauses) ->
            let summarise_clause (pattern, cl_sts) =
              let binders = get_pattern_bounds pattern in
              let senv' =
                List.fold_left
                  (fun env_acc id -> env_new_ident id env_acc)
                  senv binders
              in
              sa_stmt senv' summary cl_sts
            in
            let%bind cl_summaries = mapM summarise_clause clauses in
            let%bind summary' =
              foldM
                (fun acc_summ cl_summ ->
                  pure @@ ComponentSummary.union acc_summ cl_summ)
                summary cl_summaries
            in
            cont senv summary' sts
        | CallProc (p, arglist) -> (
            let opt_proc_sig = SAEnv.lookupS senv (get_id p) in
            match opt_proc_sig with
            | Some proc_sig ->
                let%bind call_summ =
                  procedure_call_summary senv proc_sig arglist
                in
                let summary' = ComponentSummary.union summary call_summ in
                cont senv summary' sts
            (* If this occurs, it's a bug. Type checking should prevent it. *)
            | _ ->
                fail1
                  "Sharding analysis: calling procedure that was not analysed"
                  (SR.get_loc (get_rep p)) )
        (* TODO: be defensive about unsupported instructions *)
        | _ -> cont senv summary sts )

  let sa_component_summary senv (comp : component) =
    let open PrimTypes in
    let si a t = ER.mk_id (mk_ident a) t in
    let all_params =
      [
        ( si ContractUtil.MessagePayload.sender_label (bystrx_typ 20),
          bystrx_typ 20 );
        (si ContractUtil.MessagePayload.amount_label uint128_typ, uint128_typ);
      ]
      @ comp.comp_params
    in
    (* Add component parameters to the analysis environment *)
    let senv' =
      env_bind_ident_list senv
        (List.map (fun (i, _) -> i) all_params)
        (IdentSig ComponentParameter)
    in
    sa_stmt senv' ComponentSummary.empty comp.comp_body

  let sa_module (cmod : cmodule) (elibs : libtree list) =
    (* Stage 1: determine state footprint of components *)
    let senv = SAEnv.mk () in

    (* This is a combined map and fold: fold for senv', map for summaries *)
    let%bind senv', summaries =
      foldM
        (fun (senv_acc, summ_acc) comp ->
          let%bind comp_summ = sa_component_summary senv_acc comp in
          let senv' =
            env_bind_component senv_acc comp
              (ComponentSig (comp.comp_params, comp_summ))
          in
          let summaries = (comp.comp_name, comp_summ) :: summ_acc in
          pure @@ (senv', summaries))
        (senv, []) cmod.contr.ccomps
    in
    let summaries = List.rev summaries in

    pure summaries
end
