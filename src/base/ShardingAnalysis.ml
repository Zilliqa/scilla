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

  (* field name, with optional map keys; if the field is a map, the pseudofield
     is always a bottom-level access *)
  type pseudofield = ER.rep ident * ER.rep ident list option

  let pp_pseudofield field opt_keys =
    let base = get_id field in
    let keys =
      match opt_keys with
      | Some ks ->
          List.fold_left (fun acc kid -> acc ^ "[" ^ get_id kid ^ "]") "" ks
      | None -> ""
    in
    base ^ keys

  (* We keep track of whether identifiers in the impure part of the language
     shadow any of their component's parameters *)
  type ident_shadow_status =
    | ShadowsComponentParameter
    | ComponentParameter
    | DoesNotShadow

  (* In the expression language, all contribution sources are bindings In
     the statement language, all contribution sources are pseudofields, i.e.
     identifier contributions are reduced after every statement *)
  type contrib_source = Pseudofield of pseudofield | Binding of ER.rep ident

  type contrib_cardinality = LinearContrib | NonLinearContrib

  (* There is a design choice to be made here: should we only support builitin
     operations, or should we also support user-defined operations (that the
     programmer needs to prove commutative)? For now, we only support builtins. *)
  type contrib_op = BuiltinOp of builtin

  let pp_contrib_source cs =
    match cs with
    | Pseudofield (f, opt_keys) -> pp_pseudofield f opt_keys
    | Binding i -> get_id i

  let pp_contrib_cardinality cc =
    match cc with LinearContrib -> "Linear" | NonLinearContrib -> "NonLinear"

  let pp_contrib_op co = match co with BuiltinOp blt -> pp_builtin blt

  module OrderedContribOp = struct
    type t = contrib_op

    let compare a b =
      match (a, b) with
      | BuiltinOp a, BuiltinOp b -> compare (pp_builtin a) (pp_builtin b)
  end

  module ContribOps = Set.Make (OrderedContribOp)

  type contrib_ops = ContribOps.t

  type contrib_summary = contrib_cardinality * contrib_ops

  module OrderedContribSource = struct
    type t = contrib_source

    let compare a b = compare (pp_contrib_source a) (pp_contrib_source b)
  end

  module Contrib = Map.Make (OrderedContribSource)

  (* keys are contrib_source, values are contrib_summary *)
  type contributions = contrib_summary Contrib.t

  let pp_contrib_summary (cs : contrib_summary) =
    let card, ops_set = cs in
    let ops = ContribOps.elements ops_set in
    let ops_str =
      List.fold_left (fun s op -> s ^ pp_contrib_op op ^ " ") "" ops
    in
    let card_str = pp_contrib_cardinality card in
    card_str ^ ", [" ^ ops_str ^ "]"

  let pp_contribs (contribs : contributions) =
    Contrib.fold
      (fun co_src co_summ str ->
        str ^ "{" ^ pp_contrib_source co_src ^ ", " ^ pp_contrib_summary co_summ
        ^ "} ")
      contribs ""

  (* For each contract component, we keep track of the operations it performs.
     This gives us enough information to tell whether two transitions have disjoint
     footprints and thus commute. *)
  type component_operation =
    (* Read of cfield, with map keys which are comp_params if field is a map *)
    | Read of pseudofield
    | Write of pseudofield * contributions
    | AcceptMoney
    | SendMessages
    (* Top element -- in case of ambiguity, be conservative *)
    | AlwaysExclusive

  let pp_operation op =
    match op with
    | Read (field, opt_keys) -> "Read " ^ pp_pseudofield field opt_keys
    | Write ((field, opt_keys), contribs) ->
        "Write "
        ^ pp_pseudofield field opt_keys
        ^ " (" ^ pp_contribs contribs ^ ")"
    | AcceptMoney -> "AcceptMoney"
    | SendMessages -> "SendMessages"
    | AlwaysExclusive -> "AlwaysExclusive"

  module OrderedComponentOperation = struct
    type t = component_operation

    (* This is super hacky, but works *)
    let compare a b =
      let str_a = pp_operation a in
      let str_b = pp_operation b in
      compare str_a str_b
  end

  (* A component's summary is the set of the operations it performs *)
  module ComponentSummary = Set.Make (OrderedComponentOperation)

  let pp_summary summ =
    let ops = ComponentSummary.elements summ in
    List.fold_left (fun acc op -> acc ^ pp_operation op ^ "\n") "" ops

  type component_summary = ComponentSummary.t

  type signature =
    (* ComponentSig: comp_params * component_summary *)
    | ComponentSig of (ER.rep ident * typ) list * component_summary
    (* Within a transition, we assign an identifier to all field values, i.e. we
       only treat final writes as proper writes, with all others being "global"
       bindings. This lets us track multiple reads/writes to a field in a
       transition in the same way we track expressions.*)
    | IdentSig of ident_shadow_status * contributions

  (* XXX: I copied this from GasUsageAnalysis; why not use a Map? *)
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

  let contrib_pseudofield (f, opt_keys) =
    let csumm = (LinearContrib, ContribOps.empty) in
    let csrc = Pseudofield (f, opt_keys) in
    Contrib.singleton csrc csumm

  let contrib_union a b =
    (* Any combination of the same ident leads to a NonLinearContrib *)
    let combine_contrib_summary ident (_, opsa) (_, opsb) =
      let ops = ContribOps.union opsa opsb in
      Some (NonLinearContrib, ops)
    in
    Contrib.union combine_contrib_summary a b

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
      | Some m_sig -> (
          match m_sig with
          | IdentSig (ComponentParameter, _) -> true
          | _ -> false )
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
    let translate_comp_contribs contribs =
      let c_keys, c_values = List.split @@ Contrib.bindings contribs in
      let tt_contrib_source cs =
        match cs with
        | Pseudofield (f, Some keys) -> Pseudofield (f, Some (map_keys keys))
        (* This is only applied to pseudofields *)
        | _ -> cs
      in
      let new_keys = List.map tt_contrib_source c_keys in
      let new_bindings = List.combine new_keys c_values in
      Contrib.of_seq (List.to_seq new_bindings)
    in

    match op with
    | Read (f, Some keys) -> Read (f, Some (map_keys keys))
    | Write ((f, Some keys), cs) ->
        Write ((f, Some (map_keys keys)), translate_comp_contribs cs)
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

  let get_ident_contrib senv i =
    let%bind isig = SAEnv.resolvS senv (get_id i) in
    match isig with
    | IdentSig (_, c) -> pure @@ c
    (* If this happens, it's a bug *)
    | _ -> fail0 "Sharding analysis: ident does not have a signature"

  let rec sa_expr senv contrib (erep : expr_annot) =
    let e, rep = erep in
    match e with
    | Literal l -> pure @@ contrib
    | Var i ->
        let%bind ic = get_ident_contrib senv i in
        pure @@ contrib_union contrib ic
    | Builtin ((b, _), actuals) ->
        let%bind arg_contribs =
          mapM actuals ~f:(fun i -> get_ident_contrib senv i)
        in
        let c = List.fold_left contrib_union Contrib.empty arg_contribs in
        let c_wops =
          Contrib.map
            (fun (co_cc, co_ops) ->
              let co_ops' =
                ContribOps.union co_ops (ContribOps.singleton (BuiltinOp b))
              in
              (co_cc, co_ops'))
            c
        in
        pure @@ c_wops
    | _ -> pure @@ contrib

  (* Precondition: senv contains the component parameters, appropriately marked *)
  let rec sa_stmt senv summary (stmts : stmt_annot list) =
    (* Add a new identifier to the environment, keeping track of whether it
       shadows a component parameter *)
    let env_new_ident i contrib senv =
      let id = get_id i in
      let opt_shadowed = SAEnv.lookupS senv id in
      let new_id_sig =
        match opt_shadowed with
        | None -> IdentSig (DoesNotShadow, contrib)
        | Some shadowed_sig -> (
            match shadowed_sig with
            | IdentSig (ComponentParameter, _) ->
                IdentSig (ShadowsComponentParameter, contrib)
            | _ -> IdentSig (DoesNotShadow, contrib) )
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
    let cont_ident ident contrib summary sts =
      let senv' = env_new_ident ident contrib senv in
      cont senv' summary sts
    in
    (* Perform an operation and introduce an identifier *)
    let cont_ident_op ident contrib op summary sts =
      let senv' = env_new_ident ident contrib senv in
      let summary' = ComponentSummary.add op summary in
      cont senv' summary' sts
    in

    (* We can assume everything is well-typed, which makes this much easier *)
    match stmts with
    | [] -> pure summary
    | (s, sloc) :: sts -> (
        match s with
        | Load (x, f) ->
            cont_ident_op x
              (contrib_pseudofield (f, None))
              (Read (f, None))
              summary sts
        | Store (f, i) ->
            let%bind ic = get_ident_contrib senv i in
            cont_op (Write ((f, None), ic)) summary sts
        | MapGet (x, m, klist, _) ->
            let op =
              if map_access_can_be_summarised senv m klist then
                Read (m, Some klist)
              else AlwaysExclusive
            in
            cont_ident_op x (contrib_pseudofield (m, Some klist)) op summary sts
        | MapUpdate (m, klist, opt_i) ->
            let%bind ic =
              match opt_i with
              | Some i -> get_ident_contrib senv i
              | None -> pure @@ Contrib.empty
            in
            let op =
              if map_access_can_be_summarised senv m klist then
                Write ((m, Some klist), ic)
              else AlwaysExclusive
            in
            cont_op op summary sts
        | AcceptPayment -> cont_op AcceptMoney summary sts
        | SendMsgs i -> cont_op SendMessages summary sts
        (* XXX: Do we want to track blockchain reads? *)
        | ReadFromBC (x, _) -> cont_ident x Contrib.empty summary sts
        | Bind (x, expr) ->
            let%bind expr_contrib = sa_expr senv Contrib.empty expr in
            cont_ident x expr_contrib summary sts
        | MatchStmt (x, clauses) ->
            let%bind xc = get_ident_contrib senv x in
            let summarise_clause (pattern, cl_sts) =
              let binders = get_pattern_bounds pattern in
              let senv' =
                List.fold_left
                  (* Each binder in the pattern gets the full contributions of x *)
                    (fun env_acc id -> env_new_ident id xc env_acc)
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
        (IdentSig (ComponentParameter, Contrib.empty))
    in
    sa_stmt senv' ComponentSummary.empty comp.comp_body

  let sa_libentries senv (lel : lib_entry list) =
    foldM
      ~f:(fun senv le ->
        match le with
        | LibVar (lname, _, lexp) ->
            let%bind esig = sa_expr senv Contrib.empty lexp in
            pure
            @@ SAEnv.addS senv (get_id lname) (IdentSig (DoesNotShadow, esig))
        | LibTyp _ -> pure senv)
      ~init:senv lel

  let sa_module (cmod : cmodule) (elibs : libtree list) =
    (* Stage 1: determine state footprint of components *)
    let senv = SAEnv.mk () in

    (* Analyze contract libraries *)
    let%bind senv =
      match cmod.libs with
      | Some l -> sa_libentries senv l.lentries
      | None -> pure @@ senv
    in

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
