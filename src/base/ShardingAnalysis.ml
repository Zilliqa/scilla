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

  (* In the expression language, all contribution sources are formal parameters.
     In the statement language, all contribution sources are pseudofields, i.e.
     identifier contributions are reduced after every statement *)
  type contrib_source = Pseudofield of pseudofield | FormalParameter of int

  type contrib_cardinality = LinearContrib | NonLinearContrib

  (* There is a design choice to be made here: should we only support builitin
     operations, or should we also support user-defined operations (that the
     programmer needs to prove commutative)? For now, we only support builtins. *)
  type contrib_op = BuiltinOp of builtin

  let pp_contrib_source cs =
    match cs with
    | Pseudofield (f, opt_keys) -> pp_pseudofield f opt_keys
    | FormalParameter i -> "_" ^ string_of_int i

  let pp_contrib_cardinality cc =
    match cc with LinearContrib -> "Linear" | NonLinearContrib -> "NonLinear"

  let pp_contrib_op co = match co with BuiltinOp blt -> pp_builtin blt

  let max_contrib_card a b =
    match (a, b) with
    | NonLinearContrib, _ -> NonLinearContrib
    | _, NonLinearContrib -> NonLinearContrib
    | LinearContrib, LinearContrib -> LinearContrib

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
  module Cond = Set.Make (OrderedContribSource)

  (* keys are contrib_source, values are contrib_summary *)
  type contributions = contrib_summary Contrib.t

  (* How precise are we about the keys in contributions? *)
  (* Exactly: these are exactly the contributions in the result *)
  (* SubsetOf: the result has a subset of these contributions *)
  type source_precision = Exactly | SubsetOf

  type conditionals = Cond.t

  type expr_type = source_precision * contributions * conditionals

  let et_top = (Exactly, Contrib.empty, Cond.empty)

  let min_precision ua ub =
    match (ua, ub) with Exactly, Exactly -> Exactly | _ -> SubsetOf

  let et_list_split (etl : expr_type list) =
    let ps, cs = List.split @@ List.map (fun (a, b, _) -> (a, b)) etl in
    let conds = List.map (fun (_, _, a) -> a) etl in
    (ps, cs, conds)

  let pp_contrib_summary (cs : contrib_summary) =
    let card, ops_set = cs in
    let ops = ContribOps.elements ops_set in
    let ops_str = String.concat " " @@ List.map pp_contrib_op ops in
    let card_str = pp_contrib_cardinality card in
    card_str ^ ", " ^ ops_str

  let pp_contribs (contribs : contributions) =
    Contrib.fold
      (fun co_src co_summ str ->
        str ^ "{" ^ pp_contrib_source co_src ^ ", " ^ pp_contrib_summary co_summ
        ^ "}")
      contribs ""

  let pp_precision u =
    match u with Exactly -> "Exactly" | SubsetOf -> "SubsetOf"

  let pp_conditionals c =
    let sources = Cond.elements c in
    String.concat " " @@ List.map pp_contrib_source sources

  let pp_expr_type (et : expr_type) =
    let ps, cs, cond = et in
    let show_precision = Contrib.cardinal cs > 0 in
    let show_cond = Cond.cardinal cond > 0 in
    let precision_str = if show_precision then pp_precision ps ^ " " else "" in
    let cond_str = if show_cond then " cond? " ^ pp_conditionals cond else "" in
    precision_str ^ pp_contribs cs ^ cond_str

  (* For each contract component, we keep track of the operations it performs.
     This gives us enough information to tell whether two transitions have disjoint
     footprints and thus commute. *)
  type component_operation =
    (* Read of cfield, with map keys which are comp_params if field is a map *)
    | Read of pseudofield
    | Write of pseudofield * expr_type
    | AcceptMoney
    | SendMessages
    (* Top element -- in case of ambiguity, be conservative *)
    | AlwaysExclusive

  let pp_operation op =
    match op with
    | Read (field, opt_keys) -> "Read " ^ pp_pseudofield field opt_keys
    | Write ((field, opt_keys), et) ->
        "Write " ^ pp_pseudofield field opt_keys ^ " (" ^ pp_expr_type et ^ ")"
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
    List.fold_left (fun acc op -> acc ^ "  " ^ pp_operation op ^ "\n") "" ops

  type component_summary = ComponentSummary.t

  type signature =
    (* ComponentSig: comp_params * component_summary *)
    | ComponentSig of (ER.rep ident * typ) list * component_summary
    (* Within a transition, we assign an identifier to all field values, i.e. we
       only treat final writes as proper writes, with all others being "global"
       bindings. This lets us track multiple reads/writes to a field in a
       transition in the same way we track expressions.*)
    | IdentSig of ident_shadow_status * expr_type

  let pp_sig k sgn =
    match sgn with
    | ComponentSig (comp_params, comp_summ) ->
        let ns = "State footprint for " ^ k in
        let ps =
          String.concat ", " @@ List.map (fun (i, _) -> get_id i) comp_params
        in
        let cs = pp_summary comp_summ in
        ns ^ "(" ^ ps ^ "): \n" ^ cs
    | IdentSig (_, et) ->
        let ns = "Signature for " ^ k ^ ": " in
        ns ^ pp_expr_type et

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
      let l = List.rev @@ to_list env in
      List.fold_left (fun acc (k, sgn) -> acc ^ pp_sig k sgn ^ "\n") "" l
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

  let et_pseudofield (f, opt_keys) =
    (Exactly, contrib_pseudofield (f, opt_keys), Cond.empty)

  let contribs_add_op (x : contributions) (op : contrib_op) =
    let cs_add_op (ccard, cops) op =
      let cops' = ContribOps.add op cops in
      (ccard, cops')
    in
    Contrib.map (fun cs -> cs_add_op cs op) x

  (* Combine contributions "in sequence", e.g. builtin add a b *)
  let contrib_union a b =
    (* Any combination of the same ident leads to a NonLinearContrib *)
    let combine_seq ident (_, opsa) (_, opsb) =
      let ops = ContribOps.union opsa opsb in
      Some (NonLinearContrib, ops)
    in
    Contrib.union combine_seq a b

  let et_union (eta : expr_type) (etb : expr_type) =
    let ufa, csa, cnda = eta in
    let ufb, csb, cndb = etb in
    let uf = min_precision ufa ufb in
    let cs = contrib_union csa csb in
    let cnd = Cond.union cnda cndb in
    (uf, cs, cnd)

  (* Combine contributions "in parallel", e.g. match *)
  let contrib_combine_par (carda, opsa) (cardb, opsb) =
    let card = max_contrib_card carda cardb in
    let ops = ContribOps.union opsa opsb in
    (card, ops)

  let contrib_upper_bound a b =
    Contrib.union (fun k x y -> Some (contrib_combine_par x y)) a b

  (* Combine contributions by multiplication, e.g. function application *)
  let contrib_product (multiple : contributions) (single : contrib_summary) =
    Contrib.map (fun c -> contrib_combine_par c single) multiple

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
    let tt_contrib_source cs =
      match cs with
      | Pseudofield (f, Some keys) -> Pseudofield (f, Some (map_keys keys))
      (* This is only applied to pseudofields *)
      | _ -> cs
    in
    let translate_comp_contribs contribs =
      let c_keys, c_values = List.split @@ Contrib.bindings contribs in
      let new_keys = List.map tt_contrib_source c_keys in
      let new_bindings = List.combine new_keys c_values in
      Contrib.of_seq (List.to_seq new_bindings)
    in
    let translate_comp_cond cond =
      let new_bindings = List.map tt_contrib_source (Cond.elements cond) in
      Cond.of_seq (List.to_seq new_bindings)
    in
    let translate_comp_et (et : expr_type) =
      let un, cs, cond = et in
      (un, translate_comp_contribs cs, translate_comp_cond cond)
    in

    match op with
    | Read (f, Some keys) -> Read (f, Some (map_keys keys))
    | Write ((f, Some keys), et) ->
        Write ((f, Some (map_keys keys)), translate_comp_et et)
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

  let get_ident_et senv i =
    let%bind isig = SAEnv.resolvS senv (get_id i) in
    match isig with
    | IdentSig (_, c) -> pure @@ c
    (* If this happens, it's a bug *)
    | _ -> fail0 "Sharding analysis: ident does not have a signature"

  (* Add a new identifier to the environment, keeping track of whether it
     shadows a component parameter *)
  let env_new_ident i et senv =
    let id = get_id i in
    let opt_shadowed = SAEnv.lookupS senv id in
    let new_id_sig =
      match opt_shadowed with
      | None -> IdentSig (DoesNotShadow, et)
      | Some shadowed_sig -> (
          match shadowed_sig with
          | IdentSig (ComponentParameter, _) ->
              IdentSig (ShadowsComponentParameter, et)
          | _ -> IdentSig (DoesNotShadow, et) )
    in
    SAEnv.addS senv id new_id_sig

  (* Helper for applying functions *)
  let tt_formal_parameters ~f ~none fp_contribs arg_contribs =
    List.mapi
      (fun idx argc ->
        let opt_fpc = Contrib.find_opt (FormalParameter idx) fp_contribs in
        match opt_fpc with
        | Some fpc -> f argc fpc
        (* If the formal parameter is not used, argument is not used *)
        | None -> none)
      arg_contribs

  let tt_conds ~f ~none fp_conds arg_contribs =
    List.mapi
      (fun idx argc ->
        let opt_fpc = Cond.find_opt (FormalParameter idx) fp_conds in
        match opt_fpc with
        | Some fpc -> f argc fpc
        (* If the formal parameter is not used, argument is not used *)
        | None -> none)
      arg_contribs

  (* TODO: define a wrapper for sa_expr *)
  (* fp_count keeps track of how many function formal parameters we've encountered *)
  let rec sa_expr senv (fp_count : int) (erep : expr_annot) =
    let e, rep = erep in
    match e with
    | Literal l -> pure @@ et_top
    | Var i ->
        let%bind ic = get_ident_et senv i in
        pure @@ ic
    | Builtin ((b, _), actuals) ->
        let%bind arg_ets = mapM actuals ~f:(fun i -> get_ident_et senv i) in
        let et = List.fold_left et_union et_top arg_ets in
        let uf, cs, cnds = et in
        let cs_wops =
          Contrib.map
            (fun (co_cc, co_ops) ->
              let co_ops' =
                ContribOps.union co_ops (ContribOps.singleton (BuiltinOp b))
              in
              (co_cc, co_ops'))
            cs
        in
        pure @@ (uf, cs_wops, cnds)
    | Message bs ->
        let get_payload_et pld =
          match pld with
          | MLit l -> pure @@ et_top
          | MVar i ->
              let%bind ic = get_ident_et senv i in
              pure @@ ic
        in
        let _, plds = List.split bs in
        let%bind pld_ets = mapM get_payload_et plds in
        pure @@ List.fold_left et_union et_top pld_ets
    | Constr (cname, _, actuals) ->
        let%bind arg_ets = mapM actuals ~f:(fun i -> get_ident_et senv i) in
        pure @@ List.fold_left et_union et_top arg_ets
    | MatchExpr (x, clauses) ->
        let%bind xc = get_ident_et senv x in
        let _, xcontr, xcond = xc in
        let clause_et (pattern, cl_expr) =
          let binders = get_pattern_bounds pattern in
          let senv' =
            List.fold_left
              (* Each binder in the pattern gets the full contributions of x *)
                (fun env_acc id -> env_new_ident id xc env_acc)
              senv binders
          in
          sa_expr senv' fp_count cl_expr
        in
        let%bind cl_ets = mapM clause_et clauses in
        let cl_pss, cl_contribs, cl_conds = et_list_split cl_ets in
        let fc = List.hd cl_contribs in
        let all_equal = List.for_all (fun c -> c = fc) cl_contribs in
        let cl_ps = List.fold_left min_precision Exactly cl_pss in
        (* The precision of the match is: Exactly if all clauses are equal and
           precise, otherwise SubsetOf *)
        let match_ps =
          min_precision (if all_equal then Exactly else SubsetOf) cl_ps
        in
        (* The match's contributions is the LUB of the clauses *)
        let match_contribs =
          List.fold_left contrib_upper_bound Contrib.empty cl_contribs
        in
        (* TODO: add logic for  operations *)
        let match_cond =
          let this_cond =
            Cond.of_list @@ fst @@ List.split @@ Contrib.bindings xcontr
          in
          let this_cond = Cond.union this_cond xcond in
          List.fold_left Cond.union this_cond cl_conds
        in
        pure @@ (match_ps, match_contribs, match_cond)
    | Let (i, _, lhs, rhs) ->
        let%bind lhs_et = sa_expr senv fp_count lhs in
        let senv' = env_new_ident i lhs_et senv in
        sa_expr senv' fp_count rhs
    | Fun (formal, _, body) ->
        (* Formal parameters are given a linear contribution when producing
           function summaries. Arguments (see App) might be nonlinear. *)
        let fp_contrib =
          Contrib.singleton (FormalParameter fp_count)
            (LinearContrib, ContribOps.empty)
        in
        let fp_et = (Exactly, fp_contrib, Cond.empty) in
        let senv' = env_new_ident formal fp_et senv in
        sa_expr senv' (fp_count + 1) body
    | App (f, actuals) ->
        let fun_sig f =
          let%bind fs = SAEnv.resolvS senv (get_id f) in
          match fs with
          | IdentSig (_, c) -> pure @@ c
          | _ ->
              fail0 "Sharding analysis: applied function does not have IdentSig"
        in
        let%bind arg_ets = mapM actuals ~f:(fun i -> get_ident_et senv i) in
        let arg_ps, arg_contribs, _ = et_list_split arg_ets in
        (* f's summary has arguments named _0, _1, etc. *)
        (* CAREFUL: don't rely on order of elements in map; Ord.compare may change *)
        let%bind fp_ets = fun_sig f in
        let fp_ps, fp_contribs, fp_conds = fp_ets in
        (* Distribute each argument's contributions over the respective formal
           parameter's contribution *)
        let contribs_pairwise =
          tt_formal_parameters ~f:contrib_product ~none:Contrib.empty
            fp_contribs arg_contribs
        in
        (* Which argument contributions flowed into conditionals in the function? *)
        let conds_pairwise =
          tt_conds
            ~f:(fun argc _ ->
              Cond.of_list @@ fst @@ List.split @@ Contrib.bindings argc)
            ~none:Cond.empty fp_conds arg_contribs
        in
        let app_ps = List.fold_left min_precision fp_ps arg_ps in
        let app_contrib =
          List.fold_left contrib_union Contrib.empty contribs_pairwise
        in
        let app_conds = List.fold_left Cond.union Cond.empty conds_pairwise in
        pure @@ (app_ps, app_contrib, app_conds)
    (* TODO: be defensive about unsupported instructions *)
    | _ -> pure @@ et_top

  (* Precondition: senv contains the component parameters, appropriately marked *)
  let rec sa_stmt senv summary (stmts : stmt_annot list) =
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
              (et_pseudofield (f, None))
              (Read (f, None))
              summary sts
        | Store (f, i) ->
            let%bind ic = get_ident_et senv i in
            cont_op (Write ((f, None), ic)) summary sts
        | MapGet (x, m, klist, _) ->
            let op =
              if map_access_can_be_summarised senv m klist then
                Read (m, Some klist)
              else AlwaysExclusive
            in
            cont_ident_op x (et_pseudofield (m, Some klist)) op summary sts
        | MapUpdate (m, klist, opt_i) ->
            let%bind ic =
              match opt_i with
              | Some i -> get_ident_et senv i
              | None -> pure @@ et_top
            in
            let op =
              if map_access_can_be_summarised senv m klist then
                Write ((m, Some klist), ic)
              else AlwaysExclusive
            in
            cont_op op summary sts
        | AcceptPayment -> cont_op AcceptMoney summary sts
        | SendMsgs i -> cont_op SendMessages summary sts
        (* TODO: Do we want to track blockchain reads? *)
        | ReadFromBC (x, _) -> cont_ident x et_top summary sts
        | Bind (x, expr) ->
            let%bind expr_contrib = sa_expr senv 0 expr in
            let e, rep = expr in
            (* print_endline @@ pp_expr e ^ "
               " ^ pp_contribs expr_contrib ^ "
               "; *)
            cont_ident x expr_contrib summary sts
        | MatchStmt (x, clauses) ->
            let%bind xc = get_ident_et senv x in
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
            (* TODO: LUB *)
            (* summaries are composed via set union *)
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
        (* FIXME: be defensive about unsupported instructions *)
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
        (IdentSig (ComponentParameter, et_top))
    in
    sa_stmt senv' ComponentSummary.empty comp.comp_body

  let sa_libentries senv (lel : lib_entry list) =
    foldM
      ~f:(fun senv le ->
        match le with
        | LibVar (lname, _, lexp) ->
            let%bind esig = sa_expr senv 0 lexp in
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

    (* Bind contract parameters *)
    let senv =
      let prs, _ = List.split cmod.contr.cparams in
      env_bind_ident_list senv prs (IdentSig (DoesNotShadow, et_top))
    in

    (* This is a combined map and fold: fold for senv', map for summaries *)
    let%bind senv, _ =
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

    (* let summaries = List.rev summaries in *)
    pure senv
end
