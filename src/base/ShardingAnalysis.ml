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
  module SCU = ContractUtil.ScillaContractUtil (SR) (ER)
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

  type dependencies = conditionals

  type expr_type = source_precision * contributions * conditionals

  let et_top = (Exactly, Contrib.empty, Cond.empty)

  let min_precision ua ub =
    match (ua, ub) with Exactly, Exactly -> Exactly | _ -> SubsetOf

  let et_list_split (etl : expr_type list) =
    let ps, cs = List.split @@ List.map (fun (a, b, _) -> (a, b)) etl in
    let conds = List.map (fun (_, _, a) -> a) etl in
    (ps, cs, conds)

  let et_equal (a : expr_type) (b : expr_type) =
    let psa, contra, condsa = a in
    let psb, contrb, condsb = b in
    let ps_eq = psa = psb in
    let contr_eq =
      Contrib.equal
        (fun (cca, copa) (ccb, copb) -> cca = ccb && ContribOps.equal copa copb)
        contra contrb
    in
    let conds_eq = Cond.equal condsa condsb in
    ps_eq && contr_eq && conds_eq

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
    | ConditionOn of dependencies
    | EmitEvent of dependencies
    | SendMessages of dependencies
    (* Top element -- in case of ambiguity, be conservative *)
    | AlwaysExclusive of ErrorUtils.loc option * string

  let pp_operation op =
    match op with
    | Read (field, opt_keys) -> "Read " ^ pp_pseudofield field opt_keys
    | Write ((field, opt_keys), et) ->
        "Write " ^ pp_pseudofield field opt_keys ^ " (" ^ pp_expr_type et ^ ")"
    | AcceptMoney -> "AcceptMoney"
    | ConditionOn conds -> "ConditionOn " ^ pp_conditionals conds
    | EmitEvent conds -> "EmitEvent " ^ pp_conditionals conds
    | SendMessages conds -> "SendMessages " ^ pp_conditionals conds
    | AlwaysExclusive (opt_loc, msg) ->
        let loc_str =
          match opt_loc with
          | Some loc -> "line " ^ string_of_int loc.lnum
          | None -> ""
        in
        let msg_str = if String.length msg > 0 then ": " ^ msg else "" in
        "AlwaysExclusive (" ^ loc_str ^ msg_str ^ ")"

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

  module PCMStatus = Set.Make (String)

  (* set of pcm_identifiers for which ident is the unit of the PCM *)
  type pcm_status = PCMStatus.t

  let pp_pcm_status ps = String.concat " " (PCMStatus.elements ps)

  type signature =
    (* ComponentSig: comp_params * component_summary *)
    | ComponentSig of (ER.rep ident * typ) list * component_summary
    (* Within a transition, we assign an identifier to all field values, i.e. we
       only treat final writes as proper writes, with all others being "global"
       bindings. This lets us track multiple reads/writes to a field in a
       transition in the same way we track expressions.*)
    | IdentSig of ident_shadow_status * pcm_status * expr_type

  let pp_sig k sgn =
    match sgn with
    | ComponentSig (comp_params, comp_summ) ->
        let ns = "Effect footprint for " ^ k in
        let ps =
          String.concat ", " @@ List.map (fun (i, _) -> get_id i) comp_params
        in
        let cs = pp_summary comp_summ in
        ns ^ "(" ^ ps ^ "): \n" ^ cs
    | IdentSig (_, pcm, et) ->
        let ns = "Signature for " ^ k ^ ": " in
        let is_unit = PCMStatus.cardinal pcm > 0 in
        let pcm_str =
          if is_unit then "PCM unit for: (" ^ pp_pcm_status pcm ^ ")" else ""
        in
        ns ^ pp_expr_type et ^ pcm_str

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

  module type PCM = sig
    val pcm_identifier : string

    val is_applicable_type : typ list -> bool

    val is_unit_literal : expr -> bool

    val is_unit : SAEnv.t -> expr -> bool

    val is_op : expr -> ER.rep ident -> ER.rep ident -> bool

    val is_spurious_conditional_expr' :
      SAEnv.t -> ER.rep ident -> (pattern * expr_annot) list -> bool

    val is_spurious_conditional_stmt' :
      expr_type -> ER.rep ident -> (pattern * stmt_annot list) list -> bool
  end

  let sc_option_check pcm_is_applicable_type x clauses =
    let cond_type = (ER.get_type (get_rep x)).tp in
    let is_integer_option =
      match cond_type with
      | ADT (Ident ("Option", dummy_loc), typs) -> pcm_is_applicable_type typs
      | _ -> false
    in
    let have_two_clauses = List.length clauses = 2 in
    is_integer_option && have_two_clauses

  let sc_get_clauses clauses =
    let detect_clause cls ~f =
      let detected =
        List.filter
          (fun (pattern, cl_erep) ->
            let binders = get_pattern_bounds pattern in
            let matches = f binders in
            matches)
          cls
      in
      if List.length detected > 0 then Some (List.hd detected) else None
    in
    let some_branch =
      detect_clause clauses (fun binders -> List.length binders = 1)
    in
    let none_branch =
      detect_clause clauses (fun binders -> List.length binders = 0)
    in
    (some_branch, none_branch)

  (* Given a match expression, determine whether it is "spurious", i.e. would
     not have to exist if we had monadic operations on option types. This
     function is PCM-specific. *)
  let sc_expr pcm_is_applicable_type pcm_is_unit pcm_is_op senv x clauses =
    let is_integer_option = sc_option_check pcm_is_applicable_type x clauses in
    if is_integer_option then
      let some_branch, none_branch = sc_get_clauses clauses in
      match (some_branch, none_branch) with
      | Some some_branch, Some none_branch ->
          let some_p, (some_expr, _) = some_branch in
          let _, (none_expr, _) = none_branch in
          (* e.g. match (option int) with | Some int => int | None => 0 *)
          let clauses_form_pcm_unit =
            let some_pcm_unit =
              let b = List.hd (get_pattern_bounds some_p) in
              match some_expr with Var q -> equal_id b q | _ -> false
            in
            let none_pcm_unit = pcm_is_unit senv none_expr in
            some_pcm_unit && none_pcm_unit
          in
          (* e.g. match (option int) with | Some int => PCM_op int X | None => X *)
          let clauses_form_pcm_op =
            let none_ident =
              match none_expr with Var q -> Some q | _ -> None
            in
            match none_ident with
            | None -> false
            | Some none_ident ->
                let some_ident = List.hd (get_pattern_bounds some_p) in
                pcm_is_op some_expr some_ident none_ident
          in
          clauses_form_pcm_unit || clauses_form_pcm_op
      | _ -> false
    else false

  let sc_stmt pcm_is_applicable_type pcm_is_unit pcm_is_op xc x
      (clauses : (pattern * stmt_annot list) list) =
    let is_integer_option = sc_option_check pcm_is_applicable_type x clauses in
    if is_integer_option then
      let some_branch, none_branch = sc_get_clauses clauses in
      match (some_branch, none_branch) with
      | Some some_branch, Some none_branch ->
          let some_p, some_stmts = some_branch in
          let _, none_stmts = none_branch in
          (* e.g.
             opt_x <- map[key1][key2];
             match opt_x with
               | Some x => q = PCM_op base diff; map[key1][key2] := q
               | None => map[key1][key2] := diff
             Make sure you check it's map[key1][key2] in both branches! *)
          let ok = List.length none_stmts = 1 && List.length some_stmts = 2 in
          ok
          &&
          let clauses_form_pcm_op =
            (* What is diff? *)
            let none_ident, none_ps =
              let none_stmt, sloc = List.hd none_stmts in
              match none_stmt with
              | MapUpdate (m, klist, Some i) ->
                  (Some i, Pseudofield (m, Some klist))
              (* XXX: this pseudofield is junk; should probably use an option *)
              | _ -> (None, Pseudofield (x, None))
            in

            (* Make sure opt_x is Exactly {map[key1][key2], Linear, }*)
            let cs =
              Contrib.singleton none_ps (LinearContrib, ContribOps.empty)
            in
            let expected_et : expr_type = (Exactly, cs, Cond.empty) in
            let good_et = et_equal xc expected_et in
            good_et
            &&
            (* Make sure the some branch is well-formed *)
            match none_ident with
            | None -> false
            | Some none_ident -> (
                let some_ident = List.hd (get_pattern_bounds some_p) in
                match some_stmts with
                | (Bind (q, (expr, _)), _) :: (st, _) :: _ -> (
                    let is_op = pcm_is_op expr some_ident none_ident in
                    is_op
                    &&
                    match st with
                    | MapUpdate (m, klist, Some i) ->
                        equal_id q i
                        (* f[keys] := q *)
                        && OrderedContribSource.compare none_ps
                             (Pseudofield (m, Some klist))
                           = 0
                    | _ -> false )
                | _ -> false )
          in
          clauses_form_pcm_op
      | _ -> false
    else false

  (* Generic addition PCM for all signed and unsigned types *)
  module Integer_Addition_PCM = struct
    let pcm_identifier = "integer_add"

    (* Can PCM values have this type? *)
    let is_applicable_type typs =
      let is_single = List.compare_length_with typs 1 = 0 in
      if is_single then
        let typ = List.hd typs in
        PrimTypes.is_int_type typ || PrimTypes.is_uint_type typ
      else false

    let is_unit_literal expr =
      match expr with
      | Literal (IntLit l) -> String.equal (PrimTypes.string_of_int_lit l) "0"
      | Literal (UintLit l) -> String.equal (PrimTypes.string_of_uint_lit l) "0"
      | _ -> false

    let is_unit (senv : signature AssocDictionary.dict) expr =
      is_unit_literal expr
      ||
      match expr with
      | Var i -> (
          let opt_isig = SAEnv.lookupS senv (get_id i) in
          match opt_isig with
          | Some (IdentSig (_, pcms, _)) -> PCMStatus.mem pcm_identifier pcms
          | _ -> false )
      | _ -> false

    let is_op expr ida idb =
      match expr with
      | Builtin ((Builtin_add, _), actuals) ->
          let a_uses =
            List.length @@ List.filter (fun k -> equal_id k ida) actuals
          in
          let b_uses =
            List.length @@ List.filter (fun k -> equal_id k idb) actuals
          in
          a_uses = 1 && b_uses = 1
      | _ -> false

    let is_spurious_conditional_expr' = sc_expr is_applicable_type is_unit is_op

    let is_spurious_conditional_stmt' = sc_stmt is_applicable_type is_unit is_op
  end

  let int_add_pcm = (module Integer_Addition_PCM : PCM)

  let enabled_pcms = [ int_add_pcm ]

  let pcm_unit senv expr =
    let unit_of =
      List.filter (fun (module P : PCM) -> P.is_unit senv expr) enabled_pcms
    in
    PCMStatus.of_list
    @@ List.map (fun (module P : PCM) -> P.pcm_identifier) unit_of

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

  let is_spurious_conditional_expr senv x clauses =
    List.exists
      (fun (module P : PCM) -> P.is_spurious_conditional_expr' senv x clauses)
      enabled_pcms

  let is_spurious_conditional_stmt xc x clauses =
    List.exists
      (fun (module P : PCM) -> P.is_spurious_conditional_stmt' xc x clauses)
      enabled_pcms

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
          | IdentSig (ComponentParameter, _, _) -> true
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

  (* Return the parameters actually used to in the given ComponentSig summary *)
  let parameters_in_summary (proc_sig : signature) =
    let parameters_in_set deps =
      List.flatten
      @@ List.map
           (fun k ->
             match k with Pseudofield (f, Some keys) -> keys | _ -> [])
           (Cond.elements deps)
    in
    let parameters_in_op op =
      match op with
      | Read (f, Some keys) -> keys
      | Write ((f, Some keys), _) -> keys
      | ConditionOn deps | EmitEvent deps | SendMessages deps ->
          parameters_in_set deps
      | _ -> []
    in
    match proc_sig with
    | ComponentSig (_, proc_summ) ->
        let params_in_summ =
          List.sort_uniq compare_id @@ List.flatten
          @@ List.map parameters_in_op (ComponentSummary.elements proc_summ)
        in
        pure @@ params_in_summ
    | _ -> fail0 "Sharding analysis: procedure summary is not of the right type"

  let procedure_call_summary senv p (proc_sig : signature) arglist =
    let loc = SR.get_loc (get_rep p) in
    let proc_name = get_id p in
    let implicit_params, _ = List.split @@ SCU.append_implict_comp_params [] in
    let arglist = implicit_params @ arglist in
    match proc_sig with
    | ComponentSig (proc_params, proc_summ) ->
        let proc_params = implicit_params @ fst @@ List.split proc_params in
        let%bind proc_params_in_summary = parameters_in_summary proc_sig in
        let%bind args_in_summary =
          mapM
            ( List.filter (fun k ->
                  match k with Some _ -> true | None -> false)
            @@ List.map2
                 (fun p a ->
                   if List.mem p proc_params_in_summary then Some a else None)
                 proc_params arglist )
            ~f:(fun k -> match k with Some a -> pure @@ a | None -> fail0 "")
        in
        let can_summarise = all_keys_are_parameters senv args_in_summary in
        if can_summarise then
          pure
          @@ ComponentSummary.map
               (fun op -> translate_op op proc_params arglist)
               proc_summ
        else
          pure
          @@ ComponentSummary.singleton
               (AlwaysExclusive (Some loc, "CallProc " ^ proc_name))
    | _ ->
        (* If this occurs, it's a bug. *)
        fail0 "Sharding analysis: procedure summary is not of the right type"

  let get_ident_et senv i =
    let%bind isig = SAEnv.resolvS senv (get_id i) in
    match isig with
    | IdentSig (_, _, c) -> pure @@ c
    (* If this happens, it's a bug *)
    | _ -> fail0 "Sharding analysis: ident does not have a signature"

  let dependencies_of senv i =
    let%bind isig = SAEnv.resolvS senv (get_id i) in
    match isig with
    | IdentSig (_, _, (_, contrs, conds)) ->
        let deps =
          Cond.of_list @@ fst @@ List.split @@ Contrib.bindings contrs
        in
        pure @@ Cond.union deps conds
    | _ ->
        fail0 "Sharding analysis: trying to get dependencies, but no IdentSig"

  (* Add a new identifier to the environment, keeping track of whether it
     shadows a component parameter *)
  let env_new_ident i ?pcms et senv =
    let pcms = match pcms with Some ps -> ps | None -> PCMStatus.empty in
    let id = get_id i in
    let opt_shadowed = SAEnv.lookupS senv id in
    let new_id_sig =
      match opt_shadowed with
      | None -> IdentSig (DoesNotShadow, pcms, et)
      | Some shadowed_sig -> (
          match shadowed_sig with
          | IdentSig (ComponentParameter, pcms, _) ->
              IdentSig (ShadowsComponentParameter, pcms, et)
          | _ -> IdentSig (DoesNotShadow, pcms, et) )
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

  (* TODO: might want to track pcm_status for expressions *)
  (* fp_count keeps track of how many function formal parameters we've encountered *)
  let rec sa_expr senv fp_count (erep : expr_annot) =
    let cont senv' expr = sa_expr senv' fp_count expr in
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
        let spurious = is_spurious_conditional_expr senv x clauses in
        let _, xcontr, xcond = xc in
        let clause_et (pattern, cl_expr) =
          let binders = get_pattern_bounds pattern in
          let senv' =
            List.fold_left
              (* Each binder in the pattern gets the full contributions of x *)
                (fun env_acc id -> env_new_ident id xc env_acc)
              senv binders
          in
          cont senv' cl_expr
        in
        let%bind cl_ets = mapM clause_et clauses in
        let cl_pss, cl_contribs, cl_conds = et_list_split cl_ets in
        let fc = List.hd cl_contribs in
        let all_equal = List.for_all (fun c -> c = fc) cl_contribs in
        let cl_ps = List.fold_left min_precision Exactly cl_pss in
        (* The precision of the match is: Exactly if all clauses are equal and
           precise OR the conditional is spurious, otherwise SubsetOf *)
        let match_ps =
          min_precision
            (if all_equal || spurious then Exactly else SubsetOf)
            cl_ps
        in
        (* The match's contributions is the LUB of the clauses *)
        let match_contribs =
          List.fold_left contrib_upper_bound Contrib.empty cl_contribs
        in
        let match_cond =
          let this_cond =
            if spurious then Cond.empty
            else Cond.of_list @@ fst @@ List.split @@ Contrib.bindings xcontr
          in

          let this_cond = Cond.union this_cond xcond in
          List.fold_left Cond.union this_cond cl_conds
        in
        pure @@ (match_ps, match_contribs, match_cond)
    | Let (i, _, lhs, rhs) ->
        (* We are introducing a new binder, so LHS is a new scope *)
        let%bind lhs_et = sa_expr senv 0 lhs in
        let senv' = env_new_ident i lhs_et senv in
        cont senv' rhs
    (* Our expr_types do not depend on Scilla types; just analyse as if monomorphic *)
    | TFun (_, body) -> cont senv body
    | TApp (tf, _) -> get_ident_et senv tf
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
          | IdentSig (_, _, c) -> pure @@ c
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
    | _ -> fail0 @@ "Sharding analysis: unsupported instruction " ^ pp_expr e

  let sa_expr_wrapper senv erep = sa_expr senv 0 erep

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
              else
                AlwaysExclusive
                  ( Some (ER.get_loc (get_rep m)),
                    pp_operation (Read (m, Some klist)) )
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
              else
                AlwaysExclusive
                  ( Some (ER.get_loc (get_rep m)),
                    pp_operation (Write ((m, Some klist), ic)) )
            in
            cont_op op summary sts
        | AcceptPayment -> cont_op AcceptMoney summary sts
        | SendMsgs i ->
            let%bind deps = dependencies_of senv i in
            cont_op (SendMessages deps) summary sts
        | CreateEvnt i ->
            let%bind deps = dependencies_of senv i in
            cont_op (EmitEvent deps) summary sts
        (* TODO: Do we want to track blockchain reads? *)
        | ReadFromBC (x, _) -> cont_ident x et_top summary sts
        | Bind (x, expr) ->
            let%bind expr_contrib = sa_expr_wrapper senv expr in
            cont_ident x expr_contrib summary sts
        | MatchStmt (x, clauses) ->
            let%bind xc = get_ident_et senv x in
            let spurious = is_spurious_conditional_stmt xc x clauses in
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

            (* Unusual case: match does not express "real" control flow, but
               monadic operation on option; in this case, the summary is the
               summary of the Some branch *)
            if spurious then
              let is_some (pattern, _) =
                List.length (get_pattern_bounds pattern) = 1
              in
              let%bind some_summary =
                mapM summarise_clause (List.filter is_some clauses)
              in
              let%bind summary' =
                foldM
                  (fun acc_summ cl_summ ->
                    pure @@ ComponentSummary.union acc_summ cl_summ)
                  summary some_summary
              in
              cont senv summary' sts (* This is the normal case *)
            else
              let _, xcontr, xcond = xc in
              let match_conds =
                let this_cond =
                  Cond.of_list @@ fst @@ List.split @@ Contrib.bindings xcontr
                in
                Cond.union this_cond xcond
              in
              let conds_op = ConditionOn match_conds in
              let%bind cl_summaries = mapM summarise_clause clauses in
              let summary_with_conds =
                (* It's possible (and fine) to condition on constants or blockchain reads *)
                if Cond.cardinal match_conds > 0 then
                  ComponentSummary.add conds_op summary
                else summary
              in
              (* TODO: LUB *)
              let%bind summary' =
                foldM
                  (fun acc_summ cl_summ ->
                    pure @@ ComponentSummary.union acc_summ cl_summ)
                  summary_with_conds cl_summaries
              in
              cont senv summary' sts
        | CallProc (p, arglist) -> (
            let opt_proc_sig = SAEnv.lookupS senv (get_id p) in
            match opt_proc_sig with
            | Some proc_sig ->
                let%bind call_summ =
                  procedure_call_summary senv p proc_sig arglist
                in
                let summary' = ComponentSummary.union summary call_summ in
                cont senv summary' sts
            (* If this occurs, it's a bug. Type checking should prevent it. *)
            | _ ->
                fail1
                  "Sharding analysis: calling procedure that was not analysed"
                  (SR.get_loc (get_rep p)) )
        | _ -> fail0 @@ "Sharding analysis: unsupported statement " ^ pp_stmt s
        )

  let sa_component_summary senv (comp : component) =
    let all_params = SCU.append_implict_comp_params comp.comp_params in
    (* Add component parameters to the analysis environment *)
    let senv' =
      env_bind_ident_list senv
        (List.map (fun (i, _) -> i) all_params)
        (IdentSig (ComponentParameter, PCMStatus.empty, et_top))
    in
    sa_stmt senv' ComponentSummary.empty comp.comp_body

  let sa_libentries senv (lel : lib_entry list) =
    foldM
      ~f:(fun senv le ->
        match le with
        | LibVar (lname, _, lexp) ->
            let%bind esig = sa_expr_wrapper senv lexp in
            let e, rep = lexp in
            let pcms = pcm_unit senv e in
            pure
            @@ SAEnv.addS senv (get_id lname)
                 (IdentSig (DoesNotShadow, pcms, esig))
        | LibTyp _ -> pure senv)
      ~init:senv lel

  let sa_module (cmod : cmodule) (elibs : libtree list) =
    (* Stage 1: determine state footprint of components *)
    let senv = SAEnv.mk () in

    (* Analyze external libraries  *)
    let%bind senv =
      let rec recurser libl =
        foldM
          ~f:(fun senv lib ->
            let%bind senv_deps = recurser lib.deps in
            let%bind senv_lib = sa_libentries senv_deps lib.libn.lentries in
            (* Retain only _this_ library's entries in env, not the deps' *)
            let senv_lib' =
              SAEnv.filterS senv_lib ~f:(fun name ->
                  List.exists
                    (function
                      | LibTyp _ -> false | LibVar (i, _, _) -> get_id i = name)
                    lib.libn.lentries)
            in
            pure @@ SAEnv.appendS senv senv_lib')
          ~init:senv libl
      in
      recurser elibs
    in

    (* Analyze contract libraries *)
    let%bind senv =
      match cmod.libs with
      | Some l -> sa_libentries senv l.lentries
      | None -> pure @@ senv
    in

    (* Bind contract parameters *)
    let senv =
      let prs, _ = List.split cmod.contr.cparams in
      env_bind_ident_list senv prs
        (IdentSig (DoesNotShadow, PCMStatus.empty, et_top))
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
