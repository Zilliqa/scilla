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

  type de_bruijn_level = int

  type arg_index = int

  type contrib_source =
    | UnknownSource
    | Pseudofield of pseudofield
    (* When analysing pure functions, we describe their output's contributions
       in terms of how the function's formal parameters flow into the output *)
    | FormalParameter of de_bruijn_level
    | ProcParameter of arg_index

  type contrib_cardinality = NoContrib | LinearContrib | NonLinearContrib

  type contrib_op = BuiltinOp of builtin | Conditional

  let pp_contrib_source cs =
    match cs with
    | UnknownSource -> "unknown_source"
    | Pseudofield (f, opt_keys) -> pp_pseudofield f opt_keys
    | FormalParameter i -> "_" ^ string_of_int i
    | ProcParameter i -> "_p" ^ string_of_int i

  let pp_contrib_op co =
    match co with BuiltinOp blt -> pp_builtin blt | Conditional -> "?cond"

  module OrderedContribOp = struct
    type t = contrib_op

    let compare a b = compare (pp_contrib_op a) (pp_contrib_op b)
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

  (* How precise are we about the keys in contributions? *)
  (* Exactly: these are exactly the contributions in the result *)
  (* SubsetOf: the result has a subset of these contributions *)
  type source_precision = Exactly | SubsetOf

  type known_contrib = source_precision * contributions

  type expr_type =
    (* "I give up" type, for things we cannot analyse *)
    | EUnknown
    (* Known contribution *)
    | EVal of known_contrib
    (* Transformation of contributions, i.e. derived contributions *)
    | EOp of contrib_op * expr_type
    | EComposeSequence of expr_type list
    (* conditional * clauses *)
    | EComposeParallel of expr_type * expr_type list
    | EFun of efun_desc
    | EApp of efun_desc * expr_type list

  (* We get away with not giving expr_types to the parameters because the Scilla
     type checker guarantees EApp will always match what we expect. *)
  and efun_desc = EFunDef of de_bruijn_level list * efun_def

  (* Either something that can be evaluated, or an unknown (parameter of a HO
     function or of a procedure). If the latter, we store _which_ parameter we
     are, so when EApp is used, we can replace the definition with the concrete
     one. *)
  and efun_def =
    | DefExpr of expr_type
    | DefFormalParameter of de_bruijn_level
    | DefProcParameter of arg_index

  let et_nothing = EVal (Exactly, Contrib.empty)

  (**  Helper functions  **)
  let min_precision ua ub =
    match (ua, ub) with Exactly, Exactly -> Exactly | _ -> SubsetOf

  let pp_contrib_cardinality cc =
    match cc with
    | NoContrib -> "None"
    | LinearContrib -> "Linear"
    | NonLinearContrib -> "NonLinear"

  let max_contrib_card a b =
    match (a, b) with
    | NoContrib, NoContrib -> NoContrib
    | NonLinearContrib, _ -> NonLinearContrib
    | _, NonLinearContrib -> NonLinearContrib
    | _ -> LinearContrib

  let product_contrib_card a b =
    match (a, b) with
    | NoContrib, _ | _, NoContrib -> NoContrib
    | NonLinearContrib, _ | _, NonLinearContrib -> NonLinearContrib
    | LinearContrib, LinearContrib -> LinearContrib

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

  let pp_known_contrib (ps, cs) = pp_precision ps ^ " " ^ pp_contribs cs

  let rec pp_expr_type et =
    match et with
    | EUnknown -> "EUnknown"
    | EVal kc -> "EVal " ^ pp_known_contrib kc
    | EOp (op, et) ->
        Printf.sprintf "EOp({%s}, {%s})" (pp_contrib_op op) (pp_expr_type et)
    | EComposeSequence etl ->
        Printf.sprintf "EComposeSeq(%s)" (pp_expr_type_list ~sep:" ;; " etl)
    | EComposeParallel (c, etl) ->
        Printf.sprintf "EComposePar(%s ~~ %s)" (pp_expr_type c)
          (pp_expr_type_list ~sep:" || " etl)
    | EFun fd -> pp_efun_desc fd
    | EApp (eref, etl) ->
        Printf.sprintf "(EApp (%s) @@ %s)" (pp_efun_desc eref)
          (pp_expr_type_list ~sep:" @@ " etl)

  and pp_expr_type_list ?(sep = ", ") etl =
    String.concat sep (List.map pp_expr_type etl)

  and pp_efun_desc d =
    let pp_efun_def def =
      match def with
      | DefExpr et -> pp_expr_type et
      | DefFormalParameter i ->
          "DefParam " ^ pp_contrib_source (FormalParameter i)
      | DefProcParameter i -> "DefParam " ^ pp_contrib_source (ProcParameter i)
    in

    match d with
    | EFunDef (lvls, def) ->
        let fargs =
          String.concat ", " (List.map (fun i -> "_" ^ string_of_int i) lvls)
        in
        let ds = pp_efun_def def in
        Printf.sprintf "EFun(%s) = %s" fargs ds

  (* Type normalisation *)
  let et_is_val et = match et with EVal _ -> true | _ -> false

  let et_is_known_fun et =
    match et with EFun (EFunDef (_, DefExpr _)) -> true | _ -> false

  let combine_seq (carda, opsa) (cardb, opsb) =
    (* This is NOT max(carda, cardb), but addition *)
    let card =
      match (carda, cardb) with
      (* Special cases for no contributions *)
      | NoContrib, b -> b
      | a, NoContrib -> a
      (* Any seq combination of contributions is NonLinear *)
      | _ -> NonLinearContrib
    in
    let ops = ContribOps.union opsa opsb in
    (card, ops)

  let combine_par (carda, opsa) (cardb, opsb) =
    let card = max_contrib_card carda cardb in
    let ops = ContribOps.union opsa opsb in
    (card, ops)

  let combine_product (carda, opsa) (cardb, opsb) =
    let card = product_contrib_card carda cardb in
    let ops = ContribOps.union opsa opsb in
    (* ?cond is only allowable op if card = NoContrib *)
    let ops =
      match card with
      | NoContrib -> ContribOps.inter ops (ContribOps.singleton Conditional)
      | _ -> ops
    in
    (card, ops)

  (* Only works on EVals *)
  let et_compose f eta etb =
    match (eta, etb) with
    | EVal (psa, contra), EVal (psb, contrb) ->
        let ps' = min_precision psa psb in
        let contr' = Contrib.union f contra contrb in
        pure @@ EVal (ps', contr')
    (* This shouldn't happen *)
    | _ -> fail0 "Sharding analysis: trying to et_compose non-EVal types"

  let et_seq_compose = et_compose (fun cs a b -> Some (combine_seq a b))

  let et_par_compose = et_compose (fun cs a b -> Some (combine_par a b))

  (* WARNING: only guaranteed accurate for fully normalised types *)
  let et_equal eta etb =
    match (eta, etb) with
    | EVal (psa, contra), EVal (psb, contrb) ->
        let ps_eq = psa = psb in
        let contr_eq =
          Contrib.equal
            (fun (cca, copa) (ccb, copb) ->
              cca = ccb && ContribOps.equal copa copb)
            contra contrb
        in
        ps_eq && contr_eq
    (* Give it our best shot *)
    | _ -> String.compare (pp_expr_type eta) (pp_expr_type etb) = 0

  (* For all the contributions in etc, add cond Op in et *)
  let add_conditional etc et =
    (* Convention that must be respected by sa_expr *)
    let spurious = et_equal etc et_nothing in
    match (etc, et) with
    | EVal (_, ccontr), EVal (ps, contr) ->
        let ps' = if spurious then Exactly else ps in
        (* Some (cc, ContribOps.add Conditional cops) *)
        let contr' =
          Contrib.merge
            (fun cs cond contr ->
              match (cond, contr) with
              | None, None -> None
              | None, Some csumm -> Some csumm
              (* contribution_source in conditional, but not contribution *)
              | Some _, None ->
                  (* Have to store it anyway, but mark as not contributing *)
                  Some (NoContrib, ContribOps.singleton Conditional)
              (* contribution_source in both cond and contr *)
              | Some _, Some (cc, cops) ->
                  Some (cc, ContribOps.add Conditional cops))
            ccontr contr
        in
        pure @@ EVal (ps', contr')
    | _ -> fail0 "Sharding analysis: add_conditional non-EVal type"

  let rec et_normalise (et : expr_type) =
    match et with
    (* Nothing to do *)
    | EUnknown | EVal _ -> pure @@ et
    | EOp (op, expr) -> (
        let%bind nexpr = et_normalise expr in
        match nexpr with
        | EVal (ps, contrs) ->
            (* Distribute operation over contributions *)
            let contrs' =
              Contrib.map
                (fun (cc, cops) -> (cc, ContribOps.add op cops))
                contrs
            in
            let kc = (ps, contrs') in
            pure @@ EVal kc
        (* Cannot perform operation, but normalisation might simplify expr *)
        | _ -> pure @@ EOp (op, nexpr) )
    | EComposeSequence etl ->
        let%bind netl = mapM et_normalise etl in
        let all_vals = List.for_all et_is_val netl in
        if all_vals then foldM et_seq_compose et_nothing netl
        else pure @@ EComposeSequence netl
    | EComposeParallel (xet, cl_etl) ->
        let%bind nxet = et_normalise xet in
        let%bind ncl_etl = mapM et_normalise cl_etl in
        let all_vals = et_is_val nxet && List.for_all et_is_val ncl_etl in
        if all_vals then
          (* Don't want to mix et_nothing in and lose precision *)
          (* Guaranteed to have at least one clauses by the typechecker *)
          let fc = List.hd ncl_etl in
          let%bind cl_et = foldM et_par_compose fc ncl_etl in
          add_conditional nxet cl_et
        else pure @@ EComposeParallel (nxet, ncl_etl)
    (* Normalise within function bodies *)
    | EFun (EFunDef (dbl, DefExpr expr)) ->
        let%bind nexpr = et_normalise expr in
        pure @@ EFun (EFunDef (dbl, DefExpr nexpr))
    (* Cannot normalise unknown functions *)
    | EFun (EFunDef (_, DefFormalParameter _)) -> pure @@ et
    | EFun (EFunDef (_, DefProcParameter _)) -> pure @@ et
    (* Normalise when EApp referent is known *)
    | EApp (EFunDef (dbls, DefExpr fde), etl) ->
        let%bind nfde = et_normalise fde in
        let%bind netl = mapM et_normalise etl in
        (* All arguments are known *)
        let all_known =
          List.for_all et_is_val netl || List.for_all et_is_known_fun netl
        in
        if all_known then
          (* There is a mismatch between Fun and App. Fun takes a single
             parameter, whereas App takes multiple arguments, i.e. length dbl = 1.
             As such, we apply arguments one by one *)
          let rec subst pid netl nfden =
            let arg_et = List.hd netl in
            let%bind nfde = substitute_argument pid arg_et nfden in
            (* TODO: is this necessary? *)
            (* let%bind nfde = et_normalise nfde in *)
            match netl with
            (* This was our last argument *)
            | [ _ ] -> pure @@ nfde
            (* More arguments exist, we have more substitution to do *)
            | _ :: rem_netl ->
                let%bind next = pid_next pid in
                subst next rem_netl nfde
            (* This can't happen *)
            | _ -> fail0 "Sharding analysis: EApp argument list is empty??"
          in
          (* Scilla type checking guarantees this is safe *)
          let dbl = List.hd dbls in
          let%bind substituted = subst (FormalParameter dbl) netl nfde in
          et_normalise substituted
        else pure @@ EApp (EFunDef (dbls, DefExpr nfde), netl)
    | EApp (EFunDef (_, DefFormalParameter _), etl) -> pure @@ et
    | EApp (EFunDef (_, DefProcParameter _), etl) -> pure @@ et

  (* Parameter identifier *)
  and pid_idx_eq idx cs =
    match cs with
    | FormalParameter i -> idx = i
    | ProcParameter i -> idx = i
    | _ -> false

  and pid_next cs =
    match cs with
    | FormalParameter i -> pure @@ FormalParameter (i + 1)
    | ProcParameter i -> pure @@ ProcParameter (i + 1)
    | _ -> fail0 "Sharding analysis: wrong argument to pid_next"

  and substitute_argument pid this in_this =
    let cont et = substitute_argument pid this et in
    let%bind result =
      match in_this with
      | EUnknown -> pure @@ EUnknown
      | EVal (fd_ps, fd_contr) -> (
          (* Combine parameters's contributions with those of the argument, i.e.
             arg_contr. *)
          match this with
          | EVal (arg_ps, arg_contr) -> (
              let opt_fpc = Contrib.find_opt pid fd_contr in
              match opt_fpc with
              | Some fpc ->
                  let ps = min_precision arg_ps fd_ps in
                  (* Compute contributions due to the argument *)
                  let acontr =
                    Contrib.map (fun c -> combine_product c fpc) arg_contr
                  in
                  (* Add them to fd_contr instead of old value *)
                  let fd_contr = Contrib.remove pid fd_contr in
                  let contr =
                    Contrib.union
                      (fun cs a b -> Some (combine_seq a b))
                      acontr fd_contr
                  in
                  pure @@ EVal (ps, contr)
                  (* If the formal parameter is not used, argument is not used,
                     i.e. nothing changes *)
              | None -> pure @@ in_this )
          | _ -> pure @@ in_this )
      | EOp (op, expr) ->
          let%bind sexpr = cont expr in
          pure @@ EOp (op, sexpr)
      | EComposeSequence etl ->
          let%bind setl = mapM cont etl in
          pure @@ EComposeSequence setl
      | EComposeParallel (c, etl) ->
          let%bind sc = cont c in
          let%bind setl = mapM cont etl in
          pure @@ EComposeParallel (sc, setl)
      | EFun (EFunDef (dbl, DefExpr fd)) ->
          (* Functions are well-scoped, so we can safely substitute inside
             their definitions, i.e. the nesting protects us from mistakes. *)
          let%bind sfd = cont fd in
          (* After substituting the formal parameter, we only return the body *)
          pure @@ sfd
      (* This substitutes returned functions *)
      | EFun (EFunDef (_, DefFormalParameter i))
      | EFun (EFunDef (_, DefProcParameter i)) ->
          if pid_idx_eq i pid then
            match this with
            (* TODO: should do this only for DefExpr? *)
            | EFun _ -> pure @@ this
            | _ -> fail0 "Sharding analysis: substituting non-EFun into EFun"
          else
            (* Not the function we're looking for; nothing to do *)
            pure @@ in_this
      (* Substitute function applications *)
      | EApp (EFunDef (fbl, DefFormalParameter i), etl) ->
          let%bind setl = mapM cont etl in
          if pid_idx_eq i pid then
            match this with
            | EFun fd -> pure @@ EApp (fd, setl)
            | _ -> fail0 "Sharding analysis: substituting non-EFun into EFun"
          else pure @@ EApp (EFunDef (fbl, DefFormalParameter i), setl)
      | EApp (EFunDef (dbl, DefProcParameter i), etl) ->
          let%bind setl = mapM cont etl in
          if pid_idx_eq i pid then
            match this with
            | EFun fd -> pure @@ EApp (fd, setl)
            | _ -> fail0 "Sharding analysis: substituting non-EFun into EFun"
          else pure @@ EApp (EFunDef (dbl, DefProcParameter i), setl)
      | EApp (EFunDef (dbl, DefExpr expr), etl) ->
          let%bind setl = mapM cont etl in
          pure @@ EApp (EFunDef (dbl, DefExpr expr), setl)
    in
    pure @@ result

  (* For each contract component, we keep track of the operations it performs.
     This gives us enough information to tell whether two transitions have disjoint
     footprints and thus commute. *)
  type component_operation =
    (* Read of cfield, with map keys which are comp_params if field is a map *)
    | Read of pseudofield
    | Write of pseudofield * expr_type
    | AcceptMoney
    | ConditionOn of expr_type
    | EmitEvent of expr_type
    | SendMessages of expr_type
    (* Top element -- in case of ambiguity, be conservative *)
    | AlwaysExclusive of ErrorUtils.loc option * string

  let pp_operation op =
    match op with
    | Read (field, opt_keys) -> "Read " ^ pp_pseudofield field opt_keys
    | Write ((field, opt_keys), kc) ->
        "Write " ^ pp_pseudofield field opt_keys ^ " (" ^ pp_expr_type kc ^ ")"
    | AcceptMoney -> "AcceptMoney"
    | ConditionOn kc -> "ConditionOn " ^ pp_expr_type kc
    | EmitEvent kc -> "EmitEvent " ^ pp_expr_type kc
    | SendMessages kc -> "SendMessages " ^ pp_expr_type kc
    | AlwaysExclusive (opt_loc, msg) ->
        let loc_str =
          match opt_loc with
          | Some loc -> "line " ^ string_of_int loc.lnum
          | None -> ""
        in
        let msg_str =
          (if String.length loc_str > 0 then ": " else "")
          ^ if String.length msg > 0 then msg else ""
        in
        "AlwaysExclusive (" ^ loc_str ^ msg_str ^ ")"

  module OrderedComponentOperation = struct
    type t = component_operation

    (* XXX: This is super hacky, but works *)
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

  (* BEGIN functions to do with detecting spurious match exprs/stmts  *)
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
               | Some x => q = PCM_op x diff; map[key1][key2] := q
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
            let expected_et = EVal (Exactly, cs) in
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

  (* END functions to do with detecting spurious match exprs/stmts  *)

  (* TODO: move PCMs into a separate file *)
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

  let is_spurious_conditional_expr senv x clauses =
    List.exists
      (fun (module P : PCM) -> P.is_spurious_conditional_expr' senv x clauses)
      enabled_pcms

  let is_spurious_conditional_stmt xc x clauses =
    List.exists
      (fun (module P : PCM) -> P.is_spurious_conditional_stmt' xc x clauses)
      enabled_pcms

  let env_bind_component senv comp (sgn : signature) =
    let i = comp.comp_name in
    SAEnv.addS senv (get_id i) sgn

  let env_bind_ident_map senv idlist sgn =
    let idlist = List.mapi (fun i k -> (i, k)) idlist in
    List.fold_left
      (fun acc_senv (idx, (i, t)) ->
        SAEnv.addS acc_senv (get_id i) (sgn idx i t))
      senv idlist

  let contrib_pseudofield (f, opt_keys) =
    let csumm = (LinearContrib, ContribOps.empty) in
    let csrc = Pseudofield (f, opt_keys) in
    Contrib.singleton csrc csumm

  let et_pseudofield (f, opt_keys) = (Exactly, contrib_pseudofield (f, opt_keys))

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

  let rec et_field_keys et =
    let remove_dups = List.sort_uniq compare_id in
    match et with
    | EUnknown -> []
    | EVal (ps, contr) ->
        let contrib_sources = fst @@ List.split @@ Contrib.bindings contr in
        let res =
          remove_dups @@ List.flatten
          @@ List.map
               (fun cs ->
                 match cs with Pseudofield (f, Some keys) -> keys | _ -> [])
               contrib_sources
        in
        res
    | EOp (op, expr) -> remove_dups @@ et_field_keys et
    | EComposeSequence etl ->
        remove_dups @@ List.flatten @@ List.map et_field_keys etl
    | EComposeParallel (ec, etl) ->
        remove_dups @@ List.flatten
        @@ (et_field_keys ec :: List.map et_field_keys etl)
    | EFun (EFunDef (_, DefExpr fd)) -> remove_dups @@ et_field_keys fd
    | EApp (EFunDef (_, DefExpr fd), etl) ->
        remove_dups @@ List.flatten
        @@ (et_field_keys fd :: List.map et_field_keys etl)
    | EApp (EFunDef (_, (DefFormalParameter _ | DefProcParameter _)), etl) ->
        remove_dups @@ List.flatten @@ List.map et_field_keys etl
    | EFun (EFunDef (_, (DefFormalParameter _ | DefProcParameter _))) -> []

  let int_range a b =
    let rec int_range_rec l a b =
      if a > b then l else int_range_rec (b :: l) a (b - 1)
    in
    int_range_rec [] a b

  let is_fun t = match t with FunType _ -> true | _ -> false

  let rec count_arrows t =
    match t with FunType (arg, ret) -> 1 + count_arrows ret | _ -> 0

  (* Get the expr_type to assign to function formal parameters *)
  let get_fp_et fp_count fp_typ =
    let primitive = not (is_fun fp_typ) in
    if primitive then
      EVal
        ( Exactly,
          Contrib.singleton (FormalParameter fp_count)
            (LinearContrib, ContribOps.empty) )
    else
      let nargs = count_arrows fp_typ in
      let fps = int_range 0 (nargs - 1) in
      (* See comment attached to efun_desc explaining why we don't need to do
         this recursively if fp takes functions as parameters as well *)
      EFun (EFunDef (fps, DefFormalParameter fp_count))

  (* Get the expr_type to assign to procedure parameters *)
  let get_pp_et arg_idx typ =
    if is_fun typ then
      let nargs = count_arrows typ in
      let fps = int_range 0 (nargs - 1) in
      EFun (EFunDef (fps, DefProcParameter arg_idx))
    else
      EVal
        ( Exactly,
          Contrib.singleton (ProcParameter arg_idx)
            (LinearContrib, ContribOps.empty) )

  (* Return the parameters actually used to in the given ComponentSig summary *)
  let idents_used_as_map_keys (proc_sig : signature) =
    let idents_in_op op =
      match op with
      | Read (f, Some keys) -> keys
      | Write ((f, Some keys), et) -> keys @ et_field_keys et
      | Write ((_, None), et) | EmitEvent et | SendMessages et | ConditionOn et
        ->
          et_field_keys et
      | Read (_, None) | AcceptMoney | AlwaysExclusive (_, _) -> []
    in
    match proc_sig with
    | ComponentSig (_, proc_summ) ->
        let idents_in_summ =
          List.sort_uniq compare_id @@ List.flatten
          @@ List.map idents_in_op (ComponentSummary.elements proc_summ)
        in
        pure @@ idents_in_summ
    | _ -> fail0 "Sharding analysis: procedure summary is not of the right type"

  let rec translate_et_field_keys et key_mapping =
    let map_keys keys =
      List.map (fun k -> List.assoc (get_id k) key_mapping) keys
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
    let cont expr = translate_et_field_keys expr key_mapping in
    match et with
    | EUnknown -> EUnknown
    | EVal (ps, contr) -> EVal (ps, translate_comp_contribs contr)
    | EOp (op, expr) -> EOp (op, cont expr)
    | EComposeSequence etl ->
        let setl = List.map cont etl in
        EComposeSequence setl
    | EComposeParallel (ec, etl) ->
        let setl = List.map cont etl in
        EComposeParallel (cont ec, setl)
    | EFun (EFunDef (dbl, DefExpr fd)) ->
        EFun (EFunDef (dbl, DefExpr (cont fd)))
    | EApp (EFunDef (dbl, DefExpr fd), etl) ->
        let setl = List.map cont etl in
        EApp (EFunDef (dbl, DefExpr (cont fd)), setl)
    | _ -> et

  let et_can_be_summarised senv et =
    let can_summarise c =
      match c with
      | Pseudofield (m, Some klist) -> map_access_can_be_summarised senv m klist
      | Pseudofield (f, None) -> true
      | UnknownSource | FormalParameter _ | ProcParameter _ -> true
    in
    match et with
    | EVal (_, kc) ->
        let c_keys, _ = List.split @@ Contrib.bindings kc in
        List.for_all can_summarise c_keys
    | _ -> false

  (* Rewrite an operation written in terms of proc_params into an operation
     written in terms of call_args. We need to rewrite both the map keys
     and the expr_types in the operation. *)
  let translate_op op proc_params call_args arg_ets =
    let pp_names, _ =
      List.split @@ List.map (fun (i, t) -> (get_id i, t)) proc_params
    in
    let pp_css = List.mapi (fun i _ -> ProcParameter i) proc_params in
    let key_mapping = List.combine pp_names call_args in
    let cs_to_et_mapping = List.combine pp_css arg_ets in
    (* The assoc will fail only if there's a bug *)
    let map_keys keys =
      List.map (fun k -> List.assoc (get_id k) key_mapping) keys
    in
    (* FIXME TODO: make this more efficient!! *)
    let translate_comp_et et =
      let substituted =
        foldM
          (fun in_this (pid, this) -> substitute_argument pid this in_this)
          et cs_to_et_mapping
      in
      match substituted with
      | Ok sub -> (
          let res = et_normalise sub in
          match res with
          | Ok r ->
              (* We need to rewrite field keys in the expr_type as well! *)
              translate_et_field_keys r key_mapping
          | Error _ -> et )
      | Error x -> et
    in

    (* TODO: currently, if the translated_comp_et is et_nothing, the operation
       still exists. This is slightly annoying, e.g. for ConditionOn *)
    match op with
    | Read (f, Some keys) -> Read (f, Some (map_keys keys))
    | Write ((f, None), et) -> Write ((f, None), translate_comp_et et)
    | Write ((f, Some keys), et) ->
        Write ((f, Some (map_keys keys)), translate_comp_et et)
    | ConditionOn et -> ConditionOn (translate_comp_et et)
    | EmitEvent et -> EmitEvent (translate_comp_et et)
    | SendMessages et -> SendMessages (translate_comp_et et)
    | Read (_, None) | AcceptMoney | AlwaysExclusive _ -> op

  let procedure_call_summary senv p (proc_sig : signature) arglist arg_ets =
    let implicit_params = SCU.append_implict_comp_params [] in
    let ip_vals, _ = List.split implicit_params in
    (* Scilla typechecker ensures arg types are correct; we don't need them *)
    let arglist = ip_vals @ arglist in
    let implicit_ets =
      List.mapi (fun idx (ident, typ) -> get_pp_et idx typ) implicit_params
    in
    let arg_ets = implicit_ets @ arg_ets in
    match proc_sig with
    | ComponentSig (proc_params, proc_summ) ->
        let proc_params = implicit_params @ proc_params in
        let pp_vals, _ = List.split @@ proc_params in
        (* To summarise the procedure call: all idents used as map keys in
           the called procedure MUST be parameters of the caller component *)
        let%bind idents_used_as_keys = idents_used_as_map_keys proc_sig in
        (* Does the called procedure use non-parameters as map keys? If yes,
           then we can't summarise it. I believe this check is redundant given the
           et_can_be_summarised check in MatchStmt, but better safe than sorry.*)
        let exist_non_params_used_as_keys =
          List.exists
            (fun i -> not @@ List.exists (fun q -> compare_id i q = 0) pp_vals)
            idents_used_as_keys
        in
        let%bind args_used_as_keys =
          (* XXX: Is there a way to do this that's easier to read? *)
          mapM
            ( List.filter (fun k ->
                  match k with Some _ -> true | None -> false)
            (* If the parameter (p) is used, the corresponding argument (a) is used *)
            @@ List.map2
                 (fun p a ->
                   (* Be very careful with List.mem and such -- they don't work with idents *)
                   if
                     List.exists
                       (fun q -> compare_id p q = 0)
                       idents_used_as_keys
                   then Some a
                   else None)
                 pp_vals arglist )
            (* This can't fail *)
            ~f:(fun k -> match k with Some a -> pure @@ a | None -> fail0 "")
        in
        let can_summarise =
          (not exist_non_params_used_as_keys)
          && all_keys_are_parameters senv args_used_as_keys
        in
        if can_summarise then
          pure
          @@ ComponentSummary.map
               (fun op -> translate_op op proc_params arglist arg_ets)
               proc_summ
        else
          let loc = SR.get_loc (get_rep p) in
          let proc_name = get_id p in
          pure
          @@ ComponentSummary.singleton
               (AlwaysExclusive (Some loc, "CallProc " ^ proc_name))
    | _ -> fail0 "Sharding analysis: procedure summary is not of the right type"

  let get_ident_et senv i =
    let%bind isig = SAEnv.resolvS senv (get_id i) in
    match isig with
    | IdentSig (_, _, c) -> pure @@ c
    (* If this happens, it's a bug *)
    | _ -> fail0 "Sharding analysis: ident does not have a signature"

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

  let get_fun_sig senv f =
    let%bind fs = SAEnv.resolvS senv (get_id f) in
    match fs with
    | IdentSig (_, _, c) -> pure @@ c
    | _ ->
        fail0
          (Printf.sprintf
             "Sharding analysis: applied function %s does not have IdentSig"
             (get_id f))

  let get_eapp_referent senv f =
    let%bind et = get_fun_sig senv f in
    match et with
    | EFun fdesc -> pure @@ fdesc
    | _ ->
        fail0
          (Printf.sprintf
             "Sharding analysis: applied function %s does not have EFun type"
             (get_id f))

  (* TODO: might want to track pcm_status for expressions *)
  (* fp_count keeps track of how many function formal parameters we've encountered *)
  let rec sa_expr senv fp_count (erep : expr_annot) =
    let cont senv expr = sa_expr senv fp_count expr in
    let e, rep = erep in
    match e with
    | Literal l -> pure @@ et_nothing
    | Var i ->
        let%bind ic = get_ident_et senv i in
        pure @@ ic
    | Builtin ((b, _), actuals) ->
        let%bind arg_ets = mapM actuals ~f:(fun i -> get_ident_et senv i) in
        pure @@ EOp (BuiltinOp b, EComposeSequence arg_ets)
    | Message bs ->
        let get_payload_et pld =
          match pld with
          | MLit l -> pure @@ et_nothing
          | MVar i -> get_ident_et senv i
        in
        (* Ignore labels, just analyse payloads *)
        let _, plds = List.split bs in
        let%bind pld_ets = mapM get_payload_et plds in
        (* Don't really care about linearity for msgs, but Par fits better than Seq *)
        pure @@ EComposeParallel (et_nothing, pld_ets)
    | Constr (cname, _, actuals) ->
        let%bind arg_ets = mapM actuals ~f:(fun i -> get_ident_et senv i) in
        pure @@ EComposeSequence arg_ets
    | Let (i, _, lhs, rhs) ->
        let%bind lhs_et = cont senv lhs in
        let%bind lhs_et = et_normalise lhs_et in
        let senv' = env_new_ident i lhs_et senv in
        cont senv' rhs
    (* Our expr_types do not depend on Scilla types; just analyse as if
       monomorphic *)
    | TFun (_, body) -> cont senv body
    | TApp (tf, _) -> get_ident_et senv tf
    (* The cases below are the interesting ones *)
    | Fun (formal, ftyp, body) ->
        (* Formal parameters are given a linear contribution when producing
           function summaries. Arguments (see App) might be nonlinear. If the
           parameter is a function, we make it EFun = Unknown *)
        let fp_et = get_fp_et fp_count ftyp in
        let senv' = env_new_ident formal fp_et senv in
        let%bind body_et = sa_expr senv' (fp_count + 1) body in
        pure @@ EFun (EFunDef ([ fp_count ], DefExpr body_et))
    | App (f, actuals) ->
        let%bind eapp_ref = get_eapp_referent senv f in
        let%bind arg_ets = mapM actuals ~f:(fun i -> get_ident_et senv i) in
        pure @@ EApp (eapp_ref, arg_ets)
    | MatchExpr (x, clauses) ->
        let%bind xc = get_ident_et senv x in
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
        let spurious = is_spurious_conditional_expr senv x clauses in
        (* Convention: et_nothing if spurious *)
        let cond = if spurious then et_nothing else EOp (Conditional, xc) in
        pure @@ EComposeParallel (cond, cl_ets)
    | Fixpoint (_, _, _) ->
        fail0 "Sharding analysis: somehow encountered a fixpoint??"

  let sa_expr_wrapper senv erep = sa_expr senv 0 erep

  (* Precondition: senv contains the component parameters, appropriately marked *)
  let rec sa_stmt senv summary (stmts : stmt_annot list) ct =
    (* Helpers to continue after
       accumulating an operation *)
    let cont senv summary sts = sa_stmt senv summary sts ct in
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
    match stmts with
    | [] -> pure summary
    | (s, sloc) :: sts -> (
        match s with
        (* Reads and Writes *)
        | Load (x, f) ->
            cont_ident_op x
              (EVal (et_pseudofield (f, None)))
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
            cont_ident_op x
              (EVal (et_pseudofield (m, Some klist)))
              op summary sts
        | MapUpdate (m, klist, opt_i) ->
            let%bind ic =
              match opt_i with
              | Some i -> get_ident_et senv i
              | None -> pure @@ et_nothing
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
        (* Accept, Send, Event, ReadFromBC *)
        | AcceptPayment -> cont_op AcceptMoney summary sts
        | SendMsgs i ->
            let%bind deps = get_ident_et senv i in
            cont_op (SendMessages deps) summary sts
        | CreateEvnt i ->
            let%bind deps = get_ident_et senv i in
            cont_op (EmitEvent deps) summary sts
        (* TODO: Do we want to track blockchain reads? *)
        | ReadFromBC (x, _) -> cont_ident x et_nothing summary sts
        (* Plugging into the expression language *)
        | Bind (x, expr) ->
            let%bind expr_contrib = sa_expr_wrapper senv expr in
            let%bind expr_contrib = et_normalise expr_contrib in
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
              cont senv' summary cl_sts
            in
            let spurious = is_spurious_conditional_stmt xc x clauses in
            let%bind summary' =
              if spurious then
                (* Only the Some branch "really" contributes if spurious *)
                let%bind some_summary =
                  mapM summarise_clause
                    (List.filter
                       (fun (p, _) -> List.length (get_pattern_bounds p) = 1)
                       clauses)
                in
                foldM
                  (fun acc_summ cl_summ ->
                    pure @@ ComponentSummary.union acc_summ cl_summ)
                  summary some_summary
              else
                (* If not spurious, more things happen *)
                let%bind cond = et_normalise @@ EOp (Conditional, xc) in
                let cond_op =
                  if et_can_be_summarised senv cond then ConditionOn cond
                  else
                    AlwaysExclusive
                      ( Some (ER.get_loc (get_rep x)),
                        pp_operation (ConditionOn cond) )
                in
                let summary_with_conds = ComponentSummary.add cond_op summary in
                let%bind cl_summaries = mapM summarise_clause clauses in
                (* TODO: least upper bound? *)
                foldM
                  (fun acc_summ cl_summ ->
                    pure @@ ComponentSummary.union acc_summ cl_summ)
                  summary_with_conds cl_summaries
            in
            cont senv summary' sts
        | CallProc (p, arglist) -> (
            let%bind arg_ets = mapM arglist ~f:(fun i -> get_ident_et senv i) in
            let opt_proc_sig = SAEnv.lookupS senv (get_id p) in
            match opt_proc_sig with
            | Some proc_sig ->
                let%bind call_summ =
                  procedure_call_summary senv p proc_sig arglist arg_ets
                in
                let summary' = ComponentSummary.union summary call_summ in
                cont senv summary' sts
            (* If this occurs, it's a bug. Type checking should prevent it. *)
            | _ ->
                fail1
                  "Sharding analysis: calling procedure that was not analysed"
                  (SR.get_loc (get_rep p)) )
        | Iterate (l, _) ->
            let op =
              AlwaysExclusive (Some (ER.get_loc (get_rep l)), "Iterate")
            in
            cont_op op summary sts
        | Throw i ->
            (* Throwing cancels all effects. All effects happening is a correct
               over-approximation. *)
            cont senv summary sts )

  let sa_component_summary senv (comp : component) =
    let all_params = SCU.append_implict_comp_params comp.comp_params in
    (* Add component parameters to the analysis environment *)
    let senv' =
      (* Give proper type to procedure functions *)
      env_bind_ident_map senv all_params (fun idx i t ->
          (* Transition parameters are constants. Procedure parameters are not
             necessarily, i.e. reads can flow into them. *)
          let et =
            match comp.comp_type with
            (* TODO: might want not to track this for transitions, since
               parameters are constants *)
            | CompTrans -> get_pp_et idx t
            | CompProc -> get_pp_et idx t
          in
          IdentSig (ComponentParameter, PCMStatus.empty, et))
    in
    sa_stmt senv' ComponentSummary.empty comp.comp_body comp.comp_type

  let sa_analyze_folds senv =
    let folds =
      [ "nat_fold"; "nat_foldk"; "list_foldl"; "list_foldr"; "list_foldk" ]
    in
    List.fold_left
      (fun senv s ->
        SAEnv.addS senv s (IdentSig (DoesNotShadow, PCMStatus.empty, EUnknown)))
      senv folds

  let sa_libentries senv (lel : lib_entry list) =
    foldM
      ~f:(fun senv le ->
        match le with
        | LibVar (lname, _, lexp) ->
            let%bind esig = sa_expr_wrapper senv lexp in
            let%bind esig = et_normalise esig in
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

    let senv = sa_analyze_folds senv in

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
      let all_params = SCU.append_implict_contract_params cmod.contr.cparams in
      env_bind_ident_map senv all_params (fun _ _ _ ->
          IdentSig (DoesNotShadow, PCMStatus.empty, et_nothing))
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
