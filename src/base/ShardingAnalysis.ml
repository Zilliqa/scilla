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

  type component_operation =
    (* Read of cfield, with map keys which are comp_params if field is a map *)
    | Read of ER.rep ident *  ER.rep ident list option
    | Write of ER.rep ident *  ER.rep ident list option
    | AcceptMoney
    | SendMessages
    (* Top element -- in case of ambiguity, be conservative *)
    | AlwaysExclusive

  let sprint_operation op =
    let field_access field opt_keys = (
      let base = (get_id field) in
      let keys = (match opt_keys with
        | Some ks ->
            List.fold_left (fun acc kid -> acc ^ "[" ^ (get_id kid) ^ "]") "" ks
        | None -> ""
      ) in base ^ keys
    ) in

    match op with
    | Read (field, opt_keys) -> "Read " ^ field_access field opt_keys
    | Write (field, opt_keys) -> "Write " ^ field_access field opt_keys
    | AcceptMoney -> "AcceptMoney"
    | SendMessages -> "SendMessages"
    | AlwaysExclusive -> "AlwaysExclusive"

  module OrderedComponentOperation = struct
    type t = component_operation
    (* This is super hacky, but works *)
    let compare = fun a b ->
      let str_a = sprint_operation a in
      let str_b = sprint_operation b in
      compare str_a str_b
  end
  module ComponentSummary = Set.Make(OrderedComponentOperation)

  type signature = ComponentSummary

  let sprint_summary summ =
    let ops = ComponentSummary.elements summ in
    List.fold_left (fun acc op -> acc ^ sprint_operation op ^ "\n") "" ops

  module SAEnv = struct
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
          fail1
            (Printf.sprintf
               "Couldn't resolve the identifier in sharding analysis: \"%s\".\n"
               id)
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

  let identity_bind_ident_list senv idlist =
    List.fold_left
      (fun acc_senv i ->
        SAEnv.addS acc_senv (get_id i) ComponentSummary.empty)
      senv idlist

  let is_bottom_level_access m klist =
    let mt = (ER.get_type (get_rep m)).tp in
    let nindices = List.length klist in
    let map_access = nindices <  TU.map_depth mt in
    not map_access

  let rec sa_stmt senv summary (stmts : stmt_annot list) =
    (* Helpers to continue after accumulating an operation *)
    let cont summary sts = sa_stmt senv summary sts in
    let cont_op op summary sts =
      let summary' = ComponentSummary.add op summary in
      cont summary' sts
    in

    (* We can assume everything is well-typed, which makes this much easier *)
    match stmts with
    | [] -> summary
    | (s, sloc) :: sts -> (
        match s with
        | Load (_, f) -> cont_op (Read (f, None)) summary sts
        | Store (_, f) -> cont_op (Write (f, None)) summary sts
        | MapGet (x, m, klist, _) ->
            let op = if is_bottom_level_access m klist
            then (Read (m, Some klist)) else AlwaysExclusive in
            cont_op op summary sts
        | MapUpdate (m, klist, _) ->
            let op = if is_bottom_level_access m klist
            then (Write (m, Some klist)) else AlwaysExclusive in
            cont_op op summary sts
        | AcceptPayment -> cont_op AcceptMoney summary sts
        | SendMsgs i -> cont_op SendMessages summary sts

        | MatchStmt (x, clauses) ->
          let summarize_clause (pattern, cl_sts) = sa_stmt senv summary cl_sts in
          let cl_summaries = List.map summarize_clause clauses in
          let summary' =
              List.fold_left
                (fun acc_summ cl_summ -> ComponentSummary.union acc_summ cl_summ) summary cl_summaries
          in
          cont summary' sts
        | _ -> cont summary sts
    )

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
    let senv' =
      identity_bind_ident_list senv (List.map (fun (i, _) -> i) all_params)
    in
    sa_stmt senv' ComponentSummary.empty comp.comp_body

  let sa_module (cmod: cmodule) (elibs : libtree list) =
    (* Stage 1: determine state footprint of components *)
    let senv = SAEnv.mk() in

    let%bind summaries =
      mapM
        ~f:(fun cp ->
          let%bind summ = pure @@ sa_component_summary senv cp in
          pure @@ (cp.comp_name, summ))
        cmod.contr.ccomps
    in
    pure summaries
end