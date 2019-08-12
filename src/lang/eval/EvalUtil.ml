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
  scilla.  If not, see <http://www.gnu.org/licenses/>.
*)

open Syntax
open Core
open ErrorUtils
open MonadUtil
open EvalMonad
open EvalMonad.Let_syntax
open Stdint
open ContractUtil
open PrettyPrinters
open TypeUtil
open BuiltIns
open Gas

open ParserUtil
module SR = ParserRep
module ER = ParserRep
module EvalSyntax = ScillaSyntax (SR) (ER)
module EvalTypeUtilities = TypeUtilities
module EvalBuiltIns = ScillaBuiltIns (SR) (ER) 
module EvalGas = ScillaGas (SR) (ER)

open EvalSyntax
    
(* Return a builtin_op wrapped in EvalMonad *)
let builtin_executor f arg_tps arg_lits =
  let%bind (_, ret_typ, op) =
    fromR @@ EvalBuiltIns.BuiltInDictionary.find_builtin_op f arg_tps in
  let%bind cost = fromR @@ EvalGas.builtin_cost f arg_lits in
  let res () = op arg_lits ret_typ in
  checkwrap_opR res (Uint64.of_int cost)

(* Add a check that the just evaluated statement was in our gas limit. *)
let stmt_gas_wrap scon sloc =
  let%bind cost = fromR @@ EvalGas.stmt_cost scon in
  let err = (mk_error1 "Ran out of gas evaluating statement" sloc) in 
  let dummy () = pure () in (* the operation is already executed unfortunately *)
    checkwrap_op dummy (Uint64.of_int cost) err

(*****************************************************)
(* Update-only execution environment for expressions *)
(*****************************************************)
module Env = struct
  type ident = string

  (* Environment *)
  type t = (string * literal) list
  [@@deriving sexp]

  (* Pretty-printing *)
  let rec pp_value = pp_literal
  and pp ?f:(f = fun (_ : (string * literal)) -> true) e =
    (* FIXME: Do not print folds *)
    let e_filtered = List.filter e ~f:f in
    let ps = List.map e_filtered
        ~f:(fun (k, v) -> " [" ^ k ^ " -> " ^ (pp_value v) ^ "]") in
    let cs = String.concat ~sep:",\n " ps in
    "{" ^ cs ^ " }"

  let empty = []

  let bind e k v =
    (k, v) :: List.filter ~f:(fun z -> fst z <> k) e

  let bind_all e kvs = 
    List.fold_left ~init:e ~f:(fun z (k, v) -> bind z k v) kvs

  (* Unbind those identifiers "id" from "e" which have "f id" false. *)
  let filter e ~f =
    List.filter e ~f:(fun (id, _) -> f id)

  let lookup e k =
    let i = get_id k in
    match List.find ~f:(fun z -> fst z = i) e with 
    | Some x -> pure @@ snd x
    | None -> fail1 (sprintf
        "Identifier \"%s\" is not bound in environment:\n" i) (get_rep k)
end

(**************************************************)
(*                 Blockchain State               *)
(**************************************************)
module BlockchainState = struct
  type t = (string * literal) list

  let lookup e k =
    match List.find ~f:(fun z -> fst z = k) e with 
    | Some x -> pure @@ snd x
    | None -> fail0 @@ sprintf
        "No value for key \"%s\" at in the blockchain state:\n%s"
        k (pp_literal_map e)  
end

(**************************************************)
(*          Runtime contract configuration        *)
(**************************************************)
module Configuration = struct

  (* Runtime contract configuration and operations with it *)
  type t = {
    (* Initial environment of parameters *)
    init_env : Env.t;
    (* Current environment parameters and local variables *)
    env : Env.t;
    (* Contract fields *)
    fields : (string * typ) list;
    (* Contract balance *)
    balance : uint128;
    (* Was incoming money accepted? *)
    accepted : bool;
    (* Blockchain state *)
    blockchain_state : BlockchainState.t;
    (* Available incoming funds *)
    incoming_funds : uint128;
    (* Procedures available to the current component. The list is in
       reverse order, so that for any procedure p in the list such
       that p :: p_rest is a suffix of the list, p_rest contains the
       procedures available to p. *)
    procedures : EvalSyntax.component list;
    (* The stack of procedure call, starting from the externally invoked transition. *)
    component_stack : ER.rep ident list;
    (* Emitted messages *)
    emitted : literal list;
    (* Emitted events *)
    events : literal list
  }

  let pp conf =
    let pp_env = Env.pp conf.env in
    let pp_fields = pp_typ_map conf.fields in
    let pp_balance = Uint128.to_string conf.balance in
    let pp_accepted = Bool.to_string conf.accepted in
    let pp_bc_conf = pp_literal_map conf.blockchain_state in
    let pp_in_funds = Uint128.to_string conf.incoming_funds in
    (*  let pp_procs = TODO... *)
    let pp_emitted = pp_literal_list conf.emitted in
    let pp_events = pp_literal_list conf.events in
    sprintf "Confuration\nEnv =\n%s\nFields =\n%s\nBalance =%s\nAccepted=%s\n\\
    Blockchain conf =\n%s\nIncoming funds = %s\nEmitted Messages =\n%s\nEmitted events =\n%s\n"
      pp_env pp_fields pp_balance pp_accepted pp_bc_conf pp_in_funds pp_emitted pp_events

  (*  Manipulations with configuration *)

  let store i l =
    fromR @@ StateService.update ~fname:i ~keys:[] ~value:l

  let load st k =
    let i = get_id k in
    if i = balance_label
    then
      (* Balance is a special case *)
      let l = UintLit (Uint128L st.balance) in
      pure (l, G_Load(l))
    else
      let%bind fval = fromR @@ StateService.fetch ~fname:k ~keys:[] in
      match fval with
      | (Some v, g) -> pure (v, g)
      | _ -> fail1 (Printf.sprintf "Error loading field %s" i) (ER.get_loc (get_rep k))

  (* Update a map. If "vopt" is None, delete the key, else replace the key value with Some v. *)
  let map_update m klist vopt =
    match vopt with
    | Some v -> fromR @@ StateService.update ~fname:m ~keys:klist ~value:v
    | None -> fromR @@  StateService.remove ~fname:m ~keys:klist

  (* Fetch from a map. If "fetchval" is true, fetch the value, else just query if the key exists. *)
  let map_get st m klist fetchval =
    let open BuiltIns.UsefulLiterals in
    if fetchval
    then
      let%bind vopt = fromR @@ StateService.fetch ~fname:m ~keys:klist in
      match List.find st.fields ~f:(fun (z, _) -> z = (get_id m)) with
      | Some (_, mt) ->
        let%bind vt = fromR @@ EvalTypeUtilities.map_access_type mt (List.length klist) in
        (* Need to wrap the result in a Scilla Option. *)
        (match vopt with
        | (Some v, G_MapGet(i, Some lo)) ->
          let%bind lo_lit = fromR @@ some_lit lo in
          let%bind v_lit = fromR @@ some_lit v in
          let g' = G_MapGet(i, Some lo_lit) in
          pure (v_lit, g')
        | (None, G_MapGet(i, None)) ->
          let g' = G_MapGet(i, Some (none_lit vt)) in
          pure (none_lit vt, g')
        | _ -> fail1 (sprintf "Inconsistency in fetching map value form StateService for field %s" (get_id m))
                  (ER.get_loc (get_rep m))
        )
      | None -> fail1 (sprintf "Unable to fetch from map field %s" (get_id m))
                  (ER.get_loc (get_rep m))
    else
      let%bind (is_member, g) = fromR @@ StateService.is_member ~fname:m ~keys:klist in
      match g with
      | G_MapGet (i, _) ->
          let is_member_lit = to_Bool is_member in
          let g' = G_MapGet(i, Some is_member_lit) in
          pure (is_member_lit, g')
      | _ -> fail1 (sprintf "Unable to check exists for map field %s" (get_id m))
                  (ER.get_loc (get_rep m))

  let bind st k v =
    let e = st.env in
    {st with env = (k, v) :: List.filter ~f:(fun z -> fst z <> k) e}

  let bind_all st ks vs =
    let e = st.env in
    match List.zip ks vs with
    | None -> fail0 "Attempting to bind different number of keys and values in environment"
    | Some kvs ->
        let filtered_env =
          List.filter e ~f:(fun z ->
              not (
                List.exists ks ~f:(fun x -> fst z = x))) in
        pure {st with env = kvs @ filtered_env}

  let lookup st k = Env.lookup st.env k

  let bc_lookup st k = BlockchainState.lookup st.blockchain_state k

  let accept_incoming st =
    let incoming' = st.incoming_funds in
    (* Although unsigned integer is used, and this check isn't
     * necessary, we have it just in case, some how a malformed
     * Uint128 literal manages to reach here.  *)
    if (Uint128.compare incoming' Uint128.zero) >= 0
    then
      let balance = Uint128.add st.balance incoming' in
      let accepted = true in
      let incoming_funds = Uint128.zero in
      pure @@ {st with balance; accepted; incoming_funds}
    else
      fail0 @@ sprintf "Incoming balance is negaitve (somehow):%s."
        (Uint128.to_string incoming')

  (* Finds a procedure proc_name, and returns the procedure and the
     list of procedures in scope for that procedure *)
  let lookup_procedure st proc_name =
    let rec finder procs =
      match procs with
      | p :: p_rest
        when (get_id p.comp_name) = proc_name ->
          pure (p, p_rest)
      | _ :: p_rest ->
          finder p_rest
      | [] ->
          fail0 @@ sprintf "Procedure %s not found." proc_name in
    finder st.procedures
  
  (* Check that message is well-formed before adding to the sending pool *)
  let rec validate_messages ls =
    (* Note: We don't need a whole lot of checks as the checker does it. *)
    let validate_msg_payload pl =
      let has_tag = List.exists pl ~f:(fun (k, _) -> k = "tag") in      
      if has_tag then pure true
      else fail0 @@ sprintf "Message contents have no \"tag\" field:\n[%s]"
          (pp_literal_map pl)
    in
    match ls with
      | (Msg pl) :: tl ->
          let%bind _ = validate_msg_payload pl in
          validate_messages tl
      | [] -> pure true
      | m :: _ -> fail0 @@ sprintf "This is not a message:\n%s" (pp_literal m)

  let validate_outgoing_message m' =
    let open ContractUtil.MessagePayload in
    match m' with
    | Msg m ->
      (* All outgoing messages must have certain mandatory fields *)
      let tag_found = List.exists ~f:(fun (s, _) -> s = tag_label) m in
      let amount_found = List.exists ~f:(fun (s, _) -> s = amount_label) m in
      let recipient_found = List.exists ~f:(fun (s, _) -> s = recipient_label) m in
      let uniq_entries = List.for_all m
        ~f:(fun e -> (List.count m ~f:(fun e' -> fst e = fst e')) = 1) in
      if tag_found && amount_found && recipient_found && uniq_entries then pure m'
      else fail0 @@ sprintf 
        "Message %s is missing a mandatory field or has duplicate fields." (pp_literal (Msg m))
    | _ -> fail0 @@ sprintf "Literal %s is not a message, cannot be sent." (pp_literal m')

  let send_messages conf ms =
    let%bind ls' = fromR @@ Datatypes.scilla_list_to_ocaml ms in
    let%bind ls = mapM ~f:validate_outgoing_message ls' in
    let old_emitted = conf.emitted in
    let emitted = old_emitted @ ls in
    pure ({conf with emitted}, G_SendMsgs ls)

  let validate_event m' =
    let open ContractUtil.MessagePayload in
    match m' with
    | Msg m ->
      (* All events must have certain mandatory fields *)
      let eventname_found = List.exists ~f:(fun (s, _) -> s = eventname_label) m in
      let uniq_entries = List.for_all m
        ~f:(fun e -> (List.count m ~f:(fun e' -> fst e = fst e')) = 1) in
      if eventname_found && uniq_entries then pure m'
      else fail0 @@ sprintf 
        "Event %s is missing a mandatory field or has duplicate fields." (pp_literal (Msg m))
    | _ -> fail0 @@ sprintf "Literal %s is not a valid event argument." (pp_literal m')


  let create_event conf l =
    let%bind event = 
      (match l with
       | Msg _  ->
           pure @@ l
       | _ -> fail0 @@ sprintf "Incorrect event parameter(s): %s\n" (pp_literal l))
    in
    let%bind event' = validate_event event in
    let old_events = conf.events in
    let events = event'::old_events in
    pure ({conf with events}, G_CreateEvnt event')
end

(*****************************************************)
(*         Contract state after initialization       *)
(*****************************************************)

module ContractState = struct

  type init_args = (string * literal) list 

  (* Runtime contract configuration and operations with it *)
  type t = {
    (* Immutable parameters *)
    env : Env.t;
    (* Contract fields *)
    fields : (string * typ) list;
    (* Contract balance *)
    balance : uint128;
  }

  (* Pretty-printing *)
  let pp cstate =
    let pp_params = Env.pp cstate.env in
    let pp_fields = pp_typ_map cstate.fields in
    let pp_balance = Uint128.to_string cstate.balance in
    sprintf "Contract State:\nImmutable parameters and libraries =\n%s\nMutable fields = \n%s\nBalance = %s\n"
      pp_params pp_fields pp_balance

end
