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

open Core_kernel
open Scilla_base
open ParserUtil
open MonadUtil
open EvalMonad
open EvalMonad.Let_syntax
open Stdint
open ContractUtil
open PrettyPrinters
open TypeUtil
open EvalBuiltins
open Gas
module SR = ParserRep
module ER = ParserRep
module EvalGas = ScillaGas (SR) (ER)
module EvalSyntax = EvalGas.GasSyntax
module EvalLiteral = EvalSyntax.SLiteral
module EvalTypeUtilities = TypeUtilities
module EvalBuiltIns = ScillaEvalBuiltIns (SR) (ER)
module EvalType = EvalSyntax.SType
module EvalIdentifier = EvalSyntax.SIdentifier
module EvalName = EvalIdentifier.Name
open EvalIdentifier
open EvalSyntax

(*****************************************************)
(* Update-only execution environment for expressions *)
(*****************************************************)
module Env = struct
  type ident = EvalName.t

  (* Environment *)
  type t = (EvalName.t * EvalLiteral.t) list [@@deriving sexp]

  (* Pretty-printing *)
  let rec pp_value = pp_literal

  and pp ?(f = fun (_ : EvalName.t * EvalLiteral.t) -> true) e =
    (* FIXME: Do not print folds *)
    let e_filtered = List.filter e ~f in
    let ps =
      List.map e_filtered ~f:(fun (k, v) ->
          " [" ^ EvalName.as_string k ^ " -> " ^ pp_value v ^ "]")
    in
    let cs = String.concat ~sep:",\n " ps in
    "{" ^ cs ^ " }"

  let empty = []

  (* Core's List.Assoc.add function removes duplicate key-value entries to keep lists small *)
  let bind e k v = List.Assoc.add e k v ~equal:[%equal: EvalName.t]

  let bind_all e kvs =
    List.fold_left ~init:e ~f:(fun z (k, v) -> bind z k v) kvs

  (* Unbind those identifiers "id" from "e" which have "f id" false. *)
  let filter e ~f = List.filter e ~f:(fun (id, _) -> f id)

  let lookup e k =
    let open MonadUtil in
    let i = get_id k in
    match List.Assoc.find e i ~equal:[%equal: EvalName.t] with
    | Some v -> pure v
    | None ->
        fail1
          (sprintf "Identifier \"%s\" is not bound in environment:\n"
             (EvalName.as_error_string i))
          (get_rep k)
end

(**************************************************)
(*                 Blockchain State               *)
(**************************************************)
module BlockchainState = struct
  type t = (string * EvalLiteral.t) list

  let lookup e k =
    match List.Assoc.find e k ~equal:String.( = ) with
    | Some v -> pure v
    | None ->
        fail0
        @@ sprintf "No value for key \"%s\" at in the blockchain state:\n%s" k
             (pp_literal_map e)
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
    fields : (EvalName.t * EvalType.t) list;
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
    component_stack : ER.rep EvalIdentifier.t list;
    (* Emitted messages *)
    emitted : EvalLiteral.t list;
    (* Emitted events *)
    events : EvalLiteral.t list;
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
    sprintf
      "Confuration\n\
       Env =\n\
       %s\n\
       Fields =\n\
       %s\n\
       Balance =%s\n\
       Accepted=%s\n\
       \\\n\
      \    Blockchain conf =\n\
       %s\n\
       Incoming funds = %s\n\
       Emitted Messages =\n\
       %s\n\
       Emitted events =\n\
       %s\n"
      pp_env pp_fields pp_balance pp_accepted pp_bc_conf pp_in_funds pp_emitted
      pp_events

  (*  Manipulations with configuration *)

  let store i l = fromR @@ StateService.update ~fname:i ~keys:[] ~value:l

  let load st k =
    let i = get_id k in
    if [%equal: EvalName.t] i balance_label then
      (* Balance is a special case *)
      let l = EvalLiteral.UintLit (Uint128L st.balance) in
      pure l
    else
      let%bind fval = fromR @@ StateService.fetch ~fname:k ~keys:[] in
      match fval with
      | Some v -> pure v
      | _ ->
          fail1
            (Printf.sprintf "Error loading field %s"
               (EvalName.as_error_string i))
            (ER.get_loc (get_rep k))

  let remote_load caddr k =
    let%bind fval =
      fromR
      @@ StateService.external_fetch ~caddr ~fname:k ~keys:[] ~ignoreval:false
    in
    match fval with
    | Some v, _ -> pure v
    | _ ->
        fail1
          (Printf.sprintf "Error loading field %s"
             (EvalName.as_error_string (get_id k)))
          (ER.get_loc (get_rep k))

  let remote_field_type caddr k =
    let%bind fval =
      fromR
      @@ StateService.external_fetch ~caddr ~fname:k ~keys:[] ~ignoreval:true
    in
    match fval with
    | _, Some ty -> pure ty
    | _ ->
        fail0
          (sprintf "Unable to fetch type for field %s at address %s"
             (EvalLiteral.Bystrx.hex_encoding caddr)
             (as_error_string k))

  (* Update a map. If "vopt" is None, delete the key, else replace the key value with Some v. *)
  let map_update m klist vopt =
    match vopt with
    | Some v -> fromR @@ StateService.update ~fname:m ~keys:klist ~value:v
    | None -> fromR @@ StateService.remove ~fname:m ~keys:klist

  (* Fetch from a map. If "fetchval" is true, fetch the value, else just query if the key exists. *)
  let map_get st m klist fetchval =
    let open BuiltIns.UsefulLiterals in
    if fetchval then
      let%bind vopt = fromR @@ StateService.fetch ~fname:m ~keys:klist in
      match
        List.Assoc.find st.fields (get_id m) ~equal:[%equal: EvalName.t]
      with
      | Some mt -> (
          let%bind vt =
            fromR @@ EvalTypeUtilities.map_access_type mt (List.length klist)
          in
          (* Need to wrap the result in a Scilla Option. *)
          match vopt with
          | Some v ->
              let%bind v_lit = fromR @@ some_lit v in
              pure v_lit
          | None -> pure (none_lit vt) )
      | None ->
          fail1
            (sprintf "Unable to fetch from map field %s" (as_error_string m))
            (ER.get_loc (get_rep m))
    else
      let%bind is_member =
        fromR @@ StateService.is_member ~fname:m ~keys:klist
      in
      pure @@ EvalLiteral.build_bool_lit is_member

  let remote_map_get caddr m keys fetchval =
    let open BuiltIns.UsefulLiterals in
    if fetchval then
      (* We need to fetch the type in advance because the type-option returned
       * by the actual call may be None if the key(s) wasn't found,
       * (but the map map field itself still exists). *)
      let%bind mt = remote_field_type caddr m in
      let%bind vt =
        fromR @@ EvalTypeUtilities.map_access_type mt (List.length keys)
      in
      let%bind vopt, _ =
        fromR
        @@ StateService.external_fetch ~caddr ~fname:m ~keys ~ignoreval:false
      in
      (* Need to wrap the result in a Scilla Option. *)
      match vopt with
      | Some v ->
          let%bind v_lit = fromR @@ some_lit v in
          pure v_lit
      | None -> pure (none_lit vt)
    else
      let%bind _, topt =
        fromR
        @@ StateService.external_fetch ~caddr ~fname:m ~keys ~ignoreval:true
      in
      pure @@ EvalLiteral.build_bool_lit (Option.is_some topt)

  let bind st k v =
    let e = st.env in
    { st with env = List.Assoc.add e k v ~equal:[%equal: EvalName.t] }

  let bind_all st ks vs =
    let e = st.env in
    match List.zip ks vs with
    | Unequal_lengths ->
        fail0
          "Attempting to bind different number of keys and values in \
           environment"
    | Ok kvs ->
        let filtered_env =
          List.filter e ~f:(fun z ->
              not (List.mem ks (fst z) ~equal:[%equal: EvalName.t]))
        in
        pure { st with env = kvs @ filtered_env }

  let lookup st k = Env.lookup st.env k

  let bc_lookup st k = BlockchainState.lookup st.blockchain_state k

  let accept_incoming st =
    let incoming' = st.incoming_funds in
    (* Although unsigned integer is used, and this check isn't
     * necessary, we have it just in case, somehow a malformed
     * Uint128 literal manages to reach here. *)
    if Uint128.compare incoming' Uint128.zero >= 0 then
      let balance = Uint128.add st.balance incoming' in
      let accepted = true in
      let incoming_funds = Uint128.zero in
      pure @@ { st with balance; accepted; incoming_funds }
    else
      fail0
      @@ sprintf "Incoming balance is negative (somehow):%s."
           (Uint128.to_string incoming')

  (* Finds a procedure proc_name, and returns the procedure and the
     list of procedures in scope for that procedure *)
  let lookup_procedure st proc_name =
    let rec finder procs =
      match procs with
      | p :: p_rest when EvalIdentifier.equal p.comp_name proc_name ->
          pure (p, p_rest)
      | _ :: p_rest -> finder p_rest
      | [] ->
          fail0 @@ sprintf "Procedure %s not found." (as_error_string proc_name)
    in
    finder st.procedures

  (* Check that message is well-formed before adding to the sending pool *)
  let rec validate_messages ls =
    let open EvalLiteral in
    (* Note: We don't need a whole lot of checks as the checker does it. *)
    let validate_msg_payload pl =
      let has_tag = List.Assoc.mem pl "tag" ~equal:String.( = ) in
      if has_tag then pure ()
      else
        fail0
        @@ sprintf "Message contents have no \"tag\" field:\n[%s]"
             (pp_literal_map pl)
    in
    match ls with
    | Msg pl :: tl ->
        let%bind () = validate_msg_payload pl in
        validate_messages tl
    | [] -> pure ()
    | m :: _ -> fail0 @@ sprintf "This is not a message:\n%s" (pp_literal m)

  let validate_outgoing_message m' =
    let open EvalLiteral in
    let open ContractUtil.MessagePayload in
    match m' with
    | Msg m ->
        (* All outgoing messages must have certain mandatory fields *)
        let tag_found = List.Assoc.mem m tag_label ~equal:String.( = ) in
        let amount_found = List.Assoc.mem m amount_label ~equal:String.( = ) in
        let recipient_found =
          List.Assoc.mem m recipient_label ~equal:String.( = )
        in
        let uniq_entries =
          not
          @@ List.contains_dup m ~compare:(fun (s, _) (t, _) ->
                 String.compare s t)
        in
        if tag_found && amount_found && recipient_found && uniq_entries then
          pure m'
        else
          fail0
          @@ sprintf
               "Message %s is missing a mandatory field or has duplicate \
                fields."
               (pp_literal (Msg m))
    | _ ->
        fail0
        @@ sprintf "Literal %s is not a message, cannot be sent."
             (pp_literal m')

  let send_messages conf ms =
    let%bind ls' = fromR @@ Datatypes.scilla_list_to_ocaml ms in
    let%bind ls = mapM ~f:validate_outgoing_message ls' in
    let old_emitted = conf.emitted in
    let emitted = old_emitted @ ls in
    pure { conf with emitted }

  let validate_event m' =
    let open EvalLiteral in
    let open ContractUtil.MessagePayload in
    match m' with
    | Msg m ->
        (* All events must have certain mandatory fields *)
        let eventname_found =
          List.Assoc.mem m eventname_label ~equal:String.( = )
        in
        let uniq_entries =
          not
          @@ List.contains_dup m ~compare:(fun (s, _) (t, _) ->
                 String.compare s t)
        in
        if eventname_found && uniq_entries then pure m'
        else
          fail0
          @@ sprintf
               "Event %s is missing a mandatory field or has duplicate fields."
               (pp_literal (Msg m))
    | _ ->
        fail0
        @@ sprintf "Literal %s is not a valid event argument." (pp_literal m')

  let create_event conf l =
    let open EvalLiteral in
    let%bind event =
      match l with
      | Msg _ -> pure @@ l
      | _ ->
          fail0 @@ sprintf "Incorrect event parameter(s): %s\n" (pp_literal l)
    in
    let%bind event' = validate_event event in
    let old_events = conf.events in
    let events = event' :: old_events in
    pure { conf with events }
end

(*****************************************************)
(*         Contract state after initialization       *)
(*****************************************************)

module ContractState = struct
  type init_args = (string * EvalLiteral.t) list

  (* Runtime contract configuration and operations with it *)
  type t = {
    (* Immutable parameters *)
    env : Env.t;
    (* Contract fields *)
    fields : (EvalName.t * EvalType.t) list;
    (* Contract balance *)
    balance : uint128;
  }

  (* Pretty-printing *)
  let pp cstate =
    let pp_params = Env.pp cstate.env in
    let pp_fields = pp_typ_map cstate.fields in
    let pp_balance = Uint128.to_string cstate.balance in
    sprintf
      "Contract State:\n\
       Immutable parameters and libraries =\n\
       %s\n\
       Mutable fields = \n\
       %s\n\
       Balance = %s\n"
      pp_params pp_fields pp_balance
end
