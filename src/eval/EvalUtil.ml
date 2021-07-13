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

  let lookup st k = Env.lookup st.env k

  (* Helper function for remote fetches *)
  let lookup_sender_addr st =
    let%bind sender =
      fromR
      @@ lookup st
           (mk_loc_id (label_name_of_string MessagePayload.sender_label))
    in
    match sender with
    | EvalLiteral.ByStrX bs -> pure bs
    | _ ->
        fail0
          (sprintf "Incorrect type of _sender in environment: %s"
             (pp_literal sender))

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

  let remote_load st caddr k =
    let%bind fval =
      fromR
      @@ StateService.external_fetch ~caddr ~fname:k ~keys:[] ~ignoreval:false
    in
    match fval with
    | Some v, _ ->
        (* _sender._balance is a special case if funds have been accepted. _amount must be deducted. *)
        let%bind sender_addr = lookup_sender_addr st in
        if
          st.accepted
          && EvalLiteral.Bystrx.equal sender_addr caddr
          && EvalName.equal (get_id k) balance_label
        then
          let%bind amount_lit =
            fromR
            @@ lookup st
                 (mk_loc_id (label_name_of_string MessagePayload.amount_label))
          in
          match (v, amount_lit) with
          | UintLit (Uint128L sender_balance), UintLit (Uint128L amount)
            when Uint128.compare sender_balance amount >= 0 ->
              pure
              @@ EvalLiteral.UintLit
                   (Uint128L Uint128.(sender_balance - amount))
          | _ ->
              fail0
              @@ sprintf
                   "Unexpected sender balance or amount literal: sender \
                    balance = %s, amount = %s"
                   (pp_literal v) (pp_literal amount_lit)
        else pure v
    | _ ->
        fail1
          (Printf.sprintf "Error loading remote field %s at address %s"
             (EvalName.as_error_string (get_id k))
             (SLiteral.Bystrx.hex_encoding caddr))
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
    let open EvalLiteral in
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
              let%bind v_lit = pure @@ build_some_lit v vt in
              pure v_lit
          | None -> pure (build_none_lit vt))
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
    let open EvalLiteral in
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
      | Some v -> pure @@ build_some_lit v vt
      | None -> pure (build_none_lit vt)
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

  let bc_lookup st k = BlockchainState.lookup st.blockchain_state k

  let accept_incoming st =
    if st.accepted then (* Do nothing *)
      pure st
    else
      (* Check that sender balance is sufficient *)
      let%bind sender_addr = lookup_sender_addr st in
      let%bind sender_balance_l =
        remote_load st sender_addr (mk_loc_id balance_label)
      in
      let incoming' = st.incoming_funds in
      match sender_balance_l with
      | UintLit (Uint128L sender_balance) ->
          if Uint128.compare incoming' sender_balance > 0 then
            fail0
              ("Insufficient sender balance for acceptance. Incoming vs \
                sender_balance: "
              ^ Uint128.to_string incoming'
              ^ " vs "
              ^ Uint128.to_string sender_balance)
          else if
            (* Although unsigned integer is used, and this check isn't
             * necessary, we have it just in case, somehow a malformed
             * Uint128 literal manages to reach here. *)
            Uint128.compare incoming' Uint128.zero >= 0
          then
            let balance = Uint128.add st.balance incoming' in
            let accepted = true in
            let incoming_funds = Uint128.zero in
            pure @@ { st with balance; accepted; incoming_funds }
          else
            fail0
            @@ sprintf "Incoming balance is negative (somehow):%s."
                 (Uint128.to_string incoming')
      | _ ->
          fail0
          @@ sprintf "Unrecognized balance literal at sender: %s"
               (pp_literal sender_balance_l)

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

  let validate_outgoing_message m' =
    let open EvalLiteral in
    let open ContractUtil.MessagePayload in
    match m' with
    | Msg m ->
        (* All outgoing messages must have certain mandatory fields *)
        let tag_found =
          List.exists m ~f:(fun (x, _, _) -> String.(tag_label = x))
        in
        let amount_found =
          List.exists m ~f:(fun (x, _, _) -> String.(amount_label = x))
        in
        let recipient_found =
          List.exists m ~f:(fun (x, _, _) -> String.(recipient_label = x))
        in
        let uniq_entries =
          not
          @@ List.contains_dup m ~compare:(fun (s, _, _) (t, _, _) ->
                 String.compare s t)
        in
        if tag_found && amount_found && recipient_found && uniq_entries then
          pure ()
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
    let%bind () = forallM ~f:validate_outgoing_message ls' in
    let%bind out_funds = foldM ls' ~init:Uint128.zero ~f:(fun run_total msg_lit ->
        match msg_lit with
        | SLiteral.Msg msg ->
            let%bind amount = fromR @@ MessagePayload.get_amount msg in
            pure (Uint128.(run_total + amount))
        | _ ->
            fail0
            @@ sprintf "Literal %s verified as a message, but is not a message literal."
              (pp_literal msg_lit))
    in
    let old_emitted = conf.emitted in
    let emitted = old_emitted @ ls' in
    let old_balance = conf.balance in
    let%bind balance =
      if Uint128.compare old_balance out_funds < 0 then
        fail0
        @@ sprintf
          "The balance (%s) is too low to transfer all the funds in the \
           messages (%s)"
          (Uint128.to_string old_balance)
          (Uint128.to_string out_funds)
      else pure @@ Uint128.(old_balance - out_funds)
    in
    pure { conf with emitted; balance }

  let validate_event m' =
    let open EvalLiteral in
    let open ContractUtil.MessagePayload in
    match m' with
    | Msg m ->
        (* All events must have certain mandatory fields *)
        let eventname_found =
          List.exists m ~f:(fun (x, _, _) -> String.(eventname_label = x))
        in
        let uniq_entries =
          not
          @@ List.contains_dup m ~compare:(fun (s, _, _) (t, _, _) ->
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

(*****************************************************)
(*          Dynamic typecheck of addresses           *)
(*****************************************************)
module EvalTypecheck = struct
  open MonadUtil
  open Result.Let_syntax

  (* Checks that _this_address is defined *)
  let is_contract_addr ~caddr =
    let this_id = EvalIdentifier.mk_loc_id this_address_label in
    let%bind _, this_typ_opt =
      StateService.external_fetch ~caddr ~fname:this_id ~keys:[] ~ignoreval:true
    in
    pure @@ Option.is_some this_typ_opt

  (* Checks that balance > 0 || nonce > 0 *)
  let is_user_addr ~caddr =
    (* First check if the address is a user address with balance > 0 || nonce > 0 *)
    let balance_id = EvalIdentifier.mk_loc_id balance_label in
    let nonce_id = EvalIdentifier.mk_loc_id nonce_label in
    let%bind balance_lit, _ =
      StateService.external_fetch ~caddr ~fname:balance_id ~keys:[]
        ~ignoreval:false
    in
    let%bind nonce_lit, _ =
      StateService.external_fetch ~caddr ~fname:nonce_id ~keys:[]
        ~ignoreval:false
    in
    match (balance_lit, nonce_lit) with
    | Some (UintLit (Uint128L balance)), Some (UintLit (Uint64L nonce))
      when Uint128.compare balance Uint128.zero > 0
           || Uint64.compare nonce Uint64.zero > 0 ->
        pure true
    | _ -> pure false

  let is_address_in_use ~caddr =
    (* True if the address is in use, false otherwise *)
    let%bind user_addr = is_user_addr ~caddr in
    if not user_addr then
      let%bind contract_addr = is_contract_addr ~caddr in
      pure contract_addr
    else pure true

  let typecheck_remote_fields ~caddr fts =
    (* Check that all fields are defined at caddr, and that their types are assignable to what is expected *)
    allM fts ~f:(fun (f, t) ->
        let%bind res =
          StateService.external_fetch ~caddr ~fname:f ~keys:[] ~ignoreval:true
        in
        match res with
        | _, Some ext_typ ->
            pure @@ EvalType.type_assignable ~expected:t ~actual:ext_typ
        | _, None -> pure false)

  type evalTCResult =
    | AddressNotInUse
    | NoContractAtAddress
    | FieldTypeMismatch
    | Success

  let typecheck_fts ~caddr fts_opt =
    match fts_opt with
    | None ->
        let%bind in_use = is_address_in_use ~caddr in
        if not in_use then pure AddressNotInUse else pure Success
    | Some fts ->
        (* True if the address contains a contract with the appropriate fields, false otherwise *)
        let%bind contract_addr = is_contract_addr ~caddr in
        if not contract_addr then pure NoContractAtAddress
        else
          let%bind fts_ok = typecheck_remote_fields ~caddr fts in
          if not fts_ok then pure FieldTypeMismatch else pure Success

  let get_fts_opt_from_address t =
    let open EvalType in
    match t with
    | Address fts_opt -> pure fts_opt
    | _ ->
        fail0
        @@ sprintf "Unable to perform dynamic typecheck on type %s\n" (pp_typ t)

  let assert_typecheck_remote_field_types ~caddr t =
    let open EvalType in
    let%bind fts_opt = get_fts_opt_from_address t in
    let%bind tc_res =
      typecheck_fts ~caddr (Option.map ~f:IdLoc_Comp.Map.to_alist fts_opt)
    in
    match tc_res with
    | AddressNotInUse ->
        fail0
        @@ sprintf "Address %s not in use."
             (EvalLiteral.Bystrx.hex_encoding caddr)
    | NoContractAtAddress ->
        fail0
        @@ sprintf "No contract found at address %s"
             (EvalLiteral.Bystrx.hex_encoding caddr)
    | FieldTypeMismatch ->
        fail0
        @@ sprintf "Address %s does not satisfy type %s\n"
             (EvalLiteral.Bystrx.hex_encoding caddr)
             (pp_typ t)
    | Success -> pure ()

  let typecheck_remote_field_types ~caddr t =
    let open EvalType in
    let%bind fts_opt = get_fts_opt_from_address t in
    let%bind tc_res =
      typecheck_fts ~caddr (Option.map ~f:IdLoc_Comp.Map.to_alist fts_opt)
    in
    match tc_res with
    | AddressNotInUse | NoContractAtAddress | FieldTypeMismatch -> pure false
    | Success -> pure true
end
