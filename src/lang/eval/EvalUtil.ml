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
open Result.Let_syntax
open MonadUtil
open Stdint
open ContractUtil

(*****************************************************)
(* Update-only execution environment for expressions *)
(*****************************************************)
module Env = struct
  type ident = string

  (* Environment *)
  type 'rep t =
    (string * 'rep value) list
  and
  (* Fully reduced value *)
  'rep value =
    | ValLit of literal
    | ValClosure of 'rep Syntax.ident * typ * 'rep expr_annot * 'rep t
    | ValTypeClosure of 'rep Syntax.ident * 'rep expr_annot * 'rep t                      
    | ValFix of 'rep Syntax.ident * typ * 'rep expr_annot * 'rep t
  [@@deriving sexp]

  (* Pretty-printing *)
  let rec pp_value v = match v with
    | ValLit l ->  pp_literal l
    | ValFix _ -> "<fixpoint>"
    | ValTypeClosure _ -> "<type_closure>"
    | ValClosure _ -> "<closure>"
    (* | ValClosure (f, t, e, env) ->
         (pp_expr (Fun (f, t, e)))
          ^ ", " ^ (pp env) *)
  and pp ?f:(f = fun (_ : (string * 'rep value)) -> true) e =
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
                                                                
  let lookup e k =
    let i = get_id k in
    match List.find ~f:(fun z -> fst z = i) e with 
    | Some x -> pure @@ snd x
    | None -> fail @@ sprintf
        "Identifier \"%s\" at %s is not bound in environment:\n"
        i (get_loc_str (get_loc k))
end


(**************************************************)
(*                 Blockchain State               *)
(**************************************************)
module BlockchainState = struct
  type t = (string * literal) list

  let lookup e k =
    match List.find ~f:(fun z -> fst z = k) e with 
    | Some x -> pure @@ snd x
    | None -> fail @@ sprintf
        "No value for key \"%s\" at in the blockchain state:\n%s"
        k (pp_literal_map e)  
end

(**************************************************)
(*          Runtime contract configuration        *)
(**************************************************)
module Configuration = struct

  (* Runtime contract configuration and operations with it *)
  type 'rep t = {
    (* Immutable variables *)
    env : 'rep Env.t;
    (* Contract fields *)
    fields : (string * literal) list;
    (* Contract balance *)
    balance : uint128;
    (* Was incoming money accepted? *)
    accepted : bool;
    (* Blockchain state *)
    blockchain_state : BlockchainState.t;
    (* Available incoming funds *)
    incoming_funds : uint128;
    (* Emitted messages *)
    emitted : literal list;
    (* Emitted events *)
    events : (string * literal) list
  }

  let pp conf =
    let pp_env = Env.pp conf.env in
    let pp_fields = pp_literal_map conf.fields in
    let pp_balance = Uint128.to_string conf.balance in
    let pp_accepted = Bool.to_string conf.accepted in
    let pp_bc_conf = pp_literal_map conf.blockchain_state in
    let pp_in_funds = Uint128.to_string conf.incoming_funds in
    let pp_emitted = pp_literal_list conf.emitted in
    let pp_events = pp_named_literal_list conf.events in
    sprintf "Confuration\nEnv =\n%s\nFields =\n%s\nBalance =%s\nAccepted=%s\n\\
    Blockchain conf =\n%s\nIncoming funds = %s\nEmitted Messages =\n%s\nEmitted events =\n%s\n"
      pp_env pp_fields pp_balance pp_accepted pp_bc_conf pp_in_funds pp_emitted pp_events

  (*  Manipulations with configuartion *)
  
  let store st k v =
    match v with 
    | Env.ValClosure _ | Env.ValFix _ | Env.ValTypeClosure _ ->
        fail @@ sprintf "Cannot store a closure below into a field %s:\n%s"
          k (Env.pp_value v)
    | Env.ValLit l ->
        (let s = st.fields in
         match List.find s ~f:(fun (z, _) -> z = k) with
         | Some (_, _) -> pure @@
             {st with
              fields = (k, l) :: List.filter ~f:(fun z -> fst z <> k) s}
         | None -> fail @@ sprintf
               "No field \"%s\" in fields:\n%s" k (pp_literal_map s))

  let load st k =
    let i = get_id k in
    if i = balance_label
    then
      (* Balance is a special case *)   
      pure @@ UintLit (128, (Uint128.to_string st.balance))
    else
      (* Evenrything else is from fields *)
      let s = st.fields in
      match List.find ~f:(fun z -> fst z = i) s with 
      | Some x -> pure @@ snd x
      | None -> fail @@ sprintf
            "No field \"%s\" in field map:\n%s" i (pp_literal_map s)

  let bind st k v =
    let e = st.env in
    {st with env = (k, v) :: List.filter ~f:(fun z -> fst z <> k) e}

  let lookup st k = Env.lookup st.env k

  let bc_lookup st k = BlockchainState.lookup st.blockchain_state k

  let accept_incoming st =
    let incoming' = st.incoming_funds in
    if (Uint128.compare incoming' Uint128.zero) > 0
    then
      let balance = Uint128.add st.balance incoming' in
      let accepted = true in
      let incoming_funds = Uint128.zero in
      pure @@ {st with balance; accepted; incoming_funds}
    else
      fail @@ sprintf "Incoming balance is negaitve (somehow):%s."
        (Uint128.to_string incoming')

  (* Check that message is well-formed before adding to the sending pool *)
  let rec validate_messages ls =
    (* TODO: implement more checks *)
    let validate_msg_payload pl =
      let has_tag = List.exists pl ~f:(fun (k, _) -> k = "tag") in      
      if has_tag then pure true
      else fail @@ sprintf "Message contents have no \"tag\" field:\n[%s]"
          (pp_literal_map pl)
    in
    match ls with
      | (Msg pl) :: tl ->
          let%bind _ = validate_msg_payload pl in
          validate_messages tl
      | [] -> pure true
      | m :: _ -> fail @@ sprintf "This is not a message:\n%s" (pp_literal m)

  (* Convert Scilla list to OCaml list *)
  let get_list_literal v =
      let rec convert_to_list = (function
        | ADTValue ("Nil", _, []) -> pure []
        | ADTValue ("Cons", _, [h; t]) ->
            let%bind rest = convert_to_list t in
            pure @@ h :: rest
        | l -> fail @@ sprintf "The literal is not a list:\n%s" (pp_literal l))
      in       
      match v with
      | (Env.ValFix _ | Env.ValClosure _ | Env.ValTypeClosure _ ) as v ->
          fail @@
          sprintf "Value should be a list of messages, but is a closure:\n%s"
            (Env.pp_value v)
      | Env.ValLit l -> convert_to_list l 

  let send_messages conf ms =
    let%bind ls = get_list_literal ms in
    let old_emitted = conf.emitted in
    let emitted = old_emitted @ ls in
    pure {conf with emitted}

  let create_event conf ename eparams_resolved =
    let%bind event = 
      match eparams_resolved with
      | Env.ValLit l -> 
        (match l with
        | Msg _ ->
          (* An event is a named message. *)
          pure @@ (ename, l)
        | _ -> fail @@ sprintf "Incorrect event parameter(s): %s\n" (pp_literal l))
      | (Env.ValFix _ | Env.ValClosure _ | Env.ValTypeClosure _ ) as v -> 
        fail @@ sprintf "Incorrect event parameters: %s\n" (Env.pp_value v)
    in
    let old_events = conf.events in
    let events = event::old_events in
    pure {conf with events}
end

(*****************************************************)
(*         Contract state after initialization       *)
(*****************************************************)

module ContractState = struct

  type init_args = (string * literal) list 

  (* Runtime contract configuration and operations with it *)
  type 'rep t = {
    (* Immutable parameters *)
    env : 'rep Env.t;
    (* Contract fields *)
    fields : (string * literal) list;
    (* Contract balance *)
    balance : uint128;
  }

  (* Pretty-printing *)
  let pp cstate =
    let pp_params = Env.pp cstate.env in
    let pp_fields = pp_literal_map cstate.fields in
    let pp_balance = Uint128.to_string cstate.balance in
    sprintf "Contract State:\nImmutable parameters and libraries =\n%s\nMutable fields = \n%s\nBalance = %s\n"
      pp_params pp_fields pp_balance

end


