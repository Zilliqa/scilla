(*
 * Copyright (c) 2018 - present Zilliqa, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open Syntax
open Core
open Result.Let_syntax
open MonadUtil

let balance = "balance"

  (*  Pretty-printing *)

let pp_literal_map s =
  let ps = List.map s
      ~f:(fun (k, v) -> sprintf " [%s -> %s]" k (pp_literal v)) in
  let cs = String.concat ~sep:",\n " ps in
  sprintf "{%s }" cs
    
let pp_literal_list ls =
  let ps = List.map ls
      ~f:(fun l -> sprintf " %s" (pp_literal l)) in
  let cs = String.concat ~sep:",\n " ps in
  sprintf "[ %s]" cs
    
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
    | ValClosure of 'rep Syntax.ident * typ * 'rep expr * 'rep t
  [@@deriving sexp]

  (* Pretty-printing *)
  let rec pp e =
    let ps = List.map e
        ~f:(fun (k, v) -> " [" ^ k ^ " -> " ^ (pp_value v) ^ "]") in
    let cs = String.concat ~sep:",\n " ps in
    "{" ^ cs ^ " }"
  and
    pp_value v = match v with
    | ValLit l -> sexp_of_literal l |> Sexplib.Sexp.to_string
    | ValClosure (f, t, e, env) -> "<closure>"
        (* (sexp_of_expr sexp_of_loc (Fun (f, t, e)) |> Sexplib.Sexp.to_string)
         * ^ ", " ^ (pp env)   *)

  let empty = []

  let bind e k v =
    (k, v) :: List.filter ~f:(fun z -> fst z <> k) e
                                                                
  let lookup e k =
    let i = get_id k in
    match List.find ~f:(fun z -> fst z = i) e with 
    | Some x -> pure @@ snd x
    | None -> fail @@ sprintf
        "Indentifier \"%s\" at %s is not bound in environment:\n%s"
        i (get_loc_str (get_loc k)) (pp e)
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
    balance : Big_int.big_int;
    (* Blockchain state *)
    blockchain_state : BlockchainState.t;
    (* Available incoming funds *)
    incoming_funds : Big_int.big_int;
    (* Emitted messages *)
    emitted : literal list;
    (* Emitted events *)
    events : (string * string) list
  }

  let pp conf =
    let pp_env = Env.pp conf.env in
    let pp_fields = pp_literal_map conf.fields in
    let pp_balance = Big_int.string_of_big_int conf.balance in
    let pp_bc_conf = pp_literal_map conf.blockchain_state in
    let pp_in_funds = Big_int.string_of_big_int conf.incoming_funds in
    let pp_emitted = pp_literal_list conf.emitted in
    let pp_events = String.concat ~sep:", " @@
        List.map conf.events (fun (e, b) -> sprintf "<%s, %s>" e b)
    in sprintf "Confuration\nEnv =\n%s\nFields =\n%s\nBalance =%s\nBlockchain conf =\n%s\nIncoming funds = %s\nEmitted Messages =\n%s\nEmitted events =\n%s\n"
      pp_env pp_fields pp_balance pp_bc_conf pp_in_funds pp_emitted pp_events

  (*  Manipulations with configuartion *)
  
  let store st k v =
    match v with 
    | Env.ValClosure _ ->
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
    if i = balance
    then
      (* Balance is a special case *)   
      pure @@ IntLit (Big_int.string_of_big_int st.balance)
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
    let open Big_int in
    let incoming' = st.incoming_funds in
    if ge_big_int incoming' zero_big_int
    then
      let balance = add_big_int st.balance incoming' in
      let incoming_funds = zero_big_int in
      pure @@ {st with balance; incoming_funds}
    else
      fail @@ sprintf "Incoming balance is negaitve (somehow):%s."
        (string_of_big_int incoming')

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
      | m :: tl -> fail @@ sprintf "This is not a message:\n%s" (pp_literal m)

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
      | Env.ValClosure _ as v ->
          fail @@
          sprintf "Value should be a list of messages, but is a closure:\n%s"
            (Env.pp_value v)
      | Env.ValLit l -> convert_to_list l 

  let send_messages conf ms =
    let%bind ls = get_list_literal ms in
    let old_emitted = conf.emitted in
    let emitted = old_emitted @ ls in
    pure {conf with emitted}
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
    balance : Big_int.big_int;
  }

  (* Pretty-printing *)
  let pp cstate =
    let pp_params = Env.pp cstate.env in
    let pp_fields = pp_literal_map cstate.fields in
    let pp_balance = Big_int.string_of_big_int cstate.balance in
    sprintf "Contract State:\nImmutable parameters and libraries =\n%s\nMutable fields = \n%s\nBalance = %s\n"
      pp_params pp_fields pp_balance

end

(*****************************************************)
(*                Message payload                    *)
(*****************************************************)

module MessagePayload = struct

  open Big_int

  let tag_label = "tag"
  let amount_label = "amount"
  let sender_label = "sender"

  let get_value_for_entry lab f es = 
    match List.find es ~f:(fun (l, p) -> l = lab) with
    | None -> fail @@ sprintf "No \"%s\" field in message [%s]."
          lab (pp_literal_map es)
    | Some (_, p) ->
        (match f p with 
         | Some x -> x
         | None -> fail @@ sprintf "Wrong value of the entry \"%s\": %s."
               lab (pp_literal p)) 

  let get_tag = get_value_for_entry tag_label
      (function StringLit s -> Some (pure s) | _ -> None)

  let get_sender = get_value_for_entry sender_label
      (function Address _ as a -> Some (pure a) | _ -> None)

  let get_amount = get_value_for_entry amount_label
      (function 
        | IntLit s ->
            (try
               let i = big_int_of_string s in
               let open Big_int in
               if ge_big_int i zero_big_int
               then Some (pure (big_int_of_string s))
               else
                 Some (fail @@ sprintf "Amount should be non-negative: %s" s)
             with
              | Failure _ -> Some (fail @@
                  sprintf "Could not convert string %s to big int." s))
        | _ -> None)
  
  let get_other_entries es =
    List.filter es ~f:(fun (l, _) -> l <> tag_label)
  
end


