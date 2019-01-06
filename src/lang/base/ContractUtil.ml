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

open Core
open Syntax
open MonadUtil
open Stdint
open Core.Result.Let_syntax
open PrettyPrinters

(*****************************************************)
(*                Message payload                    *)
(*****************************************************)

module MessagePayload = struct

  let tag_label = "_tag"
  let amount_label = "_amount"
  let sender_label = "_sender"
  let recipient_label = "_recipient"  
  let eventname_label = "_eventname"

  let get_value_for_entry lab f es = 
    match List.find es ~f:(fun (l, _) -> l = lab) with
    | None -> fail0 @@ sprintf "No field \"%s\" in message [%s]."
          lab (pp_literal_map es)
    | Some (_, p) ->
        (match f p with 
         | Some x -> x
         | None -> fail0 @@ sprintf "Wrong value of the entry \"%s\": %s."
               lab (pp_literal p)) 

  let get_tag = get_value_for_entry tag_label
      (function StringLit s -> Some (pure s) | _ -> None)

  let get_sender = get_value_for_entry sender_label
      (function ByStrX(len, _) as a when len = address_length -> Some (pure a) | _ -> None)

  let get_amount = get_value_for_entry amount_label
      (function 
        | UintLit (Uint128L i) ->
            (try
               if (compare i Uint128.zero) >= 0
               then Some (pure i)
               else
                 Some (fail0 @@ sprintf "Amount should be non-negative: %s" (Uint128.to_string i))
             with
              | Failure _ -> Some (fail0 @@
                  sprintf "Could not convert string %s to Stdint.Uint128." (Uint128.to_string i)))
        | _ -> None)
  
  let get_other_entries es =
    List.filter es ~f:(fun (l, _) -> l <> tag_label)
  
end

let balance_label = "_balance"
let creation_block_label = "_creation_block"
let this_address_label = "_this_address"
let scilla_version_label = "_scilla_version"
let accepted_label = "_accepted"

let no_store_fields =
  [balance_label]


module ScillaContractUtil
    (SR : Rep)
    (ER : Rep) = struct

  module ContractUtilSyntax = ScillaSyntax (SR) (ER)
  open ContractUtilSyntax

  let balance_field =
    let open PrimTypes in
    (ER.mk_id_uint128 balance_label, uint128_typ)

  let append_implict_contract_params tparams =
    let open PrimTypes in 
    let creation_block = (ER.mk_id_bnum creation_block_label, bnum_typ) in
    let this_address = (ER.mk_id_address this_address_label, bystrx_typ address_length) in
    let scilla_version_init = (ER.mk_id_uint32 scilla_version_label, uint32_typ) in
    creation_block :: scilla_version_init :: this_address :: tparams

  let append_implict_trans_params tparams =
    let open PrimTypes in
    let sender = (ER.mk_id_address MessagePayload.sender_label, bystrx_typ address_length) in
    let amount = (ER.mk_id_uint128 MessagePayload.amount_label, uint128_typ) in
    amount :: sender :: tparams

  (* Iterate over all messages in the contract, accumuating result. 
   * ~f takes a message and an accumulator and updates the accumulator. *)
  let fold_over_messages contr ~init ~f =
    (* Loop through each transition *)
    foldM ~f:(fun acc trans ->
        (* Loop through each statement, looking for messages. *)
        let rec stmt_iter stmt_list acc = 
          match stmt_list with
          | (stmt, _)::stmt_list' -> 
              let%bind acc' =
                (match stmt with
                 | MatchStmt (_, clauses) ->
                     (* Recurse through all clauses. *)
                     foldM ~f:(fun acc'' (_, stmt_list'') ->
                         stmt_iter stmt_list'' acc''
                       ) ~init:acc clauses
                 (* Every message created gets bound to some variable. *)
                 | Bind (b, (e, _)) ->
                     (match e with
                      | Message m -> f b m acc
                      | _ -> (* Uninteresting expression. *) pure acc
                     )
                 | _ -> (* Uninteresting statement. *) pure acc
                ) in  stmt_iter stmt_list' acc'
          | [] -> pure acc
        in
        stmt_iter trans.tbody acc
      ) ~init:init contr.ctrans
    
end

