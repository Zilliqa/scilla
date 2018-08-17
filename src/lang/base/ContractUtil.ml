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


(*****************************************************)
(*                Message payload                    *)
(*****************************************************)

module MessagePayload = struct

  let tag_label = "_tag"
  let amount_label = "_amount"
  let sender_label = "_sender"
  let recipient_label = "_recipient"
  let accepted_label = "_accepted"

  let get_value_for_entry lab f es = 
    match List.find es ~f:(fun (l, _) -> l = lab) with
    | None -> fail @@ sprintf "No field \"%s\" in message [%s]."
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
        | UintLit (ws, s) ->
            (try
               let open Uint128 in
               let i = of_string s in
               if (compare i zero) >= 0 && ws = 128
               then Some (pure i)
               else
                 Some (fail @@ sprintf "Amount should be non-negative: %s" s)
             with
              | Failure _ -> Some (fail @@
                  sprintf "Could not convert string %s to Stdint.Uint128." s))
        | _ -> None)
  
  let get_other_entries es =
    List.filter es ~f:(fun (l, _) -> l <> tag_label)
  
end

let append_implict_trans_params tparams mk_id =
  let open PrimTypes in
  let sender = (mk_id MessagePayload.sender_label, uint128_typ) in
  let amount = (mk_id MessagePayload.amount_label, uint128_typ) in
  amount :: sender :: tparams

let balance_label = "_balance"
let creation_block_label = "_creation_block"

let no_store_fields =
  [balance_label]

let append_implict_contract_params tparams =
  let open PrimTypes in 
  let creation_block_id = asId creation_block_label in
  let creation_block = (creation_block_id, bnum_typ) in
  creation_block :: tparams

let balance_field =
  let open PrimTypes in 
  let balance_id = asId balance_label in
  (balance_id, uint128_typ)
