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
open! Int.Replace_polymorphic_compare
open Identifier
open Literal
open Syntax
open MonadUtil
open Stdint
open Core_kernel.Result.Let_syntax
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

  let exception_label = "_exception"

  let get_value_for_entry lab f es =
    match List.Assoc.find es lab ~equal:String.( = ) with
    | None ->
        fail0
        @@ sprintf "No field \"%s\" in message [%s]." lab (pp_literal_map es)
    | Some p ->
        f p
        |> Option.value
             ~default:
               ( fail0
               @@ sprintf "Wrong value of the entry \"%s\": %s." lab
                    (pp_literal p) )

  let get_tag =
    get_value_for_entry tag_label (function
      | StringLit s -> Some (pure s)
      | _ -> None)

  let get_sender =
    get_value_for_entry sender_label (function
      | ByStrX bs as a when Bystrx.width bs = address_length -> Some (pure a)
      | _ -> None)

  let get_amount =
    get_value_for_entry amount_label (function
      | UintLit (Uint128L i) -> (
          try
            if Uint128.(compare i zero) >= 0 then Some (pure i)
            else
              Some
                ( fail0
                @@ sprintf "Amount should be non-negative: %s"
                     (Uint128.to_string i) )
          with Failure _ ->
            Some
              ( fail0
              @@ sprintf "Could not convert string %s to Stdint.Uint128."
                   (Uint128.to_string i) ) )
      | _ -> None)

  let get_other_entries es =
    List.filter es ~f:(fun (l, _) -> String.(l <> tag_label))
end

let balance_label = "_balance"

let creation_block_label = "_creation_block"

let this_address_label = "_this_address"

let scilla_version_label = "_scilla_version"

let accepted_label = "_accepted"

let extlibs_label = "_extlibs"

let no_store_fields = [ balance_label ]

module ScillaContractUtil (SR : Rep) (ER : Rep) = struct
  module ContractUtilSyntax = ScillaSyntax (SR) (ER)
  open ContractUtilSyntax

  let balance_field =
    let open PrimTypes in
    (asIdL balance_label ER.uint128_rep, uint128_typ)

  let append_implict_contract_params tparams =
    let open PrimTypes in
    let creation_block = (asIdL creation_block_label ER.bnum_rep, bnum_typ) in
    let this_address =
      (asIdL this_address_label ER.address_rep, bystrx_typ address_length)
    in
    let scilla_version_init =
      (asIdL scilla_version_label ER.uint32_rep, uint32_typ)
    in
    creation_block :: scilla_version_init :: this_address :: tparams

  (* Remove init arguments that the evaluator doesn't (need to) understand. *)
  let remove_noneval_args args =
    let nonevalargs = [ extlibs_label ] in
    List.filter args ~f:(fun a ->
        not (List.mem nonevalargs (fst a) ~equal:String.( = )))

  let append_implict_comp_params cparams =
    let open PrimTypes in
    let sender =
      ( asIdL MessagePayload.sender_label ER.address_rep,
        bystrx_typ address_length )
    in
    let amount =
      (asIdL MessagePayload.amount_label ER.uint128_rep, uint128_typ)
    in
    amount :: sender :: cparams

  let msg_mandatory_field_types =
    [
      (MessagePayload.tag_label, PrimTypes.string_typ);
      (MessagePayload.amount_label, PrimTypes.uint128_typ);
      (MessagePayload.recipient_label, PrimTypes.bystrx_typ address_length);
    ]

  (* Iterate over all messages in the contract, accumuating result. 
   * ~f takes a message and an accumulator and updates the accumulator. *)
  let fold_over_messages (cmod : cmodule) ~init ~f =
    let rec expr_folder loc ex acc =
      match ex with
      (* Basis of the recursion. *)
      | Message m -> f loc m acc
      (* More, unimportant bases. *)
      | Literal _ | Var _ | App _ | Constr _ | Builtin _ | TApp _ -> pure acc
      (* We don't really expect Message inside a Fixpoint. *)
      | Fixpoint _ -> pure acc
      (* Recursion. *)
      | Let (b', _, (e1, _), (e2, _)) ->
          let%bind acc' = expr_folder (ER.get_loc @@ get_rep b') e1 acc in
          expr_folder loc e2 acc'
      | Fun (_, _, (e', _)) | TFun (_, (e', _)) -> expr_folder loc e' acc
      | MatchExpr (p, pl) ->
          foldM
            ~f:(fun acc (_, (e', _)) ->
              expr_folder (ER.get_loc @@ get_rep p) e' acc)
            ~init:acc pl
    in

    (* Loop over each library entry. *)
    let%bind acc =
      let lentries =
        match cmod.libs with None -> [] | Some lib -> lib.lentries
      in
      foldM
        ~f:(fun acc le ->
          match le with
          | LibVar (b, _, (ex, _)) ->
              expr_folder (ER.get_loc @@ get_rep b) ex acc
          | LibTyp _ -> pure acc)
        ~init lentries
    in

    (* Loop through each component. *)
    foldM
      ~f:(fun acc comp ->
        (* Loop through each statement, looking for messages. *)
        let rec stmt_iter stmt_list acc =
          match stmt_list with
          | (stmt, _) :: stmt_list' ->
              let%bind acc' =
                match stmt with
                | MatchStmt (_, clauses) ->
                    (* Recurse through all clauses. *)
                    foldM
                      ~f:(fun acc'' (_, stmt_list'') ->
                        stmt_iter stmt_list'' acc'')
                      ~init:acc clauses
                (* Every message created gets bound to some variable. *)
                | Bind (b, (e, _)) ->
                    expr_folder (ER.get_loc @@ get_rep b) e acc
                | _ -> (* Uninteresting statement. *) pure acc
              in
              stmt_iter stmt_list' acc'
          | [] -> pure acc
        in
        stmt_iter comp.comp_body acc)
      ~init:acc cmod.contr.ccomps
end
