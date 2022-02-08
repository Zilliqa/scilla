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
open Type
open Literal
open Syntax
open MonadUtil
open Stdint
open Core_kernel.Result.Let_syntax
open PrettyPrinters
module CULiteral = GlobalLiteral
module CUType = CULiteral.LType
module CUIdentifier = CUType.TIdentifier
module CUName = CUIdentifier.Name

(*****************************************************)
(*                Message payload                    *)
(*****************************************************)

module MessagePayload = struct
  let tag_label = "_tag"

  let tag_type = CUType.string_typ

  let amount_label = "_amount"

  let amount_type = CUType.uint128_typ

  let sender_label = "_sender"

  let sender_type = CUType.address_typ AnyAddr

  let origin_label = "_origin"

  let origin_type = CUType.address_typ AnyAddr

  let recipient_label = "_recipient"

  let recipient_type = CUType.bystrx_typ address_length

  let eventname_label = "_eventname"

  let exception_label = "_exception"

  let get_value_for_entry lab f es =
    match List.find es ~f:(fun (x, _, _) -> String.(x = lab)) with
    | None ->
        fail0 ~kind:"Missing field in message"
          ~inst:
            (sprintf "No field \"%s\" in message [%s]." lab
               (pp_typ_literal_map es))
    | Some (_, _, p) ->
        f p
        |> Option.value
             ~default:
               (fail0
                  ~kind:(sprintf "Wrong value of the entry \"%s\"" lab)
                  ~inst:(pp_literal p))

  let get_tag =
    get_value_for_entry tag_label (function
      | StringLit s -> Some (pure s)
      | _ -> None)

  let get_sender =
    get_value_for_entry sender_label (function
      | ByStrX bs as a when CULiteral.Bystrx.width bs = address_length ->
          Some (pure a)
      | _ -> None)

  let get_origin =
    get_value_for_entry origin_label (function
      | ByStrX bs as a when CULiteral.Bystrx.width bs = address_length ->
          Some (pure a)
      | _ -> None)

  let get_amount =
    get_value_for_entry amount_label (function
      | UintLit (Uint128L i) -> (
          try
            if Uint128.(compare i zero) >= 0 then Some (pure i)
            else
              Some
                (fail0 ~kind:"Amount should be non-negative"
                   ~inst:(Uint128.to_string i))
          with Failure _ ->
            Some
              (fail0 ~kind:"Could not convert string to Stdint.Uint128"
                 ~inst:(Uint128.to_string i)))
      | _ -> None)

  let get_other_entries es =
    List.filter es ~f:(fun (l, _, _) -> String.(l <> tag_label))
end

(*****************************************************************)
(*               Blockchain component typing                     *)
(*****************************************************************)

let blocknum_name = "BLOCKNUMBER"

let chainid_name = "CHAINID"

let timestamp_name = "TIMESTAMP"

let blocknum_type = CUType.bnum_typ

(*****************************************************************)
(*           Init and outputjson component typing                *)
(*****************************************************************)

let label_name_of_string str = CUName.parse_simple_name str

let nonce_label = label_name_of_string "_nonce"

let nonce_type = CUType.uint64_typ

let balance_label = label_name_of_string "_balance"

let balance_type = CUType.uint128_typ

let creation_block_label = label_name_of_string "_creation_block"

let this_address_label = label_name_of_string "_this_address"

let scilla_version_label = label_name_of_string "_scilla_version"

let accepted_label = label_name_of_string "_accepted"

let extlibs_label = label_name_of_string "_extlibs"

let codehash_label = label_name_of_string "_codehash"

let codehash_type = CUType.bystrx_typ 32

let no_store_fields = [ balance_label ]

module ScillaContractUtil (SR : Rep) (ER : Rep) = struct
  module ContractUtilSyntax = ScillaSyntax (SR) (ER) (CULiteral)
  open ContractUtilSyntax

  let balance_field =
    (CUIdentifier.mk_id balance_label ER.uint128_rep, balance_type)

  let append_implicit_contract_params tparams =
    let open CUType in
    let creation_block =
      (CUIdentifier.mk_id creation_block_label ER.bnum_rep, bnum_typ)
    in
    let this_address =
      ( CUIdentifier.mk_id this_address_label ER.address_rep,
        bystrx_typ address_length )
    in
    let scilla_version_init =
      (CUIdentifier.mk_id scilla_version_label ER.uint32_rep, uint32_typ)
    in
    creation_block :: scilla_version_init :: this_address :: tparams

  (* Remove init arguments that the evaluator doesn't (need to) understand. *)
  let remove_noneval_args args =
    let nonevalargs = [ extlibs_label ] in
    List.filter args ~f:(fun a ->
        not (List.mem nonevalargs (fst a) ~equal:[%equal: CUName.t]))

  let append_implicit_comp_params cparams =
    let sender =
      ( CUIdentifier.mk_id
          (label_name_of_string MessagePayload.sender_label)
          ER.address_rep,
        MessagePayload.sender_type )
    in
    let origin =
      ( CUIdentifier.mk_id
          (label_name_of_string MessagePayload.origin_label)
          ER.address_rep,
        MessagePayload.origin_type )
    in
    let amount =
      ( CUIdentifier.mk_id
          (label_name_of_string MessagePayload.amount_label)
          ER.uint128_rep,
        MessagePayload.amount_type )
    in
    amount :: origin :: sender :: cparams

  let msg_mandatory_field_types =
    [
      (MessagePayload.tag_label, MessagePayload.tag_type);
      (MessagePayload.amount_label, MessagePayload.amount_type);
      (MessagePayload.recipient_label, MessagePayload.recipient_type);
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
          let%bind acc' =
            expr_folder (ER.get_loc @@ CUIdentifier.get_rep b') e1 acc
          in
          expr_folder loc e2 acc'
      | Fun (_, _, (e', _)) | TFun (_, (e', _)) | GasExpr (_, (e', _)) ->
          expr_folder loc e' acc
      | MatchExpr (p, pl) ->
          foldM
            ~f:(fun acc (_, (e', _)) ->
              expr_folder (ER.get_loc @@ CUIdentifier.get_rep p) e' acc)
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
              expr_folder (ER.get_loc @@ CUIdentifier.get_rep b) ex acc
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
                    expr_folder (ER.get_loc @@ CUIdentifier.get_rep b) e acc
                | _ -> (* Uninteresting statement. *) pure acc
              in
              stmt_iter stmt_list' acc'
          | [] -> pure acc
        in
        stmt_iter comp.comp_body acc)
      ~init:acc cmod.contr.ccomps
end
