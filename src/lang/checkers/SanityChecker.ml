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

open TypeUtil
open Syntax
open ErrorUtils
open MonadUtil

open ContractUtil.MessagePayload
open Core.Result.Let_syntax

module ScillaSanityChecker
    (SR : Rep)
    (ER : sig
       include Rep
       val get_type : rep -> PlainTypes.t inferred_type
     end) = struct

  module SER = SR
  module EER = ER
  module EISyntax = ScillaSyntax (SR) (ER)
  module TU = TypeUtilities (SR) (ER)
  module SCU = ContractUtil.ScillaContractUtil (SR) (ER)

  open EISyntax
  open SCU

  (* Basic sanity tests on the contract. *)
  let contr_sanity (contr : contract) =

    (* Check if there are duplicate entries in "ilist". *)
    let check_duplicate_ident gloc ilist =
      let rec recurser ilist' e =
        match ilist' with
        | i :: rem ->
          let e' =
            if (List.exists (fun x -> get_id x = get_id i) rem)
            then
              e @ mk_error1 (Core.sprintf "Identifier %s used more than once\n" (get_id i)) (gloc @@ get_rep i)
            else e
          in
            recurser rem e'
        | [] -> e
      in
        recurser ilist []
    in

    (* No repeating names for params. *)
    let e = check_duplicate_ident ER.get_loc (List.map (fun (i, _) -> i) contr.cparams) in
    (* No repeating field names. *)
    let e = e @ check_duplicate_ident ER.get_loc (List.map (fun (i, _, _) -> i) contr.cfields) in
    (* No repeating transition names. *)
    let e = e @ check_duplicate_ident SR.get_loc (List.map (fun t -> t.tname) contr.ctrans) in
    (* No repeating transition parameter names. *)
    let e = List.fold_left
      (fun e t -> e @ check_duplicate_ident ER.get_loc (List.map (fun (i, _) -> i) t.tparams))
       e contr.ctrans 
    in

    (* Message literals must either be for "send" or "event" and well formed. *)
    let check_message b msg e =
      (* Use location of "b" to represent the location of msg. *)
      let eloc = ER.get_loc @@ get_rep b in

      (* No repeating message field. TODO: use eloc below as "msg" has no location info. *)
      let e = e @ check_duplicate_ident SR.get_loc (List.map (fun (s, _) -> SR.mk_id_string s) msg) in

      (* Either "_tag" or "_eventname" must be present. *)
      let e = if (List.exists (fun (s, _) -> s = tag_label) msg)
      then
        (* This is a "send" Message. Ensure "_amount" and "_recipient" provided. *)
        if List.exists (fun (s, _) -> s = amount_label) msg &&
           List.exists (fun (s, _) -> s = recipient_label) msg 
        then e 
        else e @ mk_error1 ("Missing " ^ amount_label ^ " or " ^ recipient_label ^ " in Message\n") eloc
      else
        (* This is an "event" message, and must have "_eventname" field. *)
        if List.exists (fun (s, _) -> s = eventname_label) msg
        then e
        else e @ mk_error1 ("Missing " ^ eventname_label ^ " field in message\n") eloc
      in
        pure e (* as required by "fold_over_messages" *)
    in
    let%bind e = fold_over_messages contr ~init:e ~f:check_message in

    (* Transition parameters cannot have names as that of implicit ones. *)
    let e = List.fold_left (fun e t -> 
      match List.find_opt (fun (s, _) -> get_id s = amount_label || get_id s = sender_label) t.tparams with
      | Some (s, _) ->
        e @ mk_error1 (Core.sprintf "Paramter %s in transition %s cannot be explicit.\n" 
                          (get_id s) (get_id t.tname)) 
                      (SR.get_loc @@ get_rep t.tname)
      | None -> e
      ) e contr.ctrans in

    (* Contract parameters cannot have names of implicit ones. *)
    let e = 
      match (List.find_opt (fun (s, _) ->
          (get_id s = ContractUtil.creation_block_label) || (get_id s = ContractUtil.scilla_version_label)
          || (get_id s = ContractUtil.this_address_label)
        ) contr.cparams) with
      | Some (s, _) ->
        e @ mk_error1 (Core.sprintf "Contract parameter %s cannot be explicit.\n" (get_id s))
            (ER.get_loc @@ get_rep s) 
      | None -> e
    in

    if e = [] then pure () else fail e

end
