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
open Syntax
open ShardingAnalysis
open ErrorUtils
open PrettyPrinters
open Stdint
open Integer256
open BuiltIns
open TypeUtil
module PSRep = ParserRep
module PERep = ParserRep
module Rec = Recursion.ScillaRecursion (PSRep) (PERep)
module RecSRep = Rec.OutputSRep
module RecERep = Rec.OutputERep
module TC = TypeChecker.ScillaTypechecker (RecSRep) (RecERep)
module TCSRep = TC.OutputSRep
module TCERep = TC.OutputERep
module SA = ScillaSA (TCSRep) (TCERep)
module BuiltIns = ScillaBuiltIns (PSRep) (PERep)
module TU = TypeUtilities

let acc_addr_size = 20

let state_index_separator = Char.of_int_exn 0x16

(* Painful: "\u0016" is an escaped character in C++ and JSON, but not in OCaml. The OCaml
  equivalent is "\u{0016}". We have this so we can manually translate. *)
let cpp_index_separator = "\\u0016"

let ocaml_index_separator = String.of_char state_index_separator

let get_shard_req = "get_shard"

let join_req = "join"

let invalid_request = "invalid_request"

let literal_log = ref []

type state_fragment = string

type shard_id = int

module type StateJoiner = sig
  val applicable_to : SA.pcm_ident

  val join :
    Syntax.typ ->
    state_fragment ->
    state_fragment ->
    state_fragment ->
    state_fragment
end

module StateSplitJoiner = struct
  let applicable_to = SA.State_Split_PCM.pcm_identifier

  (* Overwrite *)
  let join _ _ _ shard = shard
end

module IntegerAddJoiner = struct
  let applicable_to = SA.Integer_Addition_PCM.pcm_identifier

  let join typ ancestor temp shard =
    let zero_empty x = if String.length x = 0 then "0" else x in
    let ancestor, temp, shard =
      (zero_empty ancestor, zero_empty temp, zero_empty shard)
    in
    match typ with
    | PrimType (Int_typ _ | Uint_typ _) -> (
        let conv x =
          (* The values we get from Zilliqa AccountStore are quoted strings! *)
          let xs = String.strip ~drop:(fun c -> Char.compare c '"' = 0) x in
          JSON.build_prim_lit_exn typ xs
        in
        let an, tm, sh = (conv ancestor, conv temp, conv shard) in
        let add, sub =
          match
            ( BuiltIns.BuiltInDictionary.find_builtin_op
                (Builtin_add, dummy_loc) [ typ; typ ],
              BuiltIns.BuiltInDictionary.find_builtin_op
                (Builtin_sub, dummy_loc) [ typ; typ ] )
          with
          | Ok (_, _, add), Ok (_, _, sub) -> (add, sub)
          | _ ->
              raise
                (mk_internal_error
                   (Printf.sprintf
                      "Could not find add/sub operations for type %s"
                      (pp_typ typ)))
        in
        (* merged result = temp + (shard - ancestor) *)
        let merged =
          let diff =
            match sub [ sh; an ] typ with
            | Ok res -> res
            | Error _ ->
                raise
                  (mk_internal_error "Substraction (shard - ancestor) failed")
          in
          let final =
            match add [ tm; diff ] typ with
            | Ok res -> res
            | Error _ ->
                raise (mk_internal_error "Addition (temp + diff) failed")
          in
          final
        in
        match JSON.get_integer_literal merged with
        (* The values we get from Zilliqa AccountStore are quoted strings! *)
        | Some s -> "\"" ^ s ^ "\""
        | None ->
            raise
              (mk_internal_error "Cannot convert IntegerAdd merged to string") )
    (* This really shouldn't happen *)
    | _ ->
        raise
          (mk_internal_error "IntegerAdd join called on non-integer arguments")
end

let overwrite_join = (module StateSplitJoiner : StateJoiner)

let integer_add_join = (module IntegerAddJoiner : StateJoiner)

let joiners = [ overwrite_join; integer_add_join ]

(* state_is_owned is only relevant for composite PCMs,
    e.g. owner_ovewrites+integer_add *)
let find_join_function state_is_owned pcm_name =
  let find_join searched_pcm =
    let applicable_joiners =
      List.filter joiners ~f:(fun (module P : StateJoiner) ->
          String.compare P.applicable_to searched_pcm = 0)
    in
    let join_functions =
      List.map applicable_joiners ~f:(fun (module P : StateJoiner) -> P.join)
    in
    match List.hd join_functions with
    | Some jf -> jf
    | None ->
        raise (mk_internal_error ("No join function found for " ^ pcm_name))
  in
  (* Handle composite PCMs via compensating functions *)
  (* Example:
      overwrite+integer_add : join =
        if own -> shard + (temp - ancestor)		(write + compensation)
        else -> temp + (shard - ancestor)	  	(integer_add)
  *)
  let pcm_parts = String.split_on_chars pcm_name ~on:[ '+' ] in
  let is_composite = List.length pcm_parts = 2 in
  match (is_composite, state_is_owned) with
  | false, _ -> find_join pcm_name
  (* Own the state: must apply compensating function *)
  | true, true ->
      let pcm = List.nth_exn pcm_parts 1 in
      let raw_join = find_join pcm in
      (* Arguments intentionally swapped: that's how a compensation function is defined! *)
      (* TODO: find better way to name arguments when compensating *)
      (* Maybe temp = main_source and shard = diff_source i.e. diff = shard/diff_source - ancestor *)
      let compensated_join typ ancestor temp shard =
        raw_join typ ancestor shard temp
      in
      compensated_join
  (* Don't own the state *)
  | true, false ->
      let pcm = List.nth_exn pcm_parts 1 in
      let raw_join = find_join pcm in
      raw_join

type shard_allocation = AnyShard | Shard of int | DSShard

let pp_shard_allocation sa =
  match sa with
  | AnyShard -> "AnyShard"
  | Shard i -> "Shard " ^ string_of_int i
  | DSShard -> "DSShard"

let shard_combine sa sb =
  match (sa, sb) with
  | AnyShard, x | x, AnyShard -> x
  | DSShard, _ | _, DSShard -> DSShard
  | Shard a, Shard b -> if a = b then Shard a else DSShard

let int_lit_to_shard il (num_shards : int) =
  let ns = Int256.of_string (string_of_int num_shards) in
  let i =
    match il with
    | Int32L i -> Int256.of_string (Int32.to_string i)
    | Int64L i -> Int256.of_string (Int64.to_string i)
    | Int128L i -> Int256.of_string (Int128.to_string i)
    | Int256L i -> i
  in
  let shard = Int.of_string @@ Int256.to_string @@ Int256.rem i ns in
  shard

let uint_lit_to_shard il (num_shards : int) =
  let ns = Uint256.of_string (string_of_int num_shards) in
  let i =
    match il with
    | Uint32L i -> Uint256.of_string (Uint32.to_string i)
    | Uint64L i -> Uint256.of_string (Uint64.to_string i)
    | Uint128L i -> Uint256.of_string (Uint128.to_string i)
    | Uint256L i -> i
  in
  let shard = Int.of_string @@ Uint256.to_string @@ Uint256.rem i ns in
  shard

(* This doesn't necessarily have to match the protocol's address to shard
allocation. *)
let addr_to_shard (addr : string) (con_shard : int) (num_shards : int) =
  let addr_bytes = Bytes.of_string addr in
  let bytes_length = Bytes.length addr_bytes in
  let last_n_bytes = 4 in
  if bytes_length < last_n_bytes then con_shard
  else
    let offset = bytes_length - last_n_bytes in
    let ai = Uint32.of_bytes_big_endian addr_bytes offset in
    literal_log := Printf.sprintf "ai: %d" (Uint32.to_int ai) :: !literal_log;
    let ns = Uint32.of_string (string_of_int num_shards) in
    let shard = Int.of_string @@ Uint32.to_string @@ Uint32.rem ai ns in
    shard

let literal_to_shard (lit : Syntax.literal) con_shard num_shards =
  let si =
    match lit with
    | IntLit il -> int_lit_to_shard il num_shards
    | UintLit il -> uint_lit_to_shard il num_shards
    | ByStr s ->
        let h = Bystr.to_raw_bytes s in
        addr_to_shard h con_shard num_shards
    | ByStrX s ->
        let h = Bystrx.to_raw_bytes s in
        addr_to_shard h con_shard num_shards
    | _ -> con_shard
  in
  let log_str = pp_literal lit ^ " --> " ^ string_of_int si in
  literal_log := log_str :: !literal_log;
  si

(* Allocate a pseudofield access to a shard *)
(* Possible extension: pf_to_shard can be provided by contract writer *)
(* IMPORTANT: pf_to_shard and concrete_pf_to_shard should have the same behaviour! *)
let pf_to_shard (pf : SA.pseudofield) con_shard num_shards params =
  let default = con_shard in
  match pf with
  (* normal field access *)
  | _, None -> default
  (* map access; shard based on first key's value *)
  | _, Some (k :: _) -> (
      let kv =
        List.find params ~f:(fun (n, _) -> String.compare n (get_id k) = 0)
      in
      match kv with
      | Some (_, v) -> literal_to_shard v con_shard num_shards
      | None -> default )
  (* This shouldn't happen *)
  | _, Some [] -> default

(* Concrete pseudofield = map keys are replaced with their values! *)
(* IMPORTANT: pf_to_shard and concrete_pf_to_shard should have the same behaviour! *)
let concrete_pf_to_shard (pf : SA.pseudofield) field_type con_shard num_shards =
  let default = con_shard in
  match (pf, field_type) with
  | (_, Some (k :: _)), MapType (kt, _) ->
      let key_literal = JSON.build_prim_lit_exn kt (get_id k) in
      literal_to_shard key_literal con_shard num_shards
  | _ -> default

let get_shard req_data =
  let ( (sender_shard, con_shard, ds_shard, num_shards),
        tr_constrs,
        param_contracts,
        params ) =
    req_data
  in
  let sc_to_shard sc =
    match sc with
    | SA.CUnsat -> DSShard
    | SA.CSenderShard -> Shard sender_shard
    | SA.CContractShard -> Shard con_shard
    | SA.CMustBeUserAddr i ->
        let is_con =
          List.mem param_contracts (get_id i) ~equal:(fun a b ->
              String.compare a b = 0)
        in
        if is_con then DSShard else AnyShard
    | SA.CMustNotHaveDuplicates dl ->
        let have_dups (a, b) =
          let find x =
            List.find params ~f:(fun (n, _) -> String.compare n (get_id x) = 0)
          in
          let fa, fb = (find a, find b) in
          match (fa, fb) with
          | Some (_, va), Some (_, vb) ->
              String.compare (pp_literal va) (pp_literal vb) = 0
          (* If parameters not found, the transaction is malformed and will be
             rejected by whoever we send it to *)
          | _ -> false
        in
        let dups_exist =
          List.for_all (List.map dl ~f:have_dups) ~f:(fun x ->
              Bool.compare x true = 0)
        in
        if dups_exist then DSShard else AnyShard
    | SA.CAccess pf -> Shard (pf_to_shard pf con_shard num_shards params)
  in
  let sh_alloc = List.map tr_constrs ~f:sc_to_shard in
  let shard = List.fold_left sh_alloc ~init:AnyShard ~f:shard_combine in
  let sh_log =
    List.map2_exn tr_constrs sh_alloc ~f:(fun c sh ->
        SA.pp_sharding_constraint c ^ " -> " ^ pp_shard_allocation sh)
  in
  let shard_id =
    match shard with
    | AnyShard -> sender_shard
    | Shard i -> i
    | DSShard -> ds_shard
  in
  (sh_log, shard_id)

let make_get_shard_resp _ sh_id =
  (* let log_json = List.map sh_log (fun s -> `String s) in
     let lit_log_json = List.map !literal_log (fun s -> `String s) in *)
  let json =
    `Assoc
      [
        (* ("log", `List log_json);
           ("lit_log", `List lit_log_json); *)
        ("shard", `Int sh_id);
      ]
  in
  Yojson.Basic.to_string json

let state_to_concrete_pseudofield st =
  (* Work-around a bug in the JSON parsing of the RPC library? *)
  let st =
    String.substr_replace_all st ~pattern:cpp_index_separator
      ~with_:ocaml_index_separator
  in
  (* Workaround: replace quotes *)
  let st = String.substr_replace_all st ~pattern:"\"" ~with_:"" in
  let st_components = String.split st ~on:state_index_separator in
  (* Filter out empty components, esp. the one at the end *)
  let st_components =
    List.filter st_components ~f:(fun s -> not @@ String.equal s "")
  in
  (* literal_log := Printf.sprintf "Components: %s" (String.concat ~sep:"|" st_components) :: !literal_log; *)
  let tl_stc = List.tl st_components in
  match tl_stc with
  | Some tl_stc -> (
      let st_strip_addr =
        String.concat ~sep:(String.of_char state_index_separator) tl_stc
      in
      (* literal_log := Printf.sprintf "st_strip_addr: %s" st_strip_addr :: !literal_log; *)
      let res =
        SA.pp_of_str ~extra_sep:[ state_index_separator ] st_strip_addr
      in
      match res with
      | Ok pf -> pf
      | Error _ ->
          raise (mk_invalid_json (Printf.sprintf "Invalid state name %s" st)) )
  | None -> raise (mk_invalid_json (Printf.sprintf "Invalid state name %s" st))

let join req_data =
  let (con_shard, num_shards), shard_id, field_pcms, states = req_data in
  (* let field_names = List.map field_pcms (fun (a, _) -> a)  in *)
  (* literal_log := Printf.sprintf "Field names: %s" (String.concat ~sep:", " field_names) :: !literal_log; *)
  let join_state (st_id, ancestor, temp, shard) =
    let field, keys = state_to_concrete_pseudofield st_id in
    let field_name = get_id field in
    literal_log := Printf.sprintf "%s => %s" st_id field_name :: !literal_log;
    (* Just overwrite fields that are managed by the blockchain rather
       than the contract, e.g. map_depth and sharding_info *)
    let opt_found = List.Assoc.find field_pcms ~equal:String.equal field_name in
    match opt_found with
    | Some (pcm_name, field_type) ->
        let field_type =
          match FrontEndParser.parse_type field_type with
          | Ok t -> t
          | Error _ ->
              raise
                (mk_invalid_json (Printf.sprintf "Invalid type %s" field_type))
        in
        let state_owner_shard =
          concrete_pf_to_shard (field, keys) field_type con_shard num_shards
        in
        let state_is_owned = Int.equal state_owner_shard shard_id in
        (* literal_log :=
           Printf.sprintf "%s -> owner_id: %d | shard_id: %d" st_id
             state_owner_shard shard_id
           :: !literal_log; *)
        let join_fn = find_join_function state_is_owned pcm_name in
        let bottom_field_type = TU.map_bottom_type field_type in
        let joined = join_fn bottom_field_type ancestor temp shard in
        (st_id, joined)
    | None -> (st_id, shard)
    (* raise (mk_internal_error ("Could not find sharding information for field " ^ field_name)) *)
  in
  List.map states ~f:join_state

let make_join_resp states =
  (* let lit_log_json = List.map !literal_log (fun s -> `String s) in *)
  let states_dict =
    `Assoc (List.map states ~f:(fun (st_id, joined) -> (st_id, `String joined)))
  in
  let json =
    `Assoc [ ("states", states_dict) (* ("lit_log", `List lit_log_json) *) ]
  in
  Yojson.Basic.to_string json

let run req ~exe_name =
  literal_log := [ exe_name ];
  match req with
  | Some req_str ->
      let req_type =
        try JSON.ShardingInfo.get_request_type req_str
        with Invalid_json s -> sprint_scilla_error_list s
      in
      if String.compare req_type get_shard_req = 0 then
        let err, req_data =
          try
            ( "",
              Some
                (JSON.ShardingInfo.get_shard_request_data req_str
                   SA.json_to_sharding_constraint) )
          with Invalid_json s -> (sprint_scilla_error_list s, None)
        in
        match req_data with
        | Some req_data ->
            let sh_log, sh_id = get_shard req_data in
            make_get_shard_resp sh_log sh_id
        | None -> err
      else if String.compare req_type join_req = 0 then
        let err, req_data =
          try ("", Some (JSON.ShardingInfo.get_join_request_data req_str))
          with Invalid_json s | InternalError s ->
            (sprint_scilla_error_list s, None)
        in
        match req_data with
        | Some req_data ->
            let states = join req_data in
            make_join_resp states
        | None -> err
      else invalid_request
  | None -> invalid_request
