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
open Scilla_base
open ParserUtil
open Literal
open Syntax
open ErrorUtils
open Eval
open DebugMessage
open ContractUtil
open PrettyPrinters
open Stdint
open RunnerUtil
open RunnerCLI
open GlobalConfig
module RG = Gas.ScillaGas (ParserUtil.ParserRep) (ParserUtil.ParserRep)
module FEParser = FrontEndParser.ScillaFrontEndParser (LocalLiteral)
module Dis = Disambiguate.ScillaDisambiguation (ParserRep) (ParserRep)
module RunnerSyntax = Dis.PostDisSyntax
module RunnerType = RunnerSyntax.SType
module RunnerName = RunnerSyntax.SIdentifier.Name

(****************************************************)
(*          Checking initialized libraries          *)
(****************************************************)

let check_libs clibs elibs name gas_limit =
  let ls = init_libraries clibs elibs in
  (* Are libraries ok? *)
  match ls Eval.init_gas_kont gas_limit with
  | Ok (res, gas_remaining) ->
      plog
        (sprintf
           "\n\
            [Initializing libraries]:\n\
            %s\n\n\
            Libraries for [%s] are on. All seems fine so far!\n\n"
           (* (Env.pp res) *)
           (String.concat ~sep:", "
              (List.rev_map res ~f:(fun x ->
                   EvalUtil.EvalName.as_string (fst x))))
           name);
      (res, gas_remaining)
  | Error (err, gas_remaining) ->
      fatal_error_gas_scale Gas.scale_factor err gas_remaining

let check_contr_wrapper libs_env cconstraint cfields initargs curargs gas_limit
    =
  let ls = check_contr libs_env cconstraint cfields initargs curargs in
  match ls Eval.init_gas_kont gas_limit with
  | Ok (res, gas_remaining) -> (res, gas_remaining)
  | Error (err, gas_remaining) ->
      fatal_error_gas_scale Gas.scale_factor err gas_remaining

(****************************************************)
(*     Checking initialized contract state          *)
(****************************************************)
let check_extract_cstate name res gas_limit =
  match res Eval.init_gas_kont gas_limit with
  | Error (err, remaining_gas) ->
      fatal_error_gas_scale Gas.scale_factor err remaining_gas
  | Ok ((_, cstate, dyn_checks), remaining_gas) ->
      plog (sprintf "[Initializing %s's fields]\nSuccess!\n" name);
      (cstate, remaining_gas, dyn_checks)

(****************************************************)
(*           Checking prepared message              *)
(****************************************************)
let check_prepare_message res gas_limit =
  match res Eval.init_gas_kont gas_limit with
  | Error (err, remaining_gas) ->
      fatal_error_gas_scale Gas.scale_factor err remaining_gas
  | Ok ((preppred_msg, dyn_checks), remaining_gas) ->
      plog "[Preparing message]\nSuccess!\n";
      (preppred_msg, dyn_checks, remaining_gas)

(*****************************************************)
(*   Running the simulation and printing results     *)
(*****************************************************)

let check_after_step res gas_limit =
  match res Eval.init_gas_kont gas_limit with
  | Error (err, remaining_gas) ->
      fatal_error_gas_scale Gas.scale_factor err remaining_gas
  | Ok ((cstate, outs, events, accepted_b), remaining_gas) ->
      plog
        (sprintf "Success! Here's what we got:\n"
        (* sprintf "%s" (ContractState.pp cstate) ^ *)
        ^ sprintf "Emitted messages:\n%s\n\n" (pp_literal_list outs)
        ^ sprintf "Gas remaining:%s\n" (Uint64.to_string remaining_gas)
        ^ sprintf "Emitted events:\n%s\n\n" (pp_literal_list events));
      ((cstate, outs, events, accepted_b), remaining_gas)

let map_input_string_to_name x =
  match String.split x ~on:'.' with
  | [ simple_name ] -> RunnerName.parse_simple_name simple_name
  | _ -> raise (mk_invalid_json ~kind:"invalid name in json input" ~inst:x)
  
(* Parse the input state json and extract out _balance separately *)
let input_state_json filename =
  let open JSON.ContractState in
  let states_str, estates_str = get_json_data filename in
  let states =
    List.map states_str ~f:(fun (x, t, l) -> (map_input_string_to_name x, t, l))
  in
  let estates =
    List.map estates_str ~f:(fun (addr, states_str) ->
        (addr, List.map states_str ~f:(fun (x, m, t, l) -> (map_input_string_to_name x, m, t, l))))
  in
  let bal_lit =
    match
      List.find states ~f:(fun (x, _, _) ->
          [%equal: RunnerName.t] x balance_label)
    with
    | Some v -> v
    | None ->
        raise
        @@ mk_invalid_json ~kind:"Field missing"
             ~inst:(RunnerName.as_string balance_label)
  in
  let bal_int =
    match bal_lit with
    | _, t, UintLit (Uint128L x)
      when [%equal: RunnerSyntax.SType.t] t balance_type ->
        x
    | _ ->
        raise
          (mk_invalid_json
             ~kind:(RunnerName.as_string balance_label ^ " invalid")
             ?inst:None)
  in
  let no_bal_states =
    List.filter states ~f:(fun (x, _, _) ->
        not @@ [%equal: RunnerName.t] x balance_label)
  in
  (no_bal_states, bal_int, estates)

(* Add balance to output json and print it out *)
let output_state_json balance field_vals =
  let bal_lit =
    (balance_label, balance_type, JSON.JSONLiteral.UintLit (Uint128L balance))
  in
  JSON.ContractState.state_to_json (bal_lit :: field_vals)

let output_message_json gas_remaining mlist =
  let open JSON.JSONLiteral in
  `List
    (List.map mlist ~f:(function
      | Msg m -> JSON.Message.message_to_json m
      | _ ->
          fatal_error_gas_scale Gas.scale_factor
            (mk_error0 ~kind:"Attempt to send non-message construct" ?inst:None)
            gas_remaining))

let output_event_json elist =
  let open JSON.JSONLiteral in
  `List
    (List.map elist ~f:(function
      | Msg m -> JSON.Event.event_to_json m
      | _ -> `Null))

let assert_no_address_type_in_type t gas_remaining =
  let open RunnerType in
  let rec recurser t =
    match t with
    | PrimType _ -> ()
    | Address _ ->
        fatal_error_gas_scale Gas.scale_factor
          (mk_error0 ~kind:"Address type not allowed in json file" ?inst:None)
          gas_remaining
    | MapType (kt, vt) ->
        let () = recurser kt in
        recurser vt
    | ADT (_, ts) -> List.iter ts ~f:recurser
    | FunType _ | TypeVar _ | PolyFun _ | Unit ->
        fatal_error_gas_scale Gas.scale_factor
          (mk_error0 ~kind:"Illegal type in json file" ~inst:(pp_typ t))
          gas_remaining
  in
  recurser t

(* No address types are allowed to occur in literals from init or message jsons *)
let assert_no_address_type_in_literal l gas_remaining =
  let open RunnerSyntax.SLiteral in
  let open RunnerType in
  let throw_address_type_error () =
    fatal_error_gas_scale Gas.scale_factor
      (mk_error0 ~kind:"Address type not allowed in json file" ?inst:None)
      gas_remaining
  in
  let rec recurser l =
    match l with
    | StringLit _ | IntLit _ | UintLit _ | BNum _ | ByStrX _ | ByStr _ -> ()
    | Map ((kt, vt), _) when is_address_type kt || is_address_type vt ->
        throw_address_type_error ()
    | Map (_, tbl) ->
        Caml.Hashtbl.iter
          (fun k v ->
            let () = recurser k in
            recurser v)
          tbl
    | ADTValue (_, ts, _) when List.exists ts ~f:is_address_type ->
        throw_address_type_error ()
    | ADTValue (_, _, ls) -> List.iter ls ~f:recurser
    | Msg _ | Clo _ | TAbs _ ->
        fatal_error_gas_scale Gas.scale_factor
          (mk_error0 ~kind:"Unknown literal in json file" ~inst:(pp_literal l))
          gas_remaining
  in
  recurser l

let validate_get_init_json init_file gas_remaining source_ver =
  (* Retrieve initial parameters *)
  let initargs_str, _ =
    try JSON.ContractState.get_json_data init_file
    with Invalid_json s ->
      fatal_error_gas_scale Gas.scale_factor
        (s @ mk_error0 ~kind:"Failed to parse json" ~inst:init_file)
        gas_remaining
  in
  (* Read init.json, and strip types. Types in init files must be ignored due to backward compatibility *)
  let initargs =
    List.map initargs_str ~f:(fun (x, t, l) -> (map_input_string_to_name x, t, l))
    |> List.map ~f:(fun (n, t, l) ->
           let () = assert_no_address_type_in_type t gas_remaining in
           let () = assert_no_address_type_in_literal l gas_remaining in
           (n, l))
  in
  (* Check for version mismatch. Subtract penalty for mismatch. *)
  let emsg = mk_error0 ~kind:"Scilla version mismatch" ?inst:None in
  let rgas =
    Uint64.sub gas_remaining (Uint64.of_int Gas.version_mismatch_penalty)
  in
  let init_json_scilla_version =
    List.find initargs ~f:(fun x ->
        [%equal: RunnerName.t] (fst x) ContractUtil.scilla_version_label)
  in
  let () =
    match init_json_scilla_version with
    | Some (_, UintLit (Uint32L v)) ->
        let mver, _, _ = scilla_version in
        let v' = Uint32.to_int v in
        if v' <> mver || mver <> source_ver then
          fatal_error_gas_scale Gas.scale_factor emsg rgas
    | _ -> fatal_error_gas_scale Gas.scale_factor emsg rgas
  in
  initargs

let validate_incoming_message msg_info gas_remaining =
  List.iter msg_info ~f:(fun (n, t, l) ->
      (* Address types are illegal in messages, except for _sender, _origin and _recipient *)
      if
        String.(
          n = ContractUtil.MessagePayload.sender_label
          || n = ContractUtil.MessagePayload.origin_label
          || n = ContractUtil.MessagePayload.recipient_label)
      then ()
      else
        let () = assert_no_address_type_in_type t gas_remaining in
        assert_no_address_type_in_literal l gas_remaining)

let gas_cost_rewriter_wrapper gas_remaining rewriter anode =
  match rewriter anode with
  | Error e -> fatal_error_gas_scale Gas.scale_factor e gas_remaining
  | Ok anode' -> anode'

let perform_dynamic_typechecks checks gas_remaining =
  let dummy_gas_resolver g =
    let msg =
      sprintf "Gas charge type %s must be handled by GasCharge\n"
        (RG.GasGasCharge.pp_gas_charge g)
    in
    Error (mk_error0 ~kind:msg ?inst:None)
  in
  List.fold_left checks ~init:gas_remaining ~f:(fun gas_remaining (t, caddr) ->
      (* The gas cost is static, but we go through the gas cost mechanism for maintainability *)
      let gas_charge = RG.address_typecheck_cost t in
      let gas_cost =
        match RG.GasGasCharge.eval dummy_gas_resolver gas_charge with
        | Ok (GasCharge.GInt cost) -> Uint64.of_int cost
        | Ok (GasCharge.GFloat _) ->
            fatal_error_gas_scale Gas.scale_factor
              (mk_error0 ~kind:"GasCharge evaluated to float" ?inst:None)
              gas_remaining
        | Error s -> fatal_error_gas_scale Gas.scale_factor s gas_remaining
      in
      let new_gas_remaining = Uint64.sub gas_remaining gas_cost in
      match
        EvalUtil.EvalTypecheck.assert_typecheck_remote_field_types ~caddr t
      with
      | Ok _ -> new_gas_remaining
      | Error s -> fatal_error_gas_scale Gas.scale_factor s new_gas_remaining)

let deploy_library args gas_remaining =
  match FEParser.parse_lmodule args.input with
  | Error e ->
      (* Error is printed by the parser. *)
      plog (sprintf "%s\n" "Failed to parse input library file.");
      fatal_error_gas_scale Gas.scale_factor e gas_remaining
  | Ok lmod_nogas -> (
      plog
        (sprintf "\n[Parsing]:\nLibrary module [%s] is successfully parsed.\n"
           args.input);

      (* Parse external libraries. *)
      let lib_dirs = FilePath.dirname args.input :: args.libdirs in
      StdlibTracker.add_stdlib_dirs lib_dirs;
      let this_address_opt, init_address_map =
        get_init_this_address_and_extlibs args.input_init
      in
      match this_address_opt with
      | None ->
          let msg =
            sprintf "No %s entry found in init file %s\n"
              (CUName.as_string ContractUtil.this_address_label)
              args.input_init
          in
          plog msg;
          fatal_error_gas_scale Gas.scale_factor
            (mk_error0 ~kind:msg ?inst:None)
            gas_remaining
      | Some this_address ->
          let elibs =
            List.map
              ~f:(gas_cost_rewriter_wrapper gas_remaining RG.libtree_cost)
            @@ import_libs lmod_nogas.elibs init_address_map
          in
          let dis_lmod_nogas =
            match
              Dis.disambiguate_lmodule lmod_nogas elibs init_address_map
                this_address
            with
            | Error e ->
                plog (sprintf "%s\n" "Failed to disambiguate library file.");
                fatal_error_gas_scale Gas.scale_factor e gas_remaining
            | Ok res ->
                plog
                  (sprintf
                     "\n\
                      [Disambiguation]:\n\
                      Library module [%s] is successfully disambiguated.\n"
                     args.input);
                res
          in
          let dis_lmod =
            gas_cost_rewriter_wrapper gas_remaining RG.lmod_cost dis_lmod_nogas
          in
          (* Contract library. *)
          let clibs = Some dis_lmod.libs in

          (* Checking initialized libraries! *)
          let _, gas_remaining' =
            check_libs clibs elibs args.input gas_remaining
          in
          let _ =
            validate_get_init_json args.input_init gas_remaining' dis_lmod.smver
          in
          let gas_remaining'' =
            Gas.finalize_remaining_gas args.gas_limit gas_remaining'
          in
          `Assoc
            [ ("gas_remaining", `String (Uint64.to_string gas_remaining'')) ])

let run_with_args args =
  let is_deployment = String.is_empty args.input_message in
  let is_ipc = not @@ String.is_empty args.ipc_address in
  let is_library =
    FilePath.check_extension args.input
      GlobalConfig.StdlibTracker.file_extn_library
  in
  let initial_gas_limit = Uint64.mul args.gas_limit Gas.scale_factor in
  let gas_remaining =
    (* Subtract gas based on (contract+init) size / message size. *)
    if is_deployment then
      let cost' =
        UnixLabels.((stat args.input).st_size + (stat args.input_init).st_size)
      in
      let cost = Uint64.of_int cost' in
      if Uint64.compare initial_gas_limit cost < 0 then
        fatal_error_gas_scale Gas.scale_factor
          (mk_error0 ~kind:"Insufficient gas to parse contract/init files"
             ?inst:None)
          Uint64.zero
      else Uint64.sub initial_gas_limit cost
    else
      let cost = Uint64.of_int (UnixLabels.stat args.input_message).st_size in
      if Uint64.compare initial_gas_limit cost < 0 then
        fatal_error_gas_scale Gas.scale_factor
          (mk_error0 ~kind:"Insufficient gas to parse message" ?inst:None)
          Uint64.zero
      else Uint64.sub initial_gas_limit cost
  in

  if is_library then
    if is_deployment then deploy_library args gas_remaining
    else
      (* Messages to libraries are ignored, but tolerated. *)
      `Assoc
        [
          ("gas_remaining", `String (Uint64.to_string gas_remaining));
          ( RunnerName.as_string ContractUtil.accepted_label,
            `String (Bool.to_string false) );
          ("messages", output_message_json gas_remaining []);
          ("events", output_event_json []);
        ]
  else
    match FEParser.parse_cmodule args.input with
    | Error e ->
        (* Error is printed by the parser. *)
        plog (sprintf "%s\n" "Failed to parse input file.");
        fatal_error_gas_scale Gas.scale_factor e gas_remaining
    | Ok cmod_nogas -> (
        plog
          (sprintf
             "\n[Parsing]:\nContract module [%s] is successfully parsed.\n"
             args.input);

        (* Parse external libraries. *)
        let lib_dirs = FilePath.dirname args.input :: args.libdirs in
        StdlibTracker.add_stdlib_dirs lib_dirs;
        let this_address_opt, init_address_map =
          get_init_this_address_and_extlibs args.input_init
        in
        match this_address_opt with
        | None ->
            let msg =
              sprintf "No %s entry found in init file %s\n"
                (CUName.as_string ContractUtil.this_address_label)
                args.input_init
            in
            plog msg;
            fatal_error_gas_scale Gas.scale_factor
              (mk_error0 ~kind:"Insufficient gas to parse contract/init files"
                 ?inst:None)
              gas_remaining
        | Some this_address ->
            let elibs =
              List.map
                ~f:(gas_cost_rewriter_wrapper gas_remaining RG.libtree_cost)
              @@ import_libs cmod_nogas.elibs init_address_map
            in
            let dis_cmod_nogas =
              match
                Dis.disambiguate_cmodule cmod_nogas elibs init_address_map
                  this_address
              with
              | Error e ->
                  plog (sprintf "%s\n" "Failed to disambiguate contract file.");
                  fatal_error_gas_scale Gas.scale_factor e gas_remaining
              | Ok res ->
                  plog
                    (sprintf
                       "\n\
                        [Disambiguation]:\n\
                        Contract module [%s] is successfully disambiguated.\n"
                       args.input);
                  res
            in
            let dis_cmod =
              gas_cost_rewriter_wrapper gas_remaining RG.cmod_cost
                dis_cmod_nogas
            in

            (* Contract library. *)
            let clibs = dis_cmod.libs in

            (* Checking initialized libraries! *)
            let libs_env, gas_remaining =
              check_libs clibs elibs args.input gas_remaining
            in
            let initargs =
              validate_get_init_json args.input_init gas_remaining
                dis_cmod.smver
            in

            let ( ( output_msg_json,
                    output_state_json,
                    output_events_json,
                    accepted_b ),
                  gas ) =
              if is_deployment then (
                let field_vals, gas_remaining =
                  check_contr_wrapper libs_env dis_cmod.contr.cconstraint
                    dis_cmod.contr.cfields initargs [] gas_remaining
                in
                (* Initializing the contract's state, just for checking things. *)
                let init_res =
                  init_module libs_env dis_cmod initargs Uint128.zero
                in
                (* Prints stats after the initialization and returns the initial state *)
                (* Will throw an exception if unsuccessful. *)
                let cstate', gas_remaining, dyn_checks =
                  check_extract_cstate args.input init_res gas_remaining
                in
                (* If the data store is not local, we must update the store with the initial field values.
                 * Refer to the details comments at [Initialization of StateService]. *)
                if is_ipc then
                  let open StateService in
                  let open MonadUtil in
                  let open Result.Let_syntax in
                  (* We push all fields except _balance. *)
                  let fields =
                    List.filter_map cstate'.fields ~f:(fun (s, t) ->
                        if [%equal: RunnerName.t] s balance_label then None
                        else Some { fname = s; ftyp = t; fval = None })
                  in
                  let sm = IPC args.ipc_address in
                  let () =
                    initialize ~sm ~fields ~ext_states:[]
                      ~bcinfo:(Caml.Hashtbl.create 0)
                  in
                  let field_vals' =
                    if args.reinit then
                      (* Retrieve state variables *)
                      try fst3 @@ input_state_json args.input_state
                      with Invalid_json s ->
                        fatal_error_gas_scale Gas.scale_factor
                          (s
                          @ mk_error0 ~kind:"Failed to parse json"
                              ~inst:args.input_state)
                          gas_remaining
                    else field_vals
                  in
                  (* Do the dynamic typecheck *)
                  let gas_remaining =
                    perform_dynamic_typechecks dyn_checks gas_remaining
                  in
                  match
                    let%bind () =
                      Result.ignore_m
                      @@ mapM field_vals' ~f:(fun (s, _t, v) ->
                             update ~fname:(SSIdentifier.mk_loc_id s) ~keys:[]
                               ~value:v)
                    in
                    finalize ()
                  with
                  | Error s ->
                      fatal_error_gas_scale Gas.scale_factor s gas_remaining
                  | Ok _ -> ()
                else if (* not is_ipc *)
                        not @@ List.is_empty dyn_checks then
                  plog
                    (sprintf
                       "\n\
                        [Deployment] Dynamic typecheck of contract parameters \
                        (%s) required, but disabled outside IPC mode.\n"
                       (List.map dyn_checks ~f:(fun (_, x) ->
                            RunnerSyntax.SLiteral.Bystrx.hex_encoding x)
                       |> String.concat ~sep:", "));

                (* In IPC mode, we don't need to output an initial state as it will be updated directly. *)
                let field_vals' = if is_ipc then [] else field_vals in

                plog (sprintf "\nContract initialized successfully\n");
                ( ( `Null,
                    output_state_json cstate'.balance field_vals',
                    `List [],
                    false ),
                  gas_remaining ))
              else
                (* Not initialization, execute transition specified in the message *)
                let mmsg =
                  try JSON.Message.get_json_data args.input_message
                  with Invalid_json s ->
                    fatal_error_gas_scale Gas.scale_factor
                      (s
                      @ mk_error0 ~kind:"Failed to parse json"
                          ~inst:args.input_message)
                      gas_remaining
                in
                let () = validate_incoming_message mmsg gas_remaining in
                let cstate, gas_remaining =
                  if is_ipc then
                    let cur_bal = args.balance in
                    let init_res =
                      init_module libs_env dis_cmod initargs cur_bal
                    in
                    let cstate, gas_remaining, _dyn_checks =
                      check_extract_cstate args.input init_res gas_remaining
                    in

                    (* Ignore dynamic typechecks of contract parameters - contract already deployed *)

                    (* Initialize the state server. *)
                    let fields =
                      List.filter_map cstate.fields ~f:(fun (s, t) ->
                          let open StateService in
                          if [%equal: RunnerName.t] s balance_label then None
                          else Some { fname = s; ftyp = t; fval = None })
                    in
                    let () =
                      StateService.initialize ~sm:(IPC args.ipc_address) ~fields
                        ~ext_states:[] ~bcinfo:(Caml.Hashtbl.create 0)
                    in
                    (cstate, gas_remaining)
                  else
                    (* Retrieve block chain state  *)
                    let bcinfo =
                      try
                        JSON.BlockChainState.get_json_data args.input_blockchain
                      with Invalid_json s ->
                        fatal_error_gas_scale Gas.scale_factor
                          (s
                          @ mk_error0
                              ~kind:
                                (sprintf "Failed to parse json %s:\n"
                                   args.input_blockchain)
                              ?inst:None)
                          gas_remaining
                    in
                    (* Retrieve state variables *)
                    let curargs, cur_bal, ext_states =
                      try input_state_json args.input_state
                      with Invalid_json s ->
                        fatal_error_gas_scale Gas.scale_factor
                          (s
                          @ mk_error0 ~kind:"Failed to parse json"
                              ~inst:args.input_state)
                          gas_remaining
                    in
                    let field_vals, _ignore_gas_remaining =
                      check_contr_wrapper libs_env dis_cmod.contr.cconstraint
                        dis_cmod.contr.cfields initargs curargs gas_remaining
                    in
                    (* Initializing the contract's state *)
                    let init_res =
                      init_module libs_env dis_cmod initargs cur_bal
                    in
                    (* Prints stats after the initialization and returns the initial state *)
                    (* Will throw an exception if unsuccessful. *)
                    let cstate, gas_remaining, _dyn_checks =
                      check_extract_cstate args.input init_res gas_remaining
                    in

                    (* Ignore dynamic typechecks of contract parameters - contract already deployed *)

                    (* Initialize the state server. *)
                    let fields =
                      List.map field_vals ~f:(fun (s, t, l) ->
                          let open StateService in
                          { fname = s; ftyp = t; fval = Some l })
                    in
                    let ext_states =
                      let open StateService in
                      List.map ext_states ~f:(fun (addr, fields) ->
                          let cparams', fields' =
                            List.partition_map fields ~f:(fun (n, m, t, l) ->
                                if is_mutable m then
                                  Second { fname = n; ftyp = t; fval = Some l }
                                else First { fname = n; ftyp = t; fval = Some l })
                          in
                          { caddr = addr; cparams = cparams'; cstate = fields' })
                    in
                    let () =
                      StateService.initialize ~sm:Local ~fields ~ext_states
                        ~bcinfo
                    in
                    (cstate, gas_remaining)
                in

                (* Contract code *)
                let ctr = dis_cmod.contr in

                plog
                  (sprintf "Executing message:\n%s\n"
                     (JSON.Message.message_to_jstring mmsg));
                let prepped_message, pending_dyn_checks, gas_remaining =
                  let pmsg = prepare_for_message ctr mmsg in
                  check_prepare_message pmsg gas_remaining
                in
                let gas_remaining =
                  perform_dynamic_typechecks pending_dyn_checks gas_remaining
                in
                let step_result = handle_message prepped_message cstate in
                let (cstate', mlist, elist, accepted_b), gas =
                  check_after_step step_result gas_remaining
                in

                (* If we're using a local state (JSON file) then need to fetch and dump it. *)
                let field_vals =
                  if is_ipc then []
                  else
                    match
                      (StateService.get_full_state (), StateService.finalize ())
                    with
                    | Ok fv, Ok () -> fv
                    | _ ->
                        fatal_error_gas_scale Gas.scale_factor
                          (mk_error0
                             ~kind:"Error finalizing state from StateService"
                             ?inst:None)
                          gas
                in

                let osj = output_state_json cstate'.balance field_vals in
                let omj = output_message_json gas mlist in
                let oej = output_event_json elist in
                let gas' = Gas.finalize_remaining_gas args.gas_limit gas in

                ((omj, osj, oej, accepted_b), gas')
            in
            `Assoc
              [
                ("scilla_major_version", `String (Int.to_string dis_cmod.smver));
                ("gas_remaining", `String (Uint64.to_string gas));
                ( RunnerName.as_string ContractUtil.accepted_label,
                  `String (Bool.to_string accepted_b) );
                ("messages", output_msg_json);
                ("states", output_state_json);
                ("events", output_events_json);
              ])

let run args_list ~exe_name =
  GlobalConfig.reset ();
  ErrorUtils.reset_warnings ();
  Datatypes.DataTypeDictionary.reinit ();
  let args = RunnerCLI.parse args_list ~exe_name in
  let result = run_with_args args in
  (result, args)
