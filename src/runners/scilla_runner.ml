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
open ErrorUtils
open Eval
open DebugMessage
open ContractUtil
open PrettyPrinters
open Stdint
open RunnerUtil
open GlobalConfig

(****************************************************)
(*          Checking initialized libraries          *)
(****************************************************)

let check_libs clibs elibs name gas_limit =
   let ls = init_libraries clibs elibs in
   (* Are libraries ok? *)
   match ls Eval.init_gas_kont gas_limit with
   | Ok (res, gas_remaining) ->
       plog (sprintf
         "\n[Initializing libraries]:\n%s\n\nLibraries for [%s] are on. All seems fine so far!\n\n"
         (* (Env.pp res) *)
         (String.concat ~sep:", " (List.map (List.rev res) ~f:fst))
         name);
      gas_remaining
   | Error (err, gas_remaining) ->
      fatal_error_gas err gas_remaining

(****************************************************)
(*     Checking initialized contract state          *)
(****************************************************)
let check_extract_cstate name res gas_limit = 
  match res Eval.init_gas_kont gas_limit with
  | Error (err, remaining_gas) ->
      fatal_error_gas err remaining_gas
  | Ok ((_, cstate, field_vals), remaining_gas) ->
      plog (sprintf "[Initializing %s's fields]\nSuccess!\n"
         name );
      cstate, remaining_gas, field_vals

(*****************************************************)
(*   Running the simularion and printing results     *)
(*****************************************************)

let check_after_step res gas_limit  =
  match res Eval.init_gas_kont gas_limit with
  | Error (err, remaining_gas) -> fatal_error_gas err remaining_gas
  | Ok ((cstate, outs, events, accepted_b), remaining_gas) ->
      plog (sprintf "Success! Here's what we got:\n" ^
            (* sprintf "%s" (ContractState.pp cstate) ^ *)
            sprintf "Emitted messages:\n%s\n\n" (pp_literal_list outs) ^
            sprintf"Gas remaining:%s\n" (Uint64.to_string remaining_gas) ^
            sprintf "Emitted events:\n%s\n\n" (pp_literal_list events));
       (cstate, outs, events, accepted_b), remaining_gas

(* Parse the input state json and extract out _balance separately *)
let input_state_json filename = 
  let open JSON.ContractState in
  let states = get_json_data filename in
  let match_balance ((vname : string), _) : bool = vname = balance_label in
  let bal_lit = match List.find states ~f:match_balance with
    | Some (_, lit) -> lit
    | None -> raise (mk_invalid_json (balance_label ^ " field missing"))
  in
  let bal_int = match bal_lit with
    | UintLit (Uint128L x) -> x
    | _ -> raise (mk_invalid_json (balance_label ^ " invalid"))
  in
  let no_bal_states = List.filter  states ~f:(fun c -> not @@ match_balance c) in
     no_bal_states, bal_int

(* Add balance to output json and print it out *)
let output_state_json balance field_vals =
  let ballit = (balance_label, UintLit (Uint128L balance)) in
  let concatlist = List.cons ballit field_vals in
    JSON.ContractState.state_to_json concatlist;;

let output_message_json gas_remaining mlist =
  match mlist with
  | [one_message] ->
    (match one_message with
     | Msg m ->
        JSON.Message.message_to_json m
     | _ -> `Null
    )
  | [] -> `Null
  | _ ->
    fatal_error_gas (mk_error0 "Sending more than one message not currently permitted") gas_remaining


let rec output_event_json elist =
  match elist with
  | e :: rest ->
    let j = output_event_json rest in
    (match e with
    | Msg m' ->
      let ej = JSON.Event.event_to_json m' in
      ej :: j
    | _ -> `Null :: j)
  | [] -> []

let deploy_library (cli : Cli.ioFiles) gas_remaining =
  let parse_lmodule =
    FrontEndParser.parse_file ScillaParser.lmodule cli.input in
  match parse_lmodule with
  | Error e ->
    (* Error is printed by the parser. *)
    plog (sprintf "%s\n" "Failed to parse input library file.");
    fatal_error_gas e gas_remaining
  | Ok lmod ->
      plog (sprintf "\n[Parsing]:\nLibrary module [%s] is successfully parsed.\n" cli.input);
      (* Parse external libraries. *)
      let lib_dirs = (FilePath.dirname cli.input :: cli.libdirs) in
      StdlibTracker.add_stdlib_dirs lib_dirs;
      let elibs = import_libs lmod.elibs (Some cli.input_init) in
      (* Contract library. *)
      let clibs = Some (lmod.libs) in

      (* Checking initialized libraries! *)
      let gas_remaining' = check_libs clibs elibs cli.input gas_remaining in

      (* Retrieve initial parameters *)
      let initargs =
        try
          JSON.ContractState.get_json_data cli.input_init
        with
        | Invalid_json s ->
            fatal_error_gas (s @ (mk_error0 (sprintf "Failed to parse json %s:\n" cli.input_init))) gas_remaining'
      in
      (* init.json for libraries can only have _extlibs field. *)
      (match initargs with
      | [(label, _)] when label = extlibs_label -> ()
      | _ -> perr @@ scilla_error_gas_string gas_remaining'
            (mk_error0 (sprintf "Invalid initialization file %s for library\n" cli.input_init))
      );
      let output_json = `Assoc [
        "gas_remaining", `String (Uint64.to_string gas_remaining');
        (* ("warnings", (scilla_warning_to_json (get_warnings ()))) *)
      ] in
        Out_channel.with_file cli.output ~f:(fun channel ->
          if cli.pp_json then
            Yojson.pretty_to_string output_json |> Out_channel.output_string channel
          else
            Yojson.to_string output_json |> Out_channel.output_string channel
          )

let () =
  let cli = Cli.parse () in
  let is_deployment = (cli.input_message = "") in
  let is_ipc = cli.ipc_port <> 0 in
  let is_library =
    (FilePath.get_extension cli.input = GlobalConfig.StdlibTracker.file_extn_library) in
  let gas_remaining =
    let open Unix in
    (* Subtract gas based on (contract+init) size / message size. *)
    if is_deployment then
      let cost' = Int64.add (stat cli.input).st_size (stat cli.input_init).st_size in
      let cost = Uint64.of_int64 cost' in
      if (Uint64.compare cli.gas_limit cost) < 0 then
        fatal_error_gas (mk_error0 (sprintf "Ran out of gas when parsing contract/init files.\n"))  Uint64.zero 
      else
        Uint64.sub cli.gas_limit cost
    else
      let cost = Uint64.of_int64 (stat cli.input_message).st_size in
      (* libraries can only be deployed, not "run". *)
      if is_deployment then
        fatal_error_gas (mk_error0 (sprintf "Cannot run a library contract. They can only be deployed\n")) Uint64.zero
      else if (Uint64.compare cli.gas_limit cost) < 0 then
        fatal_error_gas (mk_error0 (sprintf "Ran out of gas when parsing message.\n")) Uint64.zero 
      else
        Uint64.sub cli.gas_limit cost
  in

  if is_library then deploy_library cli gas_remaining  else

  let parse_module =
    FrontEndParser.parse_file ScillaParser.cmodule cli.input in
  match parse_module with
  | Error e -> 
    (* Error is printed by the parser. *)
    plog (sprintf "%s\n" "Failed to parse input file.");
    fatal_error_gas e gas_remaining
  | Ok cmod ->
      plog (sprintf "\n[Parsing]:\nContract module [%s] is successfully parsed.\n" cli.input);

      (* Parse external libraries. *)
      let lib_dirs = (FilePath.dirname cli.input :: cli.libdirs) in
      StdlibTracker.add_stdlib_dirs lib_dirs;
      let elibs = import_libs cmod.elibs (Some cli.input_init) in
      (* Contract library. *)
      let clibs = cmod.libs in
  
      (* Checking initialized libraries! *)
      let gas_remaining = check_libs clibs elibs cli.input gas_remaining in
 
      (* Retrieve initial parameters *)
      let initargs = 
        try 
          JSON.ContractState.get_json_data cli.input_init
        with
        | Invalid_json s -> 
            fatal_error_gas
              (s @ (mk_error0 (sprintf "Failed to parse json %s:\n" cli.input_init)))
            gas_remaining
      in

      (* Check for version mismatch. Subtract penalty for mist-match. *)
      let emsg, rgas = (mk_error0 ("Scilla version mismatch\n")),
        (Uint64.sub gas_remaining (Uint64.of_int Gas.version_mismatch_penalty))
      in
      let init_json_scilla_version = List.fold_left initargs ~init:None ~f:(fun found (name, lit) ->
        if is_some found then found else
        if name = ContractUtil.scilla_version_label
        then match lit with | UintLit(Uint32L v) -> Some v | _ -> None
        else None
      ) in
      let _ =
        match init_json_scilla_version with
        | Some ijv ->
          let (mver, _, _) = scilla_version in
          let ijv' = Uint32.to_int ijv in
          if ijv' <> mver || mver <> cmod.smver
          then fatal_error_gas emsg rgas
        | None -> fatal_error_gas emsg rgas
      in

      (* Retrieve block chain state  *)
      let bstate = 
      try
        JSON.BlockChainState.get_json_data cli.input_blockchain 
      with
        | Invalid_json s -> 
           fatal_error_gas 
              (s @ (mk_error0 (sprintf "Failed to parse json %s:\n" cli.input_blockchain)))
            gas_remaining
      in
      let (output_msg_json, output_state_json, output_events_json, accepted_b), gas = 
      if is_deployment
      then
        (* Initializing the contract's state, just for checking things. *)
        let init_res = init_module cmod initargs [] Uint128.zero bstate elibs in
        (* Prints stats after the initialization and returns the initial state *)
        (* Will throw an exception if unsuccessful. *)
        let (cstate', remaining_gas', field_vals) = check_extract_cstate cli.input init_res gas_remaining in

        (* If the data store is not local, we must update the store with the initial field values.
         * Refer to the details comments at [Initialization of StateService]. *)
        if is_ipc then (
          let open StateService in
          let open MonadUtil in
          let open Result.Let_syntax in
          let fields = List.map cstate'.fields ~f:(fun (s, t) -> { fname = s; ftyp = t; fval = None }) in
          let sm = IPC (cli.ipc_port) in
          let () = initialize ~sm ~fields in
          match
             (* TODO: Move gas accounting for initialization here? It's currently inside init_module. *)
             let%bind _ = mapM field_vals ~f:(fun (s, v) ->
              update ~fname:(asId s) ~keys:[] ~value:v
             ) in
             finalize ()
          with
          | Error s -> fatal_error_gas s remaining_gas'
          | Ok _ -> ()
        );
         
        (plog (sprintf "\nContract initialized successfully\n");
          (`Null, output_state_json cstate'.balance field_vals, `List [], false), remaining_gas')
      else
        (* Not initialization, execute transition specified in the message *)
        (let mmsg = 
        try
          JSON.Message.get_json_data cli.input_message 
        with
        | Invalid_json s ->
            fatal_error_gas
              (s @ (mk_error0 (sprintf "Failed to parse json %s:\n" cli.input_message)))
            gas_remaining
        in
        let m = Msg mmsg in

        let cstate, gas_remaining' = 
        if is_ipc then
          (* TODO: Get balance as command line argument. *)
          let cur_bal = Stdint.Uint128.zero in
          let init_res = init_module cmod initargs [] cur_bal bstate elibs in
          let cstate, gas_remaining', _ = check_extract_cstate cli.input init_res gas_remaining in
          (* Initialize the state server. *)
          let fields = List.map cstate.fields ~f:(fun (s, t) ->
            let open StateService in
            { fname = s; ftyp = t; fval = None }
          ) in
          let () = StateService.initialize ~sm:(IPC cli.ipc_port) ~fields in
          (cstate, gas_remaining')
        else

          (* Retrieve state variables *)
          let (curargs, cur_bal) = 
          try
            input_state_json cli.input_state
          with
          | Invalid_json s ->
              fatal_error_gas
                (s @ (mk_error0 (sprintf "Failed to parse json %s:\n" cli.input_state)))
              gas_remaining
          in

          (* Initializing the contract's state *)
          let init_res = init_module cmod initargs curargs cur_bal bstate elibs in
          (* Prints stats after the initialization and returns the initial state *)
          (* Will throw an exception if unsuccessful. *)
          let cstate, gas_remaining', field_vals = check_extract_cstate cli.input init_res gas_remaining in

          (* Initialize the state server. *)
          let fields = List.map field_vals ~f:(fun (s, l) ->
            let open StateService in
            let t = List.Assoc.find_exn cstate.fields ~equal:(=) s in
            { fname = s; ftyp = t; fval = Some l }
          ) in
          let () = StateService.initialize ~sm:Local ~fields in
          (cstate, gas_remaining')
        in

        (* Contract code *)
        let ctr = cmod.contr in

        plog (sprintf "Executing message:\n%s\n" (JSON.Message.message_to_jstring mmsg));
        plog (sprintf "In a Blockchain State:\n%s\n" (pp_literal_map bstate));
        let step_result = handle_message ctr cstate bstate m in
        let (cstate', mlist, elist, accepted_b), gas =
          check_after_step step_result gas_remaining' in

        (* If we're using a local state (JSON file) then need to fetch and dump it. *)
        let field_vals =
          if is_ipc then [] else
            match StateService.get_full_state () with
            | Error s -> fatal_error_gas s gas
            | Ok fv -> fv
        in

        let osj = output_state_json cstate'.balance field_vals in
        let omj = output_message_json gas mlist in
        let oej = `List (output_event_json elist) in
          (omj, osj, oej, accepted_b), gas)
      in
      let output_json = `Assoc [
        ("scilla_major_version", `String (Int.to_string cmod.smver));
        "gas_remaining", `String (Uint64.to_string gas);
        ContractUtil.accepted_label, `String (Bool.to_string accepted_b);
        ("message", output_msg_json); 
        ("states", output_state_json);
        ("events", output_events_json);
        (* ("warnings", (scilla_warning_to_json (get_warnings ()))) *)
      ] in
        Out_channel.with_file cli.output ~f:(fun channel -> 
          if cli.pp_json then
            Yojson.Basic.pretty_to_string output_json |> Out_channel.output_string channel
          else
            Yojson.Basic.to_string output_json |> Out_channel.output_string channel
          )
