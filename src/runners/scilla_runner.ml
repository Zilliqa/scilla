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
open EvalUtil
open Eval
open DebugMessage
open ContractUtil
open Stdint
open RunnerUtil

(****************************************************)
(*          Checking initialized libraries          *)
(****************************************************)

let check_libs clibs elibs name =
   let ls = init_libraries clibs elibs in
   (* Are libraries ok? *)
   match ls with
   | Ok res ->
       plog (sprintf
         "\n[Initializing libraries]:\n%s\n\nLibraries for [%s] are on. All seems fine so far!\n\n"
         (* (Env.pp res) *)
         (String.concat ~sep:", " (List.map (List.rev res) ~f:fst))
         name)
   | Error err ->
      perr (sprintf "\nFailed to initialize libraries:\n%s\n" err);
      perr "Execution stopped"

(****************************************************)
(*     Checking initialized contract state          *)
(****************************************************)
let check_extract_cstate name res = match res with
  | Error err ->
      perr (sprintf "Failed to initialize fields:\n%s\n" err);
      perr "Execution stopped";
      exit 1
  | Ok (_, cstate) ->
      plog (sprintf "[Initializing %s's fields]\nSuccess!\n%s\n"
         name (ContractState.pp cstate));
      cstate

(*****************************************************)
(*   Running the simularion and printing results     *)
(*****************************************************)

let check_after_step name res =
  match res with
  | Error err ->
      perr (sprintf "Failed to execute transition in %s:\n%s\n" name err);
      perr "Execution halted";
      exit 1
  | Ok (cstate, outs, _) ->
      plog (sprintf "Success! Here's what we got:\n" ^
            sprintf "%s" (ContractState.pp cstate) ^
            sprintf "Emitted messages:\n%s\n\n" (pp_literal_list outs));
       (cstate, outs)

(* Parse the input state json and extract out _balance separately *)
let input_state_json filename = 
  let open JSON.ContractState in
  let states = get_json_data filename in
  let match_balance ((vname : string), _) : bool = vname = balance_label in
  let bal_lit = match List.find states ~f:match_balance with
    | Some (_, lit) -> lit
    | None -> raise (JSON.Invalid_json (balance_label ^ " field missing"))
  in
  let bal_int = match bal_lit with
    | UintLit (_, x) -> Int.of_string x
    | _ -> raise (JSON.Invalid_json (balance_label ^ " invalid"))
  in
  let no_bal_states = List.filter  states ~f:(fun c -> not @@ match_balance c) in
     no_bal_states, Uint128.of_int bal_int

(* Add balance to output json and print it out *)

let output_state_json (cstate : 'rep EvalUtil.ContractState.t) =
  let ballit = (balance_label, UintLit(128, Uint128.to_string cstate.balance)) in
  let concatlist = List.cons ballit cstate.fields in
    JSON.ContractState.state_to_json concatlist;;

let output_message_json mlist =
  match mlist with
  (* TODO: What should we do with  more than one output message? *)
  | first_message :: [] -> 
    (match first_message with 
     | Msg m ->
        JSON.Message.message_to_json m
     | _ -> `Null
    )
  (* There will be at least one output message *)
  | _ -> `Null

let () =
  let cli = Cli.parse () in
  let parse_module =
    FrontEndParser.parse_file ScillaParser.cmodule cli.input in
  match parse_module with
  | None -> plog (sprintf "%s\n" "Failed to parse input file.")
  | Some cmod ->
      plog (sprintf "\n[Parsing]:\nContract module [%s] is successfully parsed.\n" cli.input);

      (* Parse external libraries. *)
      let lib_dirs = (Filename.dirname cli.input::cli.libdirs) in
      let elibs = import_libs cmod.elibs lib_dirs in
      (* Contract library. *)
      let clibs = cmod.libs in
  
      (* Checking initialized libraries! *)
      check_libs clibs elibs cli.input;
 
      (* Retrieve initial parameters *)
      let initargs = 
        try 
          JSON.ContractState.get_json_data cli.input_init
        with
        | JSON.Invalid_json s -> 
            perr (sprintf "Failed to parse json %s: %s\n" cli.input_init s);
            exit 1
      in
      (* Retrieve block chain state  *)
      let bstate = 
      try
        JSON.BlockChainState.get_json_data cli.input_blockchain 
      with
        | JSON.Invalid_json s -> 
            perr (sprintf "Failed to parse json %s: %s\n" cli.input_blockchain s);
            exit 1
      in
      let (output_msg_json, output_state_json) = 
      if cli.input_message = ""
      then
        (* Initializing the contract's state, just for checking things. *)
        let init_res = init_module cmod initargs [] Uint128.zero bstate elibs in
        (* Prints stats after the initialization and returns the initial state *)
        (* Will throw an exception if unsuccessful. *)
        let _ = check_extract_cstate cli.input init_res in
        (plog (sprintf "\nContract initialized successfully\n");
          (`Null, `List []))
      else
        (* Not initialization, execute transition specified in the message *)
        (let mmsg = 
        try
          JSON.Message.get_json_data cli.input_message 
        with
        | JSON.Invalid_json s -> 
            perr (sprintf "Failed to parse json %s: %s\n" cli.input_message s);
            exit 1
        in
        let m = Msg mmsg in

        (* Retrieve state variables *)
        let (curargs, cur_bal) = 
        try
          input_state_json cli.input_state
        with
        | JSON.Invalid_json s -> 
            perr (sprintf "Failed to parse json %s: %s\n" cli.input_state s);
            exit 1
        in

        (* Initializing the contract's state *)
        let init_res = init_module cmod initargs curargs cur_bal bstate elibs in
        (* Prints stats after the initialization and returns the initial state *)
        (* Will throw an exception if unsuccessful. *)
        let cstate = check_extract_cstate cli.input init_res in
        (* Contract code *)
        let ctr = cmod.contr in

        plog (sprintf "Executing message:\n%s\n" (JSON.Message.message_to_jstring mmsg));
        plog (sprintf "In a Blockchain State:\n%s\n" (pp_literal_map bstate));
        let step_result = handle_message ctr cstate bstate m in
        let (cstate', mlist) =
          check_after_step cli.input step_result in
      
        let osj = output_state_json cstate' in
        let omj = output_message_json mlist in
          (omj, osj))
      in
      let output_json = `Assoc [("message", output_msg_json) ; ("states", output_state_json)] in
        Out_channel.with_file cli.output ~f:(fun channel -> 
          Yojson.pretty_to_string output_json |> Out_channel.output_string channel)
