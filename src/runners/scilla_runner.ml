(*
 * Copyright (c) 2018 - present Zilliqa, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)


open Printf
open Sexplib.Std
open Syntax
open Core
open Result.Let_syntax
open MonadUtil
open EvalUtil
open Eval
open TestRunnerInputs

exception EvalError of string

(****************************************************)
(*          Checking initialized libraries          *)
(****************************************************)

let check_libs libs name =
   let ls = init_libraries libs in
   (* Are libraries ok? *)
   match ls with
   | Ok res ->
       printf
         "\n[Initializing libraries]:\n%s\n\nLibraries for [%s] are on. All seems fine so far!\n\n"
         (* (Env.pp res) *)
         (String.concat ~sep:", " (List.map (List.rev res) ~f:fst))
         name
   | Error err ->
       (printf "\nFailed to initialize libraries:\n%s\n" err;
        raise @@ EvalError "Execution stopped")

(****************************************************)
(*     Checking initialized contract state          *)
(****************************************************)
let check_extract_cstate name res = match res with
  | Error err ->
      (printf "Failed to initialize fields:\n%s\n" err;
       raise @@ EvalError "Execution stopped")
  | Ok (_, cstate) ->
      (printf "[Initializing %s's fields]\nSuccess!\n%s\n"
         name (ContractState.pp cstate);
       cstate)

(*****************************************************)
(*   Running the simularion and printing results     *)
(*****************************************************)

let rec print_message mlist =
  match mlist with
  | [] -> ()
  | a :: b ->
    (match a with
      | Msg m ->
        (printf "%s\n" (JSON.Message.message_to_jstring ~pp:true m) ;
        print_message b)
      | _ -> ()
    )

let check_after_step name res bstate m =
  match res with
  | Error err ->
      (printf "Failed to execute transition:\n%s\n" err;
       raise @@ EvalError "Execution halted")
  | Ok (cstate, outs, _) ->
      (printf "Success! Here's what we got:\n";
       printf "%s" (ContractState.pp cstate);
       printf "Emitted messages:\n%s\n\n" (pp_literal_list outs);
       print_message outs;
       cstate, outs)

let make_step ctr name cstate i  =
  let (bstate, m) = get_context name i in
  printf "[Simulated execution, step %i]\nAbout to handle:\n" (i+1);
  printf "%s\nin a Blockchain State:\n%s.\n"
    (pp_literal m) (pp_literal_map bstate);
  let step_result = handle_message ctr cstate bstate m in
  let (cstate', _) =
    check_after_step name step_result bstate m in
  cstate'

(* Recursively execute multiple steps *)
let rec make_step_loop ctr name cstate num_steps i =
  if i >= 0 && i < num_steps
  then
    let cstate' = make_step ctr name cstate i in
    make_step_loop ctr name cstate' num_steps (i + 1)
  else
    printf "\nEvalutaion complete!"

(* Parse the input state json and extract out _balance separately *)
let input_state_json filename = 
  let open JSON.ContractState in
  let states = get_json_data filename in
  let match_balance ((vname : string), _) : bool = vname = "_balance" in
  let bal_lit = match List.find states ~f:match_balance with
    | Some (_, lit) -> lit
    | None -> IntLit("0") in
  let bal_int = match bal_lit with
    | IntLit (x) -> Int.of_string x
    | _ -> 0 in
  let no_bal_states = List.filter  states ~f:(fun c -> not @@ match_balance c) in
     no_bal_states, Big_int.big_int_of_int bal_int

(* Add balance to output json and print it out *)

let output_state_json (cstate : 'rep EvalUtil.ContractState.t) =
  let ballit = ("_balance", IntLit(Big_int.string_of_big_int cstate.balance)) in
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
  | _ -> assert false

(****************************************************)
(*              Main demo procedure                 *)
(****************************************************)
(* HOW TO RUN ME

After compilinig, run from the project root folder:

bin/scilla-runner crowdfunding n
  or 
bin/scilla-runner zil-game n

where "n" is a number 0-5 for the number of "steps" to execute the protocol.

*)

let () =
  let () = Cli.parse () in
  let parse_module =
    FrontEndParser.parse_file ScillaParser.cmodule !Cli.f_input in
  match parse_module with
  | None -> printf "%s\n" "Failed to parse input file."
  | Some cmod ->
      printf "\n[Parsing]:\nContract module [%s] is successfully parsed.\n"
        !Cli.f_input;
      (* Now initialize it *)
      let libs = cmod.libs in

      (* Checking initialized libraries! *)
      check_libs libs !Cli.f_input;
 
      (* Retrieve initial parameters *)
      let initargs = JSON.ContractState.get_json_data !Cli.f_input_init in
      (* Retrieve state variables *)
      let (curargs, cur_bal) = input_state_json !Cli.f_input_state in

      let bstate = JSON.BlockChainState.get_json_data !Cli.f_input_blockchain in
      let mmsg = JSON.Message.get_json_data !Cli.f_input_message in
      let m = Msg mmsg in

      (* Initializing the contract's state *)
      let init_res = init_module cmod initargs curargs cur_bal in
      (* Prints stats after the initialization and returns the initial state *)
      (* Will throw an exception if unsuccessful. *)
      let cstate = check_extract_cstate !Cli.f_input init_res in
      (* Contract code *)
      let ctr = cmod.contr in

      printf "Executing message:\n%s\n" (JSON.Message.message_to_jstring mmsg);
      printf "In a Blockchain State:\n%s\n" (pp_literal_map bstate);
      let step_result = handle_message ctr cstate bstate m in
      let (cstate', mlist) =
        check_after_step !Cli.f_input step_result bstate m in
      
      let osj = output_state_json cstate' in
      let omj = output_message_json mlist in
      let output_json = `Assoc [("message", omj) ; ("states", osj)] in
        Out_channel.with_file !Cli.f_output ~f:(fun channel -> 
          Yojson.pretty_to_string output_json |> Out_channel.output_string channel)