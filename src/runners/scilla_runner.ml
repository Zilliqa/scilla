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
       (printf "Failed to initialize libraries:\n%s\n" err;
        raise @@ EvalError "Execution stopped")

(****************************************************)
(*     Checking initialized contract state          *)
(****************************************************)
let check_extract_cstate name res = match res with
  | Error err ->
      (printf "Failed to initialize fields:\n%s\n" err;
       raise @@ EvalError "Execution stopped")
  | Ok (_, cstate) ->
      (printf "[Initializing %s's fields]: Success!\n%s\n"
         name (ContractState.pp cstate);
       cstate)

(*****************************************************)
(*      Pretty-printing a result of interaction      *)
(*****************************************************)

let check_after_step name res bstate m =
  match res with
  | Error err ->
      (printf "Failed to execute transition:\n%s\n" err;
       raise @@ EvalError "Execution halted")
  | Ok (cstate, outs, _) ->
      (printf "Success! Here's what we got:\n";
       printf "%s" (ContractState.pp cstate);
       printf "Emitted messages:\n%s\n\n" (pp_literal_list outs);
       cstate, outs)

let make_step ctr name cstate i  =
  let (bstate, m) = get_context name i in
  printf "[Regular Execution Step %i] About to handle:\n" i;
  printf "%s\nin a Blockchain State: %s.\n"
    (pp_literal m) (pp_literal_map bstate);
  let step_result = process_message ctr cstate bstate m in
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

(****************************************************)
(*              Main demo procedure                 *)
(****************************************************)
(* HOW TO RUN ME

After compilinig, run from the project root folder:

bin/scilla-runner crowdfunding

or 

bin/scilla-runner zil-game

*)

let () =
  let arg_size = Array.length Sys.argv in
  (* Contract module name *)
  let name = if arg_size > 1 then Sys.argv.(1) else "crowdfunding" in
  (* Number of steps *)
  let num_iter = if arg_size > 2 then int_of_string Sys.argv.(2) else 1 in
  (if num_iter > 4
   then raise (EvalError "We didn't prepare data for some many steps! Pick a smaller number [1..4].") else ());

  (* Retrieve the contract *)
  let mod_path = sprintf "examples%scontracts%s%s"
      Filename.dir_sep Filename.dir_sep name in
  let filename = mod_path ^ Filename.dir_sep ^ "contract" in
  let parse_module =
    FrontEndParser.parse_file ScillaParser.cmodule filename in
  match parse_module with
  | None -> printf "%s\n" "Failed to parse input file."
  | Some cmod ->
      printf "\n[Parsing]:\nContract module [%s] is successfully parsed.\n"
        mod_path;
      (* Now initialize it *)
      let libs = cmod.libs in

      (* 1. Checking initialized libraries! *)
      check_libs libs mod_path;
 
      (* 2. Initializing the contract with arguments matching its parameters *)
      let (args, init_bal) = get_init_args name in
      let init_res = init_module cmod args init_bal in
      (* Print stats after the initialization *)
      (* Will throw exception if unsiccessful *)
      let cstate0 = check_extract_cstate name init_res in
      let ctr = cmod.contr in

      (* 3. Stepping a number of times from the inital state;  *)
      make_step_loop ctr name cstate0 num_iter 0;

      
