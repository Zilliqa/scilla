(*
 * Copyright (c) 2018 - present Zilliqa, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

let f_input_init = ref ""
let f_input_state = ref ""
let f_input_message = ref ""
let f_input_blockchain = ref ""
let f_output = ref ""
let f_input = ref ""

let usage = "-init init.json [-istate input_state.json]" ^
    " -iblockchain input_blockchain.json [-imessage input_message.json]" ^
    " -o output.json -i input.scilla" 

exception CliError of string

let print_usage () = 
  Printf.fprintf stderr "Usage: %s %s\n" Sys.argv.(0) usage

let validate () =
  (* init.json is mandatory *)
  (if (not (Sys.file_exists !f_input_init) ||
    (* input_state.json is not mandatory, but if provided, should be valid *)
    ((!f_input_state <> "") && not (Sys.file_exists !f_input_state)) ||
    (* input_message.json is not mandatory, but if provided, should be valid *)
    ((!f_input_message <> "") && not (Sys.file_exists !f_input_message)) ||
    (* input_blockchain.json is mandatory *)
    not (Sys.file_exists !f_input_blockchain))
  then 
     (print_usage ();
     raise (CliError "Invalid command line input"))
  else 
    ()
  )

 let parse =
  let speclist = [
    ("-init", Arg.String (fun x -> f_input_init := x), "Path to initialization json");
    ("-istate", Arg.String (fun x -> f_input_state := x), "Path to state input json");
    ("-imessage", Arg.String (fun x -> f_input_message := x), "Path to message input json");
    ("-iblockchain", Arg.String (fun x -> f_input_blockchain := x), "Path to blockchain input json");
    ("-o", Arg.String (fun x -> f_output := x), "Path to output json");
    ("-i", Arg.String (fun x -> f_input := x), "Path to scilla contract");
    ("-help", Arg.Unit print_usage, "Display command line usage help");
  ] in 
  let ignore_anon s = () in
  let () = Arg.parse speclist ignore_anon ("Usage: "^usage) in
    validate ()
