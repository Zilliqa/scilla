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

let usage = "scilla-runner-init init.json [-istate input_state.json]" ^
    " -iblockchain input_blockchain.json [-imessage input_message.json]" ^
    " -o output.json -i input.scilla" 

let print_usage () = 
  Printf.fprintf stderr "Mandatory and optional flags:\n%s %s\n" Sys.argv.(0) usage

let validate () =
  let msg = 
    (* init.json is mandatory *)
    (if not (Sys.file_exists !f_input_init) 
     then "Invalid initialization file\n" else "") in
  let msg1 = 
    (* input_state.json is not mandatory, but if provided, should be valid *)
    (if ((!f_input_state <> "") && not (Sys.file_exists !f_input_state)) 
     then msg ^ "Invalid input contract state\n" else msg) in
  let msg2 = 
    (* input_message.json is not mandatory, but if provided, should be valid *)
    (if ((!f_input_message <> "") && not (Sys.file_exists !f_input_message))
     then msg1 ^ "Invalid input message\n" else msg1) in
  let msg3 = 
    (* input_blockchain.json is mandatory *)
    (if not (Sys.file_exists !f_input_blockchain)
     then msg2 ^ "Invalid input blockchain state\n" else msg2) in
  let msg4 = 
    (* input file is mandatory *)
    (if not ((Sys.file_exists !f_input))
     then msg3 ^ "Invalid input contract file\n" else msg3) in
  let msg5 = 
    (* output file is mandatory *)
    (if !f_output = "" then msg4 ^ "Output file not specified\n" else msg4) in
  if msg5 <> ""
  then
    (print_usage ();
     Printf.fprintf stderr "%s\n" msg5;
     exit 1)
  else 
    ()

type ioFiles = {
    input_init : string;
    input_state : string;
    input_message : string;
    input_blockchain : string;
    output : string;
    input : string
}

let parse () =
  let speclist = [
    ("-init       ", Arg.String (fun x -> f_input_init := x), "Path to initialization json");
    ("-istate     ", Arg.String (fun x -> f_input_state := x), "Path to state input json");
    ("-imessage   ", Arg.String (fun x -> f_input_message := x), "Path to message input json");
    ("-iblockchain", Arg.String (fun x -> f_input_blockchain := x), "Path to blockchain input json");
    ("-o          ", Arg.String (fun x -> f_output := x), "Path to output json");
    ("-i          ", Arg.String (fun x -> f_input := x), "Path to scilla contract");
  ] in 
  let ignore_anon s = () in
  let () = Arg.parse speclist ignore_anon ("Usage:\n" ^ usage) in
  let () = validate () in
    {input_init = !f_input_init; input_state = !f_input_state; input_message = !f_input_message;
     input_blockchain = !f_input_blockchain; output = !f_output; input = !f_input}
