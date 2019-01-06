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

let f_input_init = ref ""
let f_input_state = ref ""
let f_input_message = ref ""
let f_input_blockchain = ref ""
let f_output = ref ""
let f_input = ref ""
let f_trace_file = ref ""
let f_trace_level = ref ""
let d_libs = ref []
let v_gas_limit = ref 0
let b_pp_lit = ref true
let b_json_errors = ref false
let b_pp_json = ref true
let b_validate_json = ref true

let usage = "-init init.json [-istate input_state.json]" ^
    " -iblockchain input_blockchain.json [-imessage input_message.json]" ^
    " -o output.json -i input.scilla [-tracefile filename] [-tracelevel none|stmt|exp ]" ^
    " -gaslimit i [-libdir dirpath] [-pplit true|false] [-disable-pp-json]"

let print_usage () = 
  Printf.fprintf stderr "Mandatory and optional flags:\n%s %s\n" Sys.argv.(0) usage

let process_trace () =
  match !f_trace_level with
  | "stmt" -> GlobalConfig.set_trace_level GlobalConfig.Trace_Statement;
            GlobalConfig.set_trace_file !f_trace_file
  | "exp" -> GlobalConfig.set_trace_level GlobalConfig.Trace_Expression;
            GlobalConfig.set_trace_file !f_trace_file
  | _ -> ()

let process_pplit () =
  if !b_pp_lit then GlobalConfig.set_pp_lit true else GlobalConfig.set_pp_lit false

let process_json_errors () =
  GlobalConfig.set_use_json_errors !b_json_errors

let process_json_validation () =
  GlobalConfig.set_validate_json !b_validate_json

let validate_main () =
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
  let msg6 =
    (* input_message.json and input_state.json can either both be there or both absent *)
    if (!f_input_message = "") <> (!f_input_state = "") 
      then msg5 ^ "Input message and input state can both be present or both absent\n"
      else msg5 in
  let msg7 = 
    (* gas limit is mandatory *)
    if !v_gas_limit <= 0 then msg6 ^ "Invalid gas limit specified" else msg6 in
  if msg7 <> ""
  then
    (print_usage ();
     Printf.fprintf stderr "%s\n" msg7;
     exit 1)
  else 
    ()

type ioFiles = {
    input_init : string;
    input_state : string;
    input_message : string;
    input_blockchain : string;
    output : string;
    input : string;
    libdirs : string list;
    gas_limit : int;
    pp_json : bool;
}

let parse () =
  let speclist = [
    ("-version", Arg.Unit (fun () -> 
        DebugMessage.pout
          (Core.Printf.sprintf "Scilla version: %s\n" PrettyPrinters.scilla_version_string);
          if true then exit 0; (* if "true" to avoid warning on exit 0 *)
          ()
      ), "Print Scilla version and exit");
    ("-init", Arg.String (fun x -> f_input_init := x), "Path to initialization json");
    ("-istate", Arg.String (fun x -> f_input_state := x), "Path to state input json");
    ("-imessage", Arg.String (fun x -> f_input_message := x), "Path to message input json");
    ("-iblockchain", Arg.String (fun x -> f_input_blockchain := x), "Path to blockchain input json");
    ("-o", Arg.String (fun x -> f_output := x), "Path to output json");
    ("-i", Arg.String (fun x -> f_input := x), "Path to scilla contract");
    ("-tracefile", Arg.String (fun x -> f_trace_file := x), "Path to trace file. (prints to stdout if no file specified)");
    ("-tracelevel", Arg.String (fun x -> f_trace_level := x), "Trace level: none|stmt|exp. (default none)");
    ("-libdir", Arg.String (fun x -> d_libs := x::!d_libs), "Path to directory containing libraries");
    ("-gaslimit", Arg.Int (fun i -> v_gas_limit := i), "Gas limit");
    ("-pplit", Arg.Bool (fun b -> b_pp_lit := b), "Pretty print literals");
    ("-jsonerrors", Arg.Unit (fun () -> b_json_errors := true), "Print errors in JSON format");
    ("-disable-pp-json", Arg.Unit (fun () -> b_pp_json := false), "Disable pretty printing of JSONs");
    ("-disable-validate-json", Arg.Unit (fun () -> b_validate_json := false), "Disable validation of input JSONs");
  ] in 
  let ignore_anon _ = () in
  let () = Arg.parse speclist ignore_anon ("Usage:\n" ^ usage) in
  let () = process_trace() in
  let () = process_pplit() in
  let () = process_json_errors() in
  let () = process_json_validation() in
  let () = validate_main () in
    {input_init = !f_input_init; input_state = !f_input_state; input_message = !f_input_message;
     input_blockchain = !f_input_blockchain; output = !f_output; input = !f_input;
     libdirs = !d_libs; gas_limit = !v_gas_limit; pp_json = !b_pp_json}
