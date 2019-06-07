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
let v_gas_limit = ref (Stdint.Uint64.zero)
let b_pp_lit = ref true
let b_json_errors = ref false
let b_pp_json = ref true
let b_validate_json = ref true
let i_ipc_port = ref 0

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

let validate_main usage =
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
    (* input_message.json and input_state.json / i_ipc_port can either both be there or both absent *)
    if (!f_input_message <> "") &&
       ((!f_input_state <> "") && (!i_ipc_port <> 0) || (!f_input_state = "" && !i_ipc_port = 0))
      then msg5 ^ "Input message provided, but either none or both of input state / IPC port provided"
      else msg5 in
  if msg6 <> ""
  then
    PrettyPrinters.fatal_error_noformat (usage ^ (Printf.sprintf "%s\n" msg6))
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
    gas_limit : Stdint.uint64;
    pp_json : bool;
    ipc_port : int;
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
    ("-ipcport", Arg.Int (fun x -> i_ipc_port := x), "Port number for IPC communication with blockchain for state access");
    ("-iblockchain", Arg.String (fun x -> f_input_blockchain := x), "Path to blockchain input json");
    ("-o", Arg.String (fun x -> f_output := x), "Path to output json");
    ("-i", Arg.String (fun x -> f_input := x), "Path to scilla contract");
    ("-tracefile", Arg.String (fun x -> f_trace_file := x), "Path to trace file. (prints to stdout if no file specified)");
    ("-tracelevel", Arg.String (fun x -> f_trace_level := x), "Trace level: none|stmt|exp. (default none)");
    ("-libdir", Arg.String (fun x ->
        let xl = if x = "" then [] else Str.split (Str.regexp "[;:]") x in
        d_libs := !d_libs @ xl
      ), "Path(s) to directory containing libraries separated by ':' (';' on windows)");
    ("-gaslimit", Arg.String
      (fun i ->
        let g = 
          try
            Stdint.Uint64.of_string i
          with
          | _ -> PrettyPrinters.fatal_error (ErrorUtils.mk_error0 (Printf.sprintf "Invalid gaslimit %s\n" i))
        in
        v_gas_limit := g)
      , "Gas limit");
    ("-pplit", Arg.Bool (fun b -> b_pp_lit := b), "Pretty print literals");
    ("-jsonerrors", Arg.Unit (fun () -> b_json_errors := true), "Print errors in JSON format");
    ("-disable-pp-json", Arg.Unit (fun () -> b_pp_json := false), "Disable pretty printing of JSONs");
    ("-disable-validate-json", Arg.Unit (fun () -> b_validate_json := false), "Disable validation of input JSONs");
  ] in 

  let mandatory_usage = "Usage:\n" ^ Sys.argv.(0) ^ " -init init.json [-istate input_state.json]" ^
    " -iblockchain input_blockchain.json [-imessage input_message.json]" ^
    " -o output.json -i input.scilla -libdir /path/to/stdlib" ^
    " -gaslimit limit" ^ "\n" in
  let optional_usage = String.concat "\n  "
    (List.map (fun (flag, _, desc) -> flag ^ " " ^ desc) speclist) in
  let usage = mandatory_usage ^ "\n  " ^ optional_usage in

  let ignore_anon _ = () in
  let () = Arg.parse speclist ignore_anon mandatory_usage in
  let () = process_trace() in
  let () = process_pplit() in
  let () = process_json_errors() in
  let () = process_json_validation() in
  let () = validate_main usage in
    {input_init = !f_input_init; input_state = !f_input_state; input_message = !f_input_message;
     input_blockchain = !f_input_blockchain; output = !f_output; input = !f_input;
     libdirs = !d_libs; gas_limit = !v_gas_limit; pp_json = !b_pp_json; ipc_port = !i_ipc_port}
