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
open Scilla_base

type args = {
  input_init : string;
  input_state : string;
  input_message : string;
  input_blockchain : string;
  output : string;
  input : string;
  libdirs : string list;
  gas_limit : Stdint.uint64;
  balance : Stdint.uint128;
  pp_json : bool;
  ipc_address : string;
}

let f_input_init = ref ""

let f_input_state = ref ""

let f_input_message = ref ""

let f_input_blockchain = ref ""

let f_output = ref ""

let f_input = ref ""

let f_trace_file = ref ""

let f_trace_level = ref ""

let d_libs = ref []

let v_gas_limit = ref Stdint.Uint64.zero

let v_balance = ref None

let b_pp_lit = ref true

let b_json_errors = ref false

let b_pp_json = ref true

let b_validate_json = ref true

let i_ipc_address = ref ""

let reset () =
  f_input_init := "";
  f_input_state := "";
  f_input_message := "";
  f_input_blockchain := "";
  f_output := "";
  f_input := "";
  f_trace_file := "";
  f_trace_level := "";
  d_libs := [];
  v_gas_limit := Stdint.Uint64.zero;
  v_balance := None;
  b_pp_lit := true;
  b_json_errors := false;
  b_pp_json := true;
  b_validate_json := true;
  i_ipc_address := ""

let process_trace () =
  match !f_trace_level with
  | "stmt" ->
      GlobalConfig.set_trace_level GlobalConfig.Trace_Statement;
      GlobalConfig.set_trace_file !f_trace_file
  | "exp" ->
      GlobalConfig.set_trace_level GlobalConfig.Trace_Expression;
      GlobalConfig.set_trace_file !f_trace_file
  | _ -> ()

let process_pplit () = GlobalConfig.set_pp_lit !b_pp_lit

let process_json_errors () = GlobalConfig.set_use_json_errors !b_json_errors

let process_json_validation () = GlobalConfig.set_validate_json true

let validate_main usage =
  (* not mandatory file name input, but if provided, should be valid *)
  let invalid_optional_fname fname =
    not (String.is_empty fname || Sys.file_exists fname)
  in
  let msg = "" in
  let msg =
    (* init.json is mandatory *)
    if not @@ Sys.file_exists !f_input_init then "Invalid initialization file\n"
    else msg
  in
  let msg =
    (* input_state.json is not mandatory, but if provided, should be valid *)
    if invalid_optional_fname !f_input_state then
      msg ^ "Invalid input contract state: " ^ !f_input_state ^ "\n"
    else msg
  in
  let msg =
    (* input_message.json is not mandatory, but if provided, should be valid *)
    if invalid_optional_fname !f_input_message then
      msg ^ "Invalid input message\n"
    else msg
  in
  let msg =
    (* input_blockchain.json is mandatory *)
    if not @@ Sys.file_exists !f_input_blockchain then
      msg ^ "Invalid input blockchain state\n"
    else msg
  in
  let msg =
    (* input file is mandatory *)
    if not @@ Sys.file_exists !f_input then
      msg ^ "Invalid input contract file\n"
    else msg
  in
  (* Note: output file is optional, if it's missing we will output to stdout *)
  let msg =
    (* input_message.json and input_state.json / i_ipc_address+balance can either both be there or both absent *)
    if
      String.(
        !f_input_message <> ""
        && ( !f_input_state <> ""
             && (!i_ipc_address <> "" || Option.is_some !v_balance)
           || !f_input_state = ""
              && (!i_ipc_address = "" || Option.is_none !v_balance) ))
    then
      msg
      ^ "Input message provided, but either none or both of input state / (IPC \
         address and balance) provided\n"
    else msg
  in
  if not @@ String.is_empty msg then
    PrettyPrinters.fatal_error_noformat (usage ^ Printf.sprintf "%s\n" msg)

let parse args ~exe_name =
  reset ();
  let speclist =
    [
      ( "-version",
        Arg.Unit
          (fun () ->
            DebugMessage.pout
              (Core_kernel.sprintf "Scilla version: %s\n"
                 PrettyPrinters.scilla_version_string);
            if true then exit 0;
            (* if "true" to avoid warning on exit 0 *)
            ()),
        "Print Scilla version and exit" );
      ( "-init",
        Arg.String (fun x -> f_input_init := x),
        "Path to initialization json" );
      ( "-istate",
        Arg.String (fun x -> f_input_state := x),
        "Path to state input json" );
      ( "-imessage",
        Arg.String (fun x -> f_input_message := x),
        "Path to message input json" );
      ( "-ipcaddress",
        Arg.String (fun x -> i_ipc_address := x),
        "Socket address for IPC communication with blockchain for state access"
      );
      ( "-iblockchain",
        Arg.String (fun x -> f_input_blockchain := x),
        "Path to blockchain input json" );
      ("-o", Arg.String (fun x -> f_output := x), "Path to output json");
      ("-i", Arg.String (fun x -> f_input := x), "Path to scilla contract");
      ( "-tracefile",
        Arg.String (fun x -> f_trace_file := x),
        "Path to trace file. (prints to stdout if no file specified)" );
      ( "-tracelevel",
        Arg.String (fun x -> f_trace_level := x),
        "Trace level: none|stmt|exp. (default none)" );
      ( "-libdir",
        Arg.String
          (fun x ->
            let xl =
              if String.is_empty x then [] else Str.split (Str.regexp "[;:]") x
            in
            d_libs := !d_libs @ xl),
        "Path(s) to directory containing libraries separated by ':' (';' on \
         windows)" );
      ( "-gaslimit",
        Arg.String
          (fun i ->
            let g =
              try Stdint.Uint64.of_string i
              with _ ->
                PrettyPrinters.fatal_error_noformat
                  (Printf.sprintf "Invalid gaslimit %s\n" i)
            in
            v_gas_limit := g),
        "Gas limit" );
      ( "-balance",
        Arg.String
          (fun i ->
            let g =
              try Stdint.Uint128.of_string i
              with _ ->
                PrettyPrinters.fatal_error
                  (ErrorUtils.mk_error0
                     (Printf.sprintf "Invalid balance %s\n" i))
            in
            v_balance := Some g),
        "Account balance" );
      ("-pplit", Arg.Bool (fun b -> b_pp_lit := b), "Pretty print literals");
      ( "-jsonerrors",
        Arg.Unit (fun () -> b_json_errors := true),
        "Print errors in JSON format" );
      ( "-disable-pp-json",
        Arg.Unit (fun () -> b_pp_json := false),
        "Disable pretty printing of JSONs" );
    ]
  in

  let mandatory_usage =
    "Usage:\n" ^ exe_name ^ " -init init.json [-istate input_state.json]"
    ^ " -iblockchain input_blockchain.json [-imessage input_message.json]"
    ^ " [-o output.json] -i input.scilla -libdir /path/to/stdlib"
    ^ " -gaslimit limit" ^ "\n"
  in
  let optional_usage =
    String.concat ~sep:"\n  "
    @@ List.map speclist ~f:(fun (flag, _, desc) -> flag ^ " " ^ desc)
  in
  let usage = mandatory_usage ^ "\n  " ^ optional_usage ^ "\n" in
  let ignore_anon _ = () in
  let () =
    match args with
    | None -> Arg.parse speclist ignore_anon mandatory_usage
    | Some argv -> (
        try
          Arg.parse_argv ~current:(ref 0)
            (List.to_array @@ (exe_name :: argv))
            speclist ignore_anon mandatory_usage
        with Arg.Bad msg ->
          PrettyPrinters.fatal_error_noformat (Printf.sprintf "%s\n" msg) )
  in
  let () = process_trace () in
  let () = process_pplit () in
  let () = process_json_errors () in
  let () = process_json_validation () in
  let () = validate_main usage in
  {
    input_init = !f_input_init;
    input_state = !f_input_state;
    input_message = !f_input_message;
    input_blockchain = !f_input_blockchain;
    output = !f_output;
    input = !f_input;
    balance = (match !v_balance with Some v -> v | None -> Stdint.Uint128.zero);
    libdirs = !d_libs;
    gas_limit = !v_gas_limit;
    pp_json = !b_pp_json;
    ipc_address = !i_ipc_address;
  }
