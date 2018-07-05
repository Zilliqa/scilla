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
open DebugMessage

let () =
  if (Array.length Sys.argv) <> 2
  then
    (perr (sprintf "Usage: %s foo.scilla\n" Sys.argv.(0));
    exit 1)
  else
    (* Testsuite runs this executable with cwd=tests and ends
       up complaining about missing _build directory for logger.
       So disable the logger.
     *)
    GlobalConfig.set_debug_level GlobalConfig.Debug_None;
    let parse_module =
      FrontEndParser.parse_file ScillaParser.cmodule Sys.argv.(1) in
    match parse_module with
    | None -> plog (sprintf "%s\n" "Failed to parse input file.")
    | Some cmod ->
      plog (sprintf "\n[Parsing]:\nContract module [%s] is successfully parsed.\n" Sys.argv.(1));
      pout (sprintf "%s\n" (JSON.ContractInfo.get_string cmod.contr))
