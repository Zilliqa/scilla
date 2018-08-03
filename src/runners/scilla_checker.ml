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
open DebugMessage
open TypeChecker
open MonadUtil
open Result.Let_syntax
open RunnerUtil

(* Check that the module parses *)
let check_parsing ctr = 
    let parse_module =
      FrontEndParser.parse_file ScillaParser.cmodule ctr in
    match parse_module with
    | None -> fail (sprintf "%s\n" "Failed to parse input file.")
    | Some cmod -> 
        plog @@ sprintf
          "\n[Parsing]:\nContract module [%s] is successfully parsed.\n" ctr;
        pout (sprintf "%s\n" (JSON.ContractInfo.get_string cmod.contr));
        pure cmod

(* Type check the contract with external libraries *)
let check_typing cmod elibs =
  let res = type_module cmod elibs in
  match res with
  | Error msg -> pout @@ sprintf "\n%s\n\n" msg; res
  | Ok _ ->
      let cn = get_id cmod.cname in 
        plog @@ sprintf
          "\n[Type Checking]:\nContract module [%s] is well-typed.\n"
          cn;
        res

let () =
  if (Array.length Sys.argv) < 2
  then
    (perr (sprintf "Usage: %s foo.scilla\n" Sys.argv.(0))
    )
  else (
    GlobalConfig.set_debug_level GlobalConfig.Debug_None;
    (* Testsuite runs this executable with cwd=tests and ends
       up complaining about missing _build directory for logger.
       So disable the logger. *)
    let _ = (
      let%bind cmod = check_parsing Sys.argv.(1) in
      let lib_dirs = [stdlib_dir()] in
      let elibs = import_libs cmod.elibs lib_dirs in
      check_typing cmod elibs)
    in ())
