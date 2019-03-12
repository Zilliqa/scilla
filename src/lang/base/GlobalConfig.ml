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

open Core
open ScillaUtil.FilePathInfix

(* Available debug levels for functions in DbgMsg *)
type debug_kind =
  | Debug_None
  | Debug_Normal
  | Debug_Verbose

let debug_level = ref Debug_None
let log_file = ref ""

let rec get_highest_numbered_log files =
  match files with
  | [] ->
    0
  | file :: files' ->
    let found = Str.string_match (Str.regexp "scilla-runner-\\([0-9]+\\)\\.log") file 0 in
    let substr = Str.matched_string file in
    if found
    then
      let numstr = Str.matched_group 1 substr in
      let num = Int.of_string numstr in
      let num' = get_highest_numbered_log files' in
        Int.max num num'
    else
      0

(* Given a directory, look for consecutively named files
 * scilla-runner-[0-9]+.log and return the next in sequence *)
let create_log_filename dir =
  if not (Caml.Sys.file_exists dir) ||
     not (Caml.Sys.is_directory dir)
  then
    Caml.Unix.mkdir dir 0o766; (* Arbitrary *)
  let files = Sys.readdir dir in
  let num = get_highest_numbered_log (Array.to_list files) in
  dir ^/ "scilla-runner-" ^ Int.to_string (num+1) ^. "log"

let get_debug_level () =
  !debug_level

let set_debug_level l =
  debug_level := l

let get_log_file () =
  if !log_file = ""
  then
    log_file := create_log_filename ("_build" ^/ "logs");
  !log_file

let set_log_file s =
  log_file := s

(* Available trace levels *)
type trace_kind =
  | Trace_None
  | Trace_Statement
  | Trace_Expression

let trace_level = ref Trace_None
let trace_file = ref ""

let get_trace_level () =
  !trace_level

let set_trace_level l =
  trace_level := l

let get_trace_file () =
  !log_file

let set_trace_file s =
  trace_file := s

let pp_lit = ref true

let set_pp_lit b =
  pp_lit := b

let get_pp_lit () = !pp_lit

let json_errors = ref false

let set_use_json_errors b =
  json_errors := b

let use_json_errors () = !json_errors

let validate_json_b = ref false

let set_validate_json b =
  validate_json_b := b

let validate_json () = !validate_json_b

module StdlibTracker = struct

(* Environment variable: where to look for stdlib.
 * Multiple entries can be specified, separated by ':' or ';'.
 *)
let scilla_stdlib_env = "SCILLA_STDLIB_PATH"

(* List of directories to look for stdlib *)
let stdlib_dirs = ref []

(* List of directories to look for stdlib.
 * Entries from scilla_stdlib_env will be first. *)
let get_stdlib_dirs () =
  let env_dirs =
    Option.value_map
      (Caml.Sys.getenv_opt scilla_stdlib_env)
      ~f:FilePath.path_of_string
      ~default:[]
  in
    List.append env_dirs !stdlib_dirs

(* Update stdlib dirs with more locations *)
let add_stdlib_dirs dirs =
  stdlib_dirs := List.append !stdlib_dirs dirs

(* File extension for Scilla contracts. *)
let file_extn_contract = "scilla"
(* File extension for Scilla libraries. *)
let file_extn_library = "scillib"

(* Try find library "name" in known locations *)
let find_lib_dir name =
  let dirs = get_stdlib_dirs () in
  BatList.find_opt
    (fun d -> Caml.Sys.file_exists (d ^/ name ^. file_extn_library)) dirs

end
