(*
 * Copyright (c) 2018 - present Zilliqa, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(* Available debug levels for functions in DbgMsg *)
type debug_kind =
  | Debug_None
  | Debug_Normal
  | Debug_Verbose

open Filename

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
      let num = Core.int_of_string numstr in
      let num' = get_highest_numbered_log files' in
        Core.Int.max num num'
    else
      0

(* Given a directory, look for consecutively named files
 * scilla-runner-[0-9]+.log and return the next in sequence *)
let create_log_filename dir =
  if not (Sys.file_exists dir) || 
     not (Sys.is_directory dir)
  then
    Unix.mkdir dir 0o766; (* Arbitrary *)
  let files = Sys.readdir dir in
  let num = get_highest_numbered_log (Array.to_list files) in
  dir ^ dir_sep ^ "scilla-runner-" ^ Core.Int.to_string (num+1) ^ ".log"

let get_debug_level () =
  !debug_level

let set_debug_level l =
  debug_level := l

let get_log_file () =
  if !log_file = ""
  then
    log_file := create_log_filename ("_build" ^ dir_sep ^ "logs");
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
