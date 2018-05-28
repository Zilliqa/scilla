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

(* Given a directory, look for consecutively named files
 * scilla-runner-[0-9]+.log and return the next in sequence *)
let create_log_filename dir =
  if not (Sys.file_exists dir) || 
     not (Sys.is_directory dir)
  then
    Unix.mkdir dir 0o766; (* Arbitrary *)
  dir ^ dir_sep ^ "scilla-runner.log"

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
