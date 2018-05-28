(*
 * Copyright (c) 2018 - present Zilliqa, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open GlobalConfig
open Core

(* Prints to log file *)
let plog msg =
  let fname = get_log_file () in
  let h = Out_channel.create ~append:true fname in
    Out_channel.output_string h msg;
    Out_channel.close h

(* Prints to stdout and log file *)
let pout msg =
  Out_channel.output_string Out_channel.stdout msg;
  plog ("stdout: " ^ msg ^ "\n")

(* Prints to stderr and log file *)
let perr msg =
  Out_channel.output_string Out_channel.stderr msg;
  plog ("stderr: " ^ msg ^ "\n")
