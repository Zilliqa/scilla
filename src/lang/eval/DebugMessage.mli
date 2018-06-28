(*
 * Copyright (c) 2018 - present Zilliqa, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

val plog : string -> unit (* Prints to log file *)
val pout : string -> unit (* Prints to stdout and log file *)
val perr: string -> unit (* Prints to stderr and log file *)
val ptrace : string -> unit (* Prints to trace file, if set, or to stdout. *)
