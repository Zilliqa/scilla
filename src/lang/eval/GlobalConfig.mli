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

val get_debug_level : unit -> debug_kind
val set_debug_level : debug_kind -> unit
val get_log_file : unit -> string
val set_log_file : string -> unit
