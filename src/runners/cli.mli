(*
 * Copyright (c) 2018 - present Zilliqa, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

val f_input_init : string ref
val f_input_state : string ref
val f_input_message : string ref
val f_input_blockchain : string ref
val f_output : string ref
val f_input : string ref

val parse : unit -> unit
