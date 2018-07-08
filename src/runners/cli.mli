(*
 * Copyright (c) 2018 - present Zilliqa, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

type ioFiles = {
    input_init : string;
    input_state : string;
    input_message : string;
    input_blockchain : string;
    output : string;
    input : string;
    libdirs : string list;
}

val parse : unit -> ioFiles
