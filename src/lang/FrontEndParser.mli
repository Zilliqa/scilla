(*
 * Copyright (c) 2018 - present Zilliqa, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open Syntax

val parse_file : ((Lexing.lexbuf -> ScillaParser.token) ->
                  Lexing.lexbuf -> 'a) -> string -> 'a option
