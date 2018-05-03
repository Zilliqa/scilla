(*
 * Copyright (c) 2018 - present Zilliqa, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)


open Printf
open Sexplib.Std
open Syntax
    open Core

let () =
  let filename = Sys.argv.(1) in
  match FrontEndParser.parse_file ScillaParser.cmodule filename with
  | Some cs ->
      printf "%s \n" (sexp_of_cmodule sexp_of_loc cs |> Sexplib.Sexp.to_string)
    | None ->
      printf "%s\n" "Failed to parse input file."
  


