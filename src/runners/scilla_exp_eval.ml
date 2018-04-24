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

(* let file = ref "" *)
(* let args = [] *)
(* let usage = "Usage: ./main <options> [file] (stdin by default)" *)

let () =
  let filename = Sys.argv.(1) in
  match FrontEndParser.parse_file ScillaParser.library filename with
    | Some libs ->
        List.iter (fun l -> printf "%s \n"
                      (sexp_of_lib_entry sexp_of_unit l |> Sexplib.Sexp.to_string)) libs
    | None ->
      printf "%s\n" "Failed to parse input file."
  


