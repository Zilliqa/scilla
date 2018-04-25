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

let () =
  let filename = Sys.argv.(1) in
  match FrontEndParser.parse_file ScillaParser.stmts filename with
    | Some stmts ->
        List.iter (fun l -> printf "%s \n"
             (sexp_of_stmt sexp_of_unit l |> Sexplib.Sexp.to_string)) stmts
    | None ->
      printf "%s\n" "Failed to parse input file."
  


