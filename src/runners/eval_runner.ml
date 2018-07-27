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
open EvalUtil
open Recursion
open GlobalConfig
open Core.Result.Let_syntax
open RunnerUtil

let () =
  if (Array.length Sys.argv) < 2 || (Array.length Sys.argv) > 3
  then
    (printf "%s\n" ("Usage: " ^ Sys.argv.(0) ^ " /path/to/exp.scilla [/path/to/stdlib]");
    exit 1)
  else
  let filename = Sys.argv.(1) in
  match FrontEndParser.parse_file ScillaParser.exps filename with
  | Some [e] ->
      (* Since this is not a contract, we have no in-contract lib defined. *)
      let clib = { lname = asId "dummy"; lentries = [] } in
      let elibs = parse_stdlib stdlib_dir in
      let envres = Eval.init_libraries clib elibs in
      let env = (match envres with
        | Ok (env') -> env'
        | Error err ->
          printf "Failed to initialize stdlib. Evaluation halted: %s\n" err;
          exit 1;) in
      let lib_fnames = List.map (fun (name, _) -> name) env in
      let res = Eval.exp_eval e env in
      (match res with
      | Ok (v, env) ->
          printf "%s\n" (Eval.pp_result res lib_fnames)
      | Error _ -> printf "Failed execution:\n%s\n" (Eval.pp_result res lib_fnames))
  | Some _ | None ->
      printf "%s\n" "Failed to parse input file."
  


