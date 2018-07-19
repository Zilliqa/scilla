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

let parse_stdlib stdlib_dir =
  let files = Array.to_list (Sys.readdir stdlib_dir) in
  let f file' = 
    let file = stdlib_dir ^ Filename.dir_sep ^ file' in
    let name = Filename.remove_extension file in
    let errmsg = (sprintf "%s. " ("Failed to import library " ^ name)) in
    try
      let parse_lib = FrontEndParser.parse_file ScillaParser.lmodule file in
      match parse_lib with
      | None -> fprintf stderr "%s" (errmsg ^ "Failed to parse.\n"); exit 1
      | Some lib ->
        lib
    with | _ -> fprintf stderr "%s" (errmsg ^ "Failed to parse.\n"); exit 1
  in
    List.map f files

let () =
  if (Array.length Sys.argv) < 2 || (Array.length Sys.argv) > 3
  then
    (printf "%s\n" ("Usage: " ^ Sys.argv.(0) ^ " /path/to/exp.scilla [/path/to/stdlib]");
    exit 1)
  else
  let stdlib_dir = 
    if (Array.length Sys.argv) = 2
    then
      match Sys.getenv_opt scilla_stdlib_path with
      | Some d -> d
      | None -> printf "%s\n" ("Scilla stdlib not found. please set " ^ scilla_stdlib_path ^ 
        " environment variable, or specify on the command line\n"); exit 1
    else
      Sys.argv.(2) in
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
  


