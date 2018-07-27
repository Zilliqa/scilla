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
open DebugMessage
open Core.Result.Let_syntax

(* A path to standard library. *)
let stdlib_dir = 
  if (Array.length Sys.argv) <= 2
  then
    match Sys.getenv_opt scilla_stdlib_path with
    | Some d -> d
    | None -> printf "\n%s\n"
       ("A path to Scilla stdlib not found. Please set " ^ scilla_stdlib_path ^ 
       " environment variable, or pass as the second command-line argument for this script.\n" ^
       "Example:\n./bin/eval-runner list_sort.scilla ./src/stdlib/\n"); exit 1
  else Sys.argv.(2)

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



(* Parse external libraries. *)
let import_libs names ldirs =
  List.map (fun id -> 
    let name = get_id id in
    let errmsg = (sprintf "%s. " ("Failed to import library " ^ name)) in
    let dir = BatList.find_opt 
      (fun d -> Caml.Sys.file_exists (d ^ Filename.dir_sep ^ name ^ ".scilla")) 
      ldirs in
    let open Core in 
    let f = match dir with
      | None -> perr (errmsg ^ "Not found.\n") ; exit 1
      | Some d -> d ^ Filename.dir_sep ^ name ^ ".scilla" in
    try
      let parse_lib = FrontEndParser.parse_file ScillaParser.lmodule f in
      match parse_lib with
      | None -> perr (errmsg ^ "Failed to parse.\n"); exit 1
      | Some lib ->
        plog (sprintf "%s\n" "Successfully imported external library " ^ name);
        lib
    with | _ -> perr (errmsg ^ "Failed to parse.\n"); exit 1
    ) names 
