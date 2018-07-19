(*
 * Copyright (c) 2018 - present Zilliqa, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open Core
open Printf
open Sexplib.Std
open Syntax
open TypeUtil
open TypeChecker
open Recursion

module SimpleTEnv = MakeTEnv(PlainTypes)
open SimpleTEnv


let () =
  let filename = Sys.argv.(1) in
  match FrontEndParser.parse_file ScillaParser.exps filename with
  | Some [e] ->
      let recs = List.map ~f:(fun (a, _, c) -> (a, c)) recursion_principles in
      let tenv = TEnv.addTs TEnv.mk recs in
      let res = TypeChecker.get_type e tenv in
      (match res with
      | Ok res ->
          printf "%s\n" (pp_typ res.tp)
      | Error s -> printf "Failed execution:\n%s\n" s)
  | Some _ | None ->
      printf "%s\n" "Failed to parse input file."
  


