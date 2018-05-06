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
open Result.Let_syntax
open MonadUtil
open EvalUtil
open Eval

exception EvalError of string

(****************************************************)
(*  Initial inputs for bootstrapping the contract   *)
(****************************************************)
let crowdfunding_init_args =
  let init_bal = Big_int.zero_big_int in
  [
    ("owner", Address "a0x134234");
    ("max_block", BNum "100");
    ("goal", IntLit "5000")
  ], init_bal

let zil_game_init_args =
  let init_bal = Big_int.big_int_of_int 500 in
  let hs =
  "'=vB\007\146\226\237\205&\153\015\221\2034p\144\t\169T\145,\1411\222\142O\017?\191.\000" in
  [
    ("owner",    Address "a0x1342345563");
    ("puzzle", Sha256 hs);
    ("player_a", Address "a0x253454234");
    ("player_b", Address "a0x6734523432");
  ], init_bal

let init_args = [
  ("crowdfunding", crowdfunding_init_args);
  ("zil-game", zil_game_init_args)
]

let get_init_args name =
  let res = List.find_exn init_args ~f:(fun (k, _) -> k = name) in
  snd res

(****************************************************)
(*          Checking initialized libraries          *)
(****************************************************)

let check_libs libs name =
   let ls = init_libraries libs in
   (* Are libraries ok? *)
   match ls with
   | Ok res ->
       printf
         "\n[Initializing libraries]:\n%s\n\nLibraries for [%s] are on. All seems fine so far!\n\n"
         (* (Env.pp res) *)
         (String.concat ~sep:", " (List.map (List.rev res) ~f:fst))
         name
   | Error err ->
       (printf "Failed to initialize libraries:\n%s\n" err;
        raise @@ EvalError "Execution stopped")

(****************************************************)
(*     Checking initialized contract state          *)
(****************************************************)
let check_extract_cstate name res = match res with
  | Error err ->
      (printf "Failed to initialize fields:\n%s\n" err;
       raise @@ EvalError "Execution stopped")
  | Ok (_, cstate) ->
      (printf "[Initializing %s's fields]: Success!\n\n%s\n\n"
         name (ContractState.pp cstate);
       cstate)


(****************************************************)
(*              Main demo procedure                 *)
(****************************************************)
(* HOW TO RUN ME

After compilinig, run from the project root folder:

bin/scilla-runner crowdfunding

or 

bin/scilla-runner zil-game

*)

let () =
  let name = Sys.argv.(1) in
  let mod_path = sprintf "examples%scontracts%s%s"
      Filename.dir_sep Filename.dir_sep name in
  let filename = mod_path ^ Filename.dir_sep ^ "contract" in
  let parse_module =
    FrontEndParser.parse_file ScillaParser.cmodule filename in
  match parse_module with
  | None -> printf "%s\n" "Failed to parse input file."
  | Some cmod ->
      printf "\n[Parsing]:\nContract module [%s] is successfully parsed.\n"
        mod_path;
      (* Now initialize it *)
      let libs = cmod.libs in

      (* 1. Checking initialized libraries! *)
      check_libs libs mod_path;
 
      (* 2. Initializing the contract with arguments matching its parameters *)
      let (args, init_bal) = get_init_args name in
      let res = init_module cmod args init_bal in
      let cstate = check_extract_cstate name res in
      ()
      



      (* printf "%s\n"
         (sexp_of_cmodule sexp_of_loc cmod |> Sexplib.Sexp.to_string) *)

