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

(****************************************************)
(*        Inputs for bootstrapping the contracts    *)
(****************************************************)

(* Initial arguments for crowdfunding *)
let owner = "0x1234567890123456789012345678901234567890"
  
let crowdfunding_init_args =
  let init_bal = Big_int.zero_big_int in
  [
    ("owner", Address owner);
    ("max_block", BNum "199");
    ("goal", IntLit "500")
  ], init_bal


(* Initial arguments for zil-game *)
let player_a = "0xabcdeabcde123456789012345678901234567890"
let player_b = "0xffcdeabcde123456789012345678901234567890"

let zil_game_init_args =
  let init_bal = Big_int.big_int_of_int 500 in
  let hs = "0x123456789012345678901234567890123456789012345678901234567890abcd" in
  [
    ("owner",    Address owner);
    ("puzzle", Sha256 hs);
    ("player_a", Address player_a);
    ("player_b", Address player_b);
  ], init_bal

let init_args = [
  ("crowdfunding", crowdfunding_init_args);
  ("zil-game", zil_game_init_args)
]

let get_init_args name =
  let res = List.find_exn init_args ~f:(fun (k, _) -> k = name) in
  snd res

(****************************************************)
(*               Blockchain "states"                *)
(****************************************************)

let bc_states =
  [
    [("BLOCKNUMBER", BNum "100")];
    [("BLOCKNUMBER", BNum "200")];
    [("BLOCKNUMBER", BNum "300")];
    [("BLOCKNUMBER", BNum "400")];
    [("BLOCKNUMBER", BNum "500")];
  ]

let get_bc_state i = List.nth_exn bc_states i

(****************************************************)
(*     Incoming messages for crowdfunding           *)
(****************************************************)

let cf_donate1 =
  Msg [
    ("_tag", StringLit "Donate");
    ("sender", Address "0x12345678901234567890123456789012345678ab");
    ("_amount", IntLit "100");
  ]

let cf_donate2 =
  Msg [
    ("_tag", StringLit "Donate");
    ("sender", Address "0x12345678901234567890123456789012345678cd");
    ("_amount", IntLit "200");
  ]

let cf_claim1 =
  Msg [
    ("_tag", StringLit "ClaimBack");
    ("sender", Address "0x12345678901234567890123456789012345678ab");
    ("_amount", IntLit "0");
  ]

let cf_get_funds =
  Msg [
    ("_tag", StringLit "GetFunds");
    ("sender", Address "0x12345678901234567890123456789012345678cd");
    ("_amount", IntLit "0");
  ]

(* Blockchain states and incoming messages for crowdfunding *)
let cf_msgs_bcs =
  [(get_bc_state 0, cf_donate1);
   (get_bc_state 0, cf_donate2);
   (get_bc_state 0, cf_donate2);
   (get_bc_state 1, cf_get_funds);
   (get_bc_state 2, cf_claim1);]

(****************************************************)
(*       Incoming messages for zil-game             *)
(****************************************************)

(* TODO: fiddle with hashes *)
let hs1 =
  "0x123456789012345678901234567890123456789012345678901234567890abff"

let hs2 = "0x123456789012345678901234567890123456789012345678901234567890ab12"

let zg_play1 =
  Msg [
    ("_tag", StringLit "Play");
    ("sender", Address player_a);
    ("guess", Sha256 hs1);    
    ("_amount", IntLit "0");
  ]

let zg_play2 =
  Msg [
    ("_tag", StringLit "Play");
    ("sender", Address player_b);
    ("guess", Sha256 hs2);    
    ("_amount", IntLit "0");
  ]

let zg_claim1 =
  Msg [
    ("_tag", StringLit "ClaimReward");
    ("sender", Address player_a);
    ("solution", IntLit "42");    
    ("_amount", IntLit "0");
  ]

let zg_claim2 =
  Msg [
    ("_tag", StringLit "ClaimReward");
    ("sender", Address player_b);
    ("solution", IntLit "43");    
    ("_amount", IntLit "0");
  ]

(* Blockchain states and incoming messages for zil-game *)
let zg_msgs_bcs =
  [(get_bc_state 0, zg_play1);
   (get_bc_state 0, zg_claim1);
   (get_bc_state 1, zg_play2);
   (get_bc_state 1, zg_claim1);
   (get_bc_state 2, zg_claim2)]

(*************************************************)
(*         Get schedule for acontract            *)
(*************************************************)

let schedules = [
  ("crowdfunding", cf_msgs_bcs);
  ("zil-game", zg_msgs_bcs)
]

let get_context name i =
  let res = snd @@ List.find_exn schedules ~f:(fun (k, _) -> k = name) in
  List.nth_exn res i
