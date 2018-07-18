(*
 * Copyright (c) 2018 - present Zilliqa, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)


module Tests = TestUtil.DiffBasedTests(
  struct
    let gold_path dir f = [dir; "checker"; "gold"; f ^ ".gold" ]
    let test_path f = ["checker"; f]
    let runner = "scilla-checker"
    let tests = [
      "crowdfunding.scilla"; "zil-game.scilla"; "fungible-token.scilla"; "auction.scilla"
    ]
    let use_stdlib = false
  end)
