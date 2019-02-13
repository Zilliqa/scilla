(*
  This file is part of scilla.

  Copyright (c) 2018 - present Zilliqa Research Pvt. Ltd.
  
  scilla is free software: you can redistribute it and/or modify it under the
  terms of the GNU General Public License as published by the Free Software
  Foundation, either version 3 of the License, or (at your option) any later
  version.
 
  scilla is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
  A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
 
  You should have received a copy of the GNU General Public License along with
  scilla.  If not, see <http://www.gnu.org/licenses/>.
*)

module Tests = TestUtil.DiffBasedTests(
  struct
    let gold_path dir f = [dir; "checker"; "good"; "gold"; f ^ ".gold" ]
    let test_path f = ["contracts"; f]
    let runner = "scilla-checker"
    let custom_args = ["-cf"; "-contractinfo"]
    let additional_libdirs = []
    let tests = [
      "auction.scilla";
      "bookstore.scilla";
      "cfinvoke.scilla";
      "chain-call-balance-1.scilla";
      "chain-call-balance-2.scilla";
      "chain-call-balance-3.scilla";
      "crowdfunding.scilla";
      "ecdsa.scilla";
      "empty.scilla";
      "fungible-token.scilla";
      "helloWorld.scilla";
      "inplace-map.scilla";
      "map_key_test.scilla";
      "mappair.scilla";
      "multiple-msgs.scilla";
      "nonfungible-token.scilla";
      "one-msg1.scilla";
      "one-msg.scilla";
      "ping.scilla";
      "pong.scilla";
      "schnorr.scilla"; 
      "simple-dex.scilla";
      "wallet.scilla";
      "zil-game.scilla";  
    ]
    let exit_code : Unix.process_status = WEXITED 0
  end)

module CheckerTests = TestUtil.DiffBasedTests(
  struct
    let gold_path dir f = [dir; "checker"; "good"; "gold"; f ^ ".gold" ]
    let test_path f = ["checker"; "good"; f]
    let runner = "scilla-checker"
    let custom_args = ["-cf"; "-contractinfo"]
    let additional_libdirs = []
    let tests = [
      "adt_test.scilla";
      "missing-accepts.scilla";
      "multiple-accepts.scilla";
      "one-accept.scilla"; 
      "one-transition-accepts.scilla";
      "one-transition-might-accept.scilla";
    ]
    let exit_code : Unix.process_status = WEXITED 0
  end)

module ShogiTests = TestUtil.DiffBasedTests(
  struct
    let gold_path dir f = [dir; "checker"; "good"; "gold"; f ^ ".gold" ]
    let test_path f = ["contracts"; f]
    let runner = "scilla-checker"
    let custom_args = ["-cf"; "-contractinfo"]
    let additional_libdirs = [[ "contracts"; "shogi_lib"]]
    let tests = [
      "shogi.scilla";
    ]
    let exit_code : Unix.process_status = WEXITED 0
  end)
