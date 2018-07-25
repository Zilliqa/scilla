
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
    let gold_path dir f = [dir; "typecheck"; "bad"; "gold"; f ^ ".gold" ]
    let test_path f = ["typecheck"; "bad"; f]
    let runner = "type-checker"      
    let tests = [
      "fun2.scilla";
      "fun3.scilla";
      "list-error.scilla";
      "list-error2.scilla";
      "pm-error1.scilla";
      "pm-error2.scilla";
    ]
    let use_stdlib = false
  end)

