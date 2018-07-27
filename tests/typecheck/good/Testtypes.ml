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
    let gold_path dir f = [dir; "typecheck"; "good"; "gold"; f ^ ".gold" ]
    let test_path f = ["typecheck"; "good"; f]
    let runner = "type-checker"      
    let tests = [
      "fun.scilla";
      "fun1.scilla";
      "addr.scilla";
      "app.scilla";
      "list1.scilla";
      "pm1.scilla";
      "pm2.scilla";
      "pm3.scilla";
      "pm4.scilla";
      "subst.scilla";
    ]
    let use_stdlib = false
  end)
