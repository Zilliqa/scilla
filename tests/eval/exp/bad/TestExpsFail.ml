(*
 * Copyright (c) 2018 - present Zilliqa, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)
let explist = [
  "app_error1.scilla"; "app_error2.scilla"; "builtin4.scilla";
  "builtin_error1.scilla"; "builtin-overflow1.scilla";
  "builtin-overflow2.scilla"; "builtin-overflow3.scilla";
  "builtin-overflow4.scilla"; "builtin-overflow5.scilla";
  "builtin-overflow6.scilla"; "builtin-overflow7.scilla";
  "builtin-overflow8.scilla"; "builtin-overflow9.scilla";
  "builtin-overflow10.scilla"; "builtin-overflow11.scilla";
  "let-error.scilla"; "string_error1.scilla";
]

module Tests = TestUtil.DiffBasedTests(
  struct
    let gold_path dir f = [dir; "eval"; "exp"; "bad"; "gold"; f ^ ".gold" ]
    let test_path f = ["eval"; "exp"; "bad"; f]
    let runner = "eval-runner"
    let tests = explist
    let use_stdlib = true

  end)

