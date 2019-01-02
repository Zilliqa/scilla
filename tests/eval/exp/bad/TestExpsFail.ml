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

open OUnit2

let explist = [
  "app_error1.scilla";
  "app_error2.scilla";
  "builtin4.scilla";
  "builtin_error1.scilla";
  "builtin-divzero.scilla";
  "builtin-divzero2.scilla";
  "builtin-divzero3.scilla";
  "builtin-divzero4.scilla";
  "builtin-remzero.scilla";
  "builtin-overflow10.scilla";
  "builtin-overflow11.scilla";
  "builtin-overflow12.scilla";
  "builtin-overflow1.scilla";
  "builtin-overflow2.scilla";
  "builtin-overflow3.scilla";
  "builtin-overflow4.scilla";
  "builtin-overflow5.scilla";
  "builtin-overflow6.scilla";
  "builtin-overflow7.scilla";
  "builtin-overflow8.scilla";
  "builtin-overflow9.scilla";
  "builtin-pow.scilla";
  "builtin-pow2.scilla";
  "msg_error2.scilla";
  "msg_error3.scilla";
  "msg_error.scilla";
  "let-error.scilla";
  "string_error1.scilla";
]

module Tests = TestUtil.DiffBasedTests(
  struct
    let gold_path dir f = [dir; "eval"; "exp"; "bad"; "gold"; f ^ ".gold" ]
    let test_path f = ["eval"; "exp"; "bad"; f]
    let runner = "eval-runner"
    let custom_args = []
    let tests = explist
    let exit_code : Unix.process_status = WEXITED 1

  end)

let all_tests env = "eval_exp_fail_tests" >::: [Tests.add_tests env]
