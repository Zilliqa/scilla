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

let main =
  let bech32_tests = TestBech32.bech32_tests in
  let integer256_tests = TestInteger256.integer256_tests in
  let arith_builtin_tests = TestSafeArith.arith_builtin_tests in
  let syntax_tests = TestSyntax.syntax_tests in
  let all_tests = "all_tests" >:::
    [
      bech32_tests; integer256_tests; arith_builtin_tests;
      syntax_tests;
    ] in
  
  (* Run all tests *)
  run_test_tt_main all_tests
