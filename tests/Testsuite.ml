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
open TestUtil
open ScillaUtil.FilePathInfix

let main =
  let bin_dir_default = (Sys.getcwd () ^/ "bin") in
  let tests_dir_default = (Sys.getcwd () ^/ "tests") in
  let stdlib_dir_default = (Sys.getcwd() ^/ "src" ^/ "stdlib") in
  let bin_dir = Conf.make_string "bin_dir" bin_dir_default "Directory containing binaries" in
  let tests_dir = Conf.make_string "tests_dir" tests_dir_default "Directory containing tests" in
  let stdlib_dir = Conf.make_string "stdlib_dir" stdlib_dir_default "Directory containing stdlib" in
  let print_cli = Conf.make_bool "print_cli" false "Print command line arguments used for test(s)" in
  let update_gold = Conf.make_bool "update_gold" false "Ignore compare mismatch and update gold file(s)" in
  let print_diff = Conf.make_bool "print_diff" false "Print the diff between gold file and actual output" in

  let env : tsuite_env = {
    bin_dir = bin_dir;
    tests_dir = tests_dir; stdlib_dir = stdlib_dir;
    print_cli = print_cli; update_gold = update_gold;
    print_diff = print_diff;
  } in
  (* Add calls to new tests from here *)
  let contract_tests = Testcontracts.add_tests env in
  let exp_tests_good = TestExps.Tests.add_tests env in
  let exp_tests_bad = TestExpsFail.Tests.add_tests env in
  let type_tests_good = Testtypes.all_tests env in
  let type_tests_bad = TestTypeFail.all_tests env in
  let pm_tests_bad = TestPMFail.all_tests env in
  let checker_tests = TestChecker.checker_tests env in
  let integer256_tests = TestInteger256.integer256_tests in
  let polynomial_tests = TestPolynomial.polynomial_tests in
  let signature_tests = TestSignatures.signature_tests env in
  (* let gas_expr_tests = TestGasExpr.Tests.add_tests env in
  let gas_contract_tests = TestGasContracts.Tests.add_tests env in *)
  let syntax_tests = TestSyntax.syntax_tests in
  let arith_builtin_tests = TestSafeArith.arith_builtin_tests in
  let bech32_tests = TestBech32.bech32_tests in

  let all_tests = "all_tests" >:::
                  [type_tests_bad; type_tests_good; exp_tests_good; exp_tests_bad;
                   pm_tests_bad; signature_tests; polynomial_tests; (*gas_expr_tests;
                   gas_contract_tests; *)bech32_tests;
                   contract_tests; checker_tests; integer256_tests; syntax_tests;
                   arith_builtin_tests] in

  (* Run all tests *)
  run_test_tt_main all_tests
