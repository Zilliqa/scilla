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

open Core_kernel
open! Int.Replace_polymorphic_compare
open TestUtil

let () =
  run_tests
    [
      (* contract_tests should always be the first to be run. This is required
         * for us to be able to run _only_ contract_tests from the blockchain for
         * external IPC server tests. If the order changes, then the test_id of
         * these tests will change, resulting in the tests not being run.
         * See the Makefile target "test_extipcserver". *)
      Testcontracts.contract_tests;
      TestExps.all_tests;
      TestExpsFail.all_tests;
      Testtypes.all_tests;
      TestTypeFail.all_tests;
      TestPMFail.all_tests;
      TestChecker.all_tests;
      (* TestGasExpr.all_tests;
         TestGasContracts.all_tests; *)
    ]
