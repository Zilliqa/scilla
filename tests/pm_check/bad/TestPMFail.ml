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

module Tests = TestUtil.DiffBasedTests(
  struct
    let gold_path dir f = [dir; "pm_check"; "bad"; "gold"; f ^ ".gold" ]
    let test_path f = ["pm_check"; "bad"; f]
    let runner = "type-checker"      
    let custom_args = []
    let additional_libdirs = []
    let tests = [
      "pm1.scilla";
      "pm2.scilla";
      "pm3.scilla";
      "pm_nesting1.scilla";
      "pm_nesting2.scilla";
      "pm_nesting3.scilla";
      "pm_unreachable1.scilla";
      "pm_unreachable2.scilla";
      "pm_unreachable_nesting1.scilla";
      "pm_unreachable_nesting2.scilla";
    ]
    let exit_code : Unix.process_status = WEXITED 1
  end)

let all_tests env = "pm_check_fail_tests" >::: [Tests.add_tests env]
