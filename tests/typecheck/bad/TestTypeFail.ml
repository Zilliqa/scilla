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
    let gold_path dir f = [dir; "typecheck"; "bad"; "gold"; f ^ ".gold" ]
    let test_path f = ["typecheck"; "bad"; f]
    let runner = "type-checker"      
    let tests = [
      "fun2.scilla";
      "fun3.scilla";
      "list-error.scilla";
      "list-error2.scilla";
      "list-lit.scilla";
      "list-lit2.scilla";
      "pm-error1.scilla";
      "pm-error2.scilla";
      "map-error.scilla";
      "map-lit.scilla";
      "nth-error.scilla";
      "folder-error.scilla";
    ]
    let use_stdlib = false
  end)

