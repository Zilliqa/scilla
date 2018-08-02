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
      "zip.scilla";
    ]
    let use_stdlib = false
  end)
