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
    let gold_path dir f = [dir; "checker"; "bad"; "gold"; f ^ ".gold" ]
    let test_path f = ["checker"; "bad"; f]
    let runner = "scilla-checker"
    let tests = [
      "bad_fields1.scilla";
      "bad_fields2.scilla";
      "bad_fields2.scilla";
      "unbound.scilla";
      "event_bad1.scilla";
      "lib_bad1.scilla";
      "zil_mod.scilla";
      "mappair2.scilla";
      "mappair.scilla";
      "multiple_accepts.scilla"
    ]
    let use_stdlib = true
  end)
