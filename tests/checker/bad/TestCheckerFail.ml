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
    let custom_args = ["-cf"]
    let lib_override = None
    let tests = [
      "bad_fields1.scilla";
      "bad_fields2.scilla";
      "bad_fields3.scilla";
      "bad_fields4.scilla";
      "bad_message1.scilla";
      "send_event1.scilla";
      "send_event2.scilla";
      "unbound.scilla";
      "event_bad1.scilla";
      "event_bad2.scilla";
      "lib_bad1.scilla";
      "zil_mod.scilla";
      "mappair2.scilla";
      "mappair.scilla";
      "inplace-map.scilla";
      "homonymous_vars.scilla";
      "homonymous_vars2.scilla";
      "homonymous_vars3.scilla";
      "bad_adt_1.scilla";
      "bad_adt_2.scilla";
      "bad_adt_3.scilla";
      "bad_adt_4.scilla";
      "unserializable_param.scilla";
      "unstorable_adt.scilla";
    ]
    let exit_code : Unix.process_status = WEXITED 1
  end)

module LibTests = TestUtil.DiffBasedTests(
  struct
    let gold_path dir f = [dir; "checker"; "bad"; "gold"; f ^ ".gold" ]
    let test_path f = ["checker"; "bad"; f]
    let runner = "scilla-checker"
    let custom_args = ["-cf"]
    let lib_override = Some ["checker"; "bad"; "lib"]
    let tests = [
      "bad_adt_lib_1.scilla";
      "bad_adt_lib_2.scilla";
      "bad_adt_lib_3.scilla";
      "bad_adt_lib_4.scilla";
      "bad_adt_lib_5.scilla";
      "bad_adt_lib_6.scilla";
    ]
    let exit_code : Unix.process_status = WEXITED 1
  end)
