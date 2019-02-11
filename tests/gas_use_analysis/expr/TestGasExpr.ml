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

let explist = [
  "app.scilla";
  "app2.scilla";
  "app3.scilla";
  "app4.scilla";
  "builtin-strings.scilla";
  "builtin1.scilla";
  "list_append.scilla";
  "list_eq.scilla";
  "list_head.scilla";
  "list_map.scilla";
  "list_sort.scilla";
  "list_sink_down.scilla";
  "list_tail.scilla";
  "list_zip.scilla";
  "list_zip_with.scilla";
]

module Tests = TestUtil.DiffBasedTests(
  struct
    let gold_path dir f = [dir; "gas_use_analysis"; "expr"; "gold"; f ^ ".gold" ]
    let test_path f = ["gas_use_analysis"; "expr"; f]
    let runner = "type-checker"
    let custom_args = ["-gua"; "-contractinfo"]
    let additional_libdirs = []
    let tests = explist
    let exit_code : Unix.process_status = WEXITED 0

  end)

