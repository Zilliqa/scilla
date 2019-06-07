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
  "app.scilexp";
  "app2.scilexp";
  "app3.scilexp";
  "app4.scilexp";
  "builtin-strings.scilexp";
  "builtin1.scilexp";
  "builtin-bech32-1.scilexp";
  "builtin-bech32-2.scilexp";
  "list_append.scilexp";
  "list_eq.scilexp";
  "list_head.scilexp";
  "list_map.scilexp";
  "list_sort.scilexp";
  "list_sink_down.scilexp";
  "list_tail.scilexp";
  "list_zip.scilexp";
  "list_zip_with.scilexp";
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

