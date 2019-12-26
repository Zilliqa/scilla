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

open Core
open Core_bench.Simplified_benchmark

let print_tests groups =
  let open Core_bench in
  let open Ascii_table in
  List.iter groups ~f:(fun group ->
      let cells =
        List.map (Test.tests group)
          ~f:(fun test ->
              [ Test.name group
              ; Test.Basic_test.name test
              ])
      in simple_list_table
        ~display:Display.column_titles
        ["group"; "test"] cells)

let to_ms = Util.ns_to_ms_string

let print_results results =
  let open Ascii_table in
  let open Result in
  let cells = List.map results ~f:(fun res ->
      let ms = to_ms res.time_per_run_nanos in
      [res.benchmark_name; ms])
  in simple_list_table
    ~display:Display.column_titles
    ["benchmark name"; "ms"] cells

let print_comparison ~previous ~current ~deltas =
  let open Ascii_table in
  let open Result in
  let cells = List.map3_exn previous current deltas
      ~f:(fun prev curr delta ->
          [ curr.benchmark_name
          ; to_ms prev.time_per_run_nanos
          ; to_ms curr.time_per_run_nanos
          ; to_ms delta.time_per_run_nanos
          ])
  in simple_list_table
    ~display:Display.column_titles
    [ "benchmark name"
    ; "previous (ms)"
    ; "current (ms)"
    ; "delta"
    ] cells
