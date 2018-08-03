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
  "addr.scilla"; "app5.scilla"; "builtin1.scilla"; "cons.scilla";
  "hash3.scilla"; "map3.scilla"; "msg_error2.scilla"; "option.scilla";
  "pm1.scilla"; "app2.scilla"; "builtin2.scilla"; "hash4.scilla";
  "let.scilla"; "map4.scilla"; "msg_error3.scilla"; "pair1.scilla";
  "pm2.scilla"; "app3.scilla"; "builtin3.scilla"; "hash1.scilla";
  "map1.scilla"; "map5.scilla"; "msg_error.scilla"; "pair2.scilla";
  "pm3.scilla"; "app4.scilla"; "app.scilla"; "builtin_error1.scilla";
  "hash2.scilla"; "let-builtin.scilla"; "map2.scilla"; "map6.scilla";
  "msg.scilla"; "pair3.scilla"; "pm_app.scilla"; "pm_nesting.scilla";
  "string1.scilla"; "string2.scilla"; "nat_eq_foldl.scilla";
  "nat_eq_false.scilla"; "times_two.scilla"; "fib.scilla"; "id.scilla";
  "hof2.scilla"; "hof3.scilla"; "hof.scilla"; "list_map.scilla";
  "list_product.scilla"; "builtin-strings.scilla"; "list_filter.scilla";
  "list_head.scilla"; "list_tail.scilla"; "list_tail1.scilla";
  "list_tail2.scilla"; "list_append.scilla"; "list_reverse.scilla";
  "list_flatten.scilla"; "list_length.scilla"; "list_eq.scilla";
  "list_mem.scilla"; "list_forall.scilla"; "list_exists.scilla";
  "list_sort.scilla"; "list_find.scilla"; "list_zip.scilla";
  "list_zip_with.scilla"; "list_unzip.scilla"; "nat_to_int.scilla";
  "list_to_map.scilla"; "list_sort_eq.scilla"; "list_nth.scilla";
  "builtin5.scilla"; "map_to_list.scilla"; "type_subst1.scilla";
  "type_subst2.scilla"; "int_conversions.scilla"; "int_to_nat.scilla";
  "uint_conversions.scilla";
]

module Tests = TestUtil.DiffBasedTests(
  struct
    let gold_path dir f = [dir; "eval"; "exp"; "gold"; f ^ ".gold" ]
    let test_path f = ["eval"; "exp"; f]
    let runner = "eval-runner"
    let tests = explist
    let use_stdlib = true

  end)

