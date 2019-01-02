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
  "addr.scilla";
  "app2.scilla";
  "app3.scilla";
  "app4.scilla";
  "app5.scilla";
  "app.scilla";
  "builtin1.scilla";
  "builtin2.scilla"; 
  "builtin3.scilla";
  "builtin5.scilla";
  "builtin6.scilla";  
  "builtin-strings.scilla";
  "builtin-div.scilla";
  "builtin-div2.scilla";
  "builtin-div3.scilla";
  "builtin-div4.scilla";
  "builtin-pow.scilla";
  "builtin-rem.scilla";
  "builtin-rem2.scilla";
  "builtin-rem3.scilla";
  "builtin-badd.scilla";
  "builtin-bsub.scilla";
  "cons.scilla";
  "fib.scilla";
  "func_pair.scilla";
  "hash1.scilla";
  "hash2.scilla";  
  "hash3.scilla";
  "hash4.scilla";
  "hash5.scilla";
  "hof2.scilla";
  "hof3.scilla";
  "hof.scilla";
  "id.scilla";  
  "int_conversions.scilla";
  "int_to_nat.scilla";
  "keccak256_1.scilla";
  "keccak256_2.scilla";
  "keccak256_3.scilla";
  "keccak256_4.scilla";
  "keccak256_5.scilla";
  "ripemd160_1.scilla";
  "ripemd160_2.scilla";
  "ripemd160_5.scilla";
  "let-builtin.scilla";
  "let.scilla";
  "list_append.scilla";
  "list_eq.scilla";
  "list_exists.scilla";
  "list_filter.scilla";
  "list_find.scilla";
  "list_flatten.scilla";
  "list_forall.scilla";
  "list_head.scilla";
  "list_length.scilla";
  "list_map.scilla";
  "list_mem.scilla";
  "list_nth.scilla";
  "list_product.scilla";
  "list_reverse.scilla";
  "list_sort.scilla";
  "list_sort_eq.scilla";
  "list_tail1.scilla";
  "list_tail2.scilla";  
  "list_tail.scilla";
  "list_to_map.scilla"; 
  "list_unzip.scilla";
  "list_zip.scilla";
  "list_zip_with.scilla";  
  "map1.scilla";
  "map2.scilla";
  "map3.scilla";
  "map4.scilla";
  "map5.scilla";
  "map6.scilla";
  "map_to_list.scilla";
  "msg.scilla";
  "nat_eq_false.scilla";
  "nat_eq_foldl.scilla";
  "nat_to_int.scilla";
  "option.scilla";
  "pair1.scilla";
  "pair2.scilla";
  "pair3.scilla";
  "pm1.scilla";  
  "pm2.scilla";
  "pm3.scilla";
  "pm_app.scilla";
  "pm_nesting.scilla";
  "string1.scilla";
  "string2.scilla";
  "times_two.scilla";
  "to_bystr.scilla";
  "type_subst1.scilla";
  "type_subst2.scilla";
  "uint_conversions.scilla";
]

module Tests = TestUtil.DiffBasedTests(
  struct
    let gold_path dir f = [dir; "eval"; "exp"; "good"; "gold"; f ^ ".gold" ]
    let test_path f = ["eval"; "exp"; "good"; f]
    let runner = "eval-runner"
    let custom_args = []
    let tests = explist
    let exit_code : Unix.process_status = WEXITED 0

  end)

