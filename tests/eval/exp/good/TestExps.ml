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
  "addr.scilexp";
  "app2.scilexp";
  "app3.scilexp";
  "app4.scilexp";
  "app5.scilexp";
  "app.scilexp";
  "builtin1.scilexp";
  "builtin2.scilexp"; 
  "builtin3.scilexp";
  "builtin5.scilexp";
  "builtin6.scilexp";  
  "builtin-strings.scilexp";
  "builtin-div.scilexp";
  "builtin-div2.scilexp";
  "builtin-div3.scilexp";
  "builtin-div4.scilexp";
  "builtin-pow.scilexp";
  "builtin-rem.scilexp";
  "builtin-rem2.scilexp";
  "builtin-rem3.scilexp";
  "builtin-badd.scilexp";
  "builtin-bsub.scilexp";
  "builtin-bech32-1.scilexp";
  "builtin-bech32-2.scilexp";
  "cons.scilexp";
  "fib.scilexp";
  "func_pair.scilexp";
  "hash1.scilexp";
  "hash2.scilexp";  
  "hash3.scilexp";
  "hash4.scilexp";
  "hash5.scilexp";
  "hash_map_stable1.scilexp";
  "hash_map_stable2.scilexp";
  "hash_map_stable3.scilexp";
  "hof2.scilexp";
  "hof3.scilexp";
  "hof.scilexp";
  "id.scilexp";  
  "int_conversions.scilexp";
  "int_to_nat.scilexp";
  "keccak256_1.scilexp";
  "keccak256_2.scilexp";
  "keccak256_3.scilexp";
  "keccak256_4.scilexp";
  "keccak256_5.scilexp";
  "ripemd160_1.scilexp";
  "ripemd160_2.scilexp";
  "ripemd160_5.scilexp";
  "let-builtin.scilexp";
  "let.scilexp";
  "list_append.scilexp";
  "list_eq.scilexp";
  "list_exists.scilexp";
  "list_filter.scilexp";
  "list_find.scilexp";
  "list_flatten.scilexp";
  "list_forall.scilexp";
  "list_head.scilexp";
  "list_length.scilexp";
  "list_map.scilexp";
  "list_mem.scilexp";
  "list_nth.scilexp";
  "list_product.scilexp";
  "list_reverse.scilexp";
  "list_sort.scilexp";
  "list_sort_eq.scilexp";
  "list_tail1.scilexp";
  "list_tail2.scilexp";  
  "list_tail.scilexp";
  "list_unzip.scilexp";
  "list_zip.scilexp";
  "list_zip_with.scilexp"; 
  "list_foldl_while.scilexp";
  "list_foldl.scilexp";
  "list_foldr.scilexp";
  "map1.scilexp";
  "map2.scilexp";
  "map3.scilexp";
  "map4.scilexp";
  "map5.scilexp";
  "map6.scilexp";
  "map_remove_no_exception.scilexp";
  "map_no_keeping_old_bindings.scilexp";
  "map_to_list.scilexp";
  "msg.scilexp";
  "nat_eq_false.scilexp";
  "nat_eq_foldl.scilexp";
  "nat_to_int.scilexp";
  "nat_fold_stress.scilexp";
  "option.scilexp";
  "pair1.scilexp";
  "pair2.scilexp";
  "pair3.scilexp";
  "pm1.scilexp";  
  "pm2.scilexp";
  "pm3.scilexp";
  "pm_app.scilexp";
  "pm_nesting.scilexp";
  "string1.scilexp";
  "string2.scilexp";
  "times_two.scilexp";
  "to_bystr.scilexp";
  "type_subst1.scilexp";
  "type_subst2.scilexp";
  "uint_conversions.scilexp";
  "let_in_let_in.scilexp";
  "builtin-schnorr_get_address.scilexp";
]

module Tests = TestUtil.DiffBasedTests(
  struct
    let gold_path dir f = [dir; "eval"; "exp"; "good"; "gold"; f ^ ".gold" ]
    let test_path f = ["eval"; "exp"; "good"; f]
    let runner = "eval-runner"
    let custom_args = []
    let additional_libdirs = []
    let tests = explist
    let exit_code : Unix.process_status = WEXITED 0

  end)

