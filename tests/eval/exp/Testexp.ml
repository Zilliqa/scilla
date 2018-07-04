(*
 * Copyright (c) 2018 - present Zilliqa, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)
let explist = [
  "addr.scilla"; "app5.scilla"; "builtin1.scilla"; "cons.scilla";
  "hash3.scilla"; "let-error.scilla"; "map3.scilla"; "msg_error2.scilla";
  "option.scilla"; "pm1.scilla"; "app2.scilla"; "app_error1.scilla";
  "builtin2.scilla"; "hash4.scilla"; "let.scilla"; "map4.scilla";
  "msg_error3.scilla"; "pair1.scilla"; "pm2.scilla";
  "app3.scilla"; "app_error2.scilla"; "builtin3.scilla"; "hash1.scilla";
  "map1.scilla"; "map5.scilla"; "msg_error.scilla"; "pair2.scilla";
  "pm3.scilla"; "app4.scilla"; "app.scilla"; "builtin_error1.scilla";
  "hash2.scilla"; "let-builtin.scilla"; "map2.scilla"; "map6.scilla";
  "msg.scilla"; "pair3.scilla"; "pm_app.scilla"; "pm_nesting.scilla";
  "string1.scilla"; "string2.scilla"; "string_error1.scilla";
  "nat_eq_foldl.scilla"; "nat_eq_false.scilla"; "times_two.scilla";
  "fib.scilla"; "id.scilla"; "hof2.scilla"; "hof3.scilla"; "hof.scilla";
  "list_map.scilla"; "list_product.scilla"; "builtin-strings.scilla";
  "list_filter.scilla"; "list_head.scilla"; "list_tail.scilla";
  "list_tail1.scilla"; "list_tail2.scilla"; "list_append.scilla";
  "list_reverse.scilla"; "list_flatten.scilla"; "list_length.scilla";
  "list_eq.scilla"; "list_mem.scilla"; "list_forall.scilla";
  "list_exists.scilla"; "list_sort.scilla"; "list_find.scilla";
  "list_zip.scilla"; "list_zip_with.scilla"; "list_unzip.scilla";
  "nat_to_int.scilla"; "list_to_map.scilla"; "list_sort_eq.scilla";
  "list_nth.scilla"; "builtin-overflow1.scilla"; "builtin-overflow2.scilla";
  "builtin-overflow3.scilla"; "builtin-overflow4.scilla"; "builtin-overflow5.scilla";
  "builtin-overflow6.scilla"; "builtin-overflow7.scilla"; "builtin-overflow8.scilla";
  "builtin-overflow9.scilla"; "builtin4.scilla";
  "type_subst1.scilla"; "type_subst2.scilla";
  "int_to_nat.scilla";
]

module Tests = TestUtil.DiffBasedTests(
  struct
    let gold_path dir f = [dir; "eval"; "exp"; "gold"; f ^ ".gold" ]
    let test_path f = ["eval"; "exp"; f]
    let runner = "eval-runner"      
    let tests = explist

  end)

