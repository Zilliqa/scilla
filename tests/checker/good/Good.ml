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

module Tests = Scilla_test.Util.DiffBasedTests (struct
  let gold_path dir f = [ dir; "checker"; "good"; "gold"; f ^ ".gold" ]
  let test_path f = [ "contracts"; f ]
  let runner = "scilla-checker"
  let ignore_predef_args = false
  let json_errors = true
  let gas_limit = Stdint.Uint64.of_int 8000
  let custom_args = [ "-cf"; "-contractinfo" ]
  let additional_libdirs = []
  let provide_init_arg = false
  let diff_filter s = s

  let tests =
    [
      "auction.scilla";
      "bookstore.scilla";
      "cfinvoke.scilla";
      "chain-call-balance-1.scilla";
      "chain-call-balance-2.scilla";
      "chain-call-balance-3.scilla";
      "constraint.scilla";
      "crowdfunding.scilla";
      "crowdfunding_proc.scilla";
      "earmarked-coin.scilla";
      "ecdsa.scilla";
      "empty.scilla";
      "fungible-token.scilla";
      "helloWorld.scilla";
      "inplace-map.scilla";
      "map_key_test.scilla";
      "mappair.scilla";
      "multiple-msgs.scilla";
      "nonfungible-token.scilla";
      "one-msg1.scilla";
      "one-msg.scilla";
      "ping.scilla";
      "pong.scilla";
      "schnorr.scilla";
      "simple-dex.scilla";
      "wallet.scilla";
      "wallet_2.scilla";
      "zil-game.scilla";
      "map_corners_test.scilla";
      "ud-registry.scilla";
      "ud-proxy.scilla";
      "listiter.scilla";
      "UintParam.scilla";
      "map_as_cparam.scilla";
      "address_list_as_cparam.scilla";
      "shadow_import.scilla";
      "remote_state_reads.scilla";
      "remote_state_reads_2.scilla";
      "remote_state_reads_cparam.scilla";
      "address_eq_test.scilla";
      "address_list_traversal.scilla";
      "polymorphic_address.scilla";
      "dead_code_test1.scilla";
      "dead_code_test2.scilla";
      "dead_code_test3.scilla";
      "dead_code_test4.scilla";
      "dead_code_test5.scilla";
      "dead_code_test6.scilla";
      "dead_code_test7.scilla";
      "dead_code_test8.scilla";
      "dead_code_test9.scilla";
      "dead_code_test10.scilla";
      "dead_code_test11.scilla";
      "dead_code_test12.scilla";
      "dead_code_test13.scilla";
      "dead_code_test14.scilla";
      "unbox_result1.scilla";
      "unbox_result2.scilla";
      "unbox_result3.scilla";
      "unbox_result4.scilla";
      "unbox_result5.scilla";
      "dead_code_test15.scilla";
      "dead_code_test16.scilla";
      "dead_code_test17.scilla";
      "dead_code_test18.scilla";
      "simple-dex-remote-reads.scilla";
      "type_casts.scilla";
      "timestamp.scilla";
      "codehash.scilla";
      "chainid.scilla";
      "ark.scilla";
    ]

  let exit_code : UnixLabels.process_status = WEXITED 0
end)

(* These tests differ from the regular contract tests by having an init file and additonal libdirs. *)
module TestsWithInit = Scilla_test.Util.DiffBasedTests (struct
  let gold_path dir f = [ dir; "checker"; "good"; "gold"; f ^ ".gold" ]
  let test_path f = [ "contracts"; f ]
  let runner = "scilla-checker"
  let ignore_predef_args = false
  let json_errors = true
  let gas_limit = Stdint.Uint64.of_int 8000
  let custom_args = [ "-cf"; "-contractinfo" ]
  let additional_libdirs = [ [ "contracts" ] ]
  let provide_init_arg = true
  let diff_filter s = s

  let tests =
    [
      "import-test-lib.scilla";
      "import-test-lib2.scilla";
      "import-test-lib3.scilla";
    ]

  let exit_code : UnixLabels.process_status = WEXITED 0
end)

(* These differ from "Tests" because of an additional libdir argument. *)
module CheckerTests = Scilla_test.Util.DiffBasedTests (struct
  let gold_path dir f = [ dir; "checker"; "good"; "gold"; f ^ ".gold" ]
  let test_path f = [ "checker"; "good"; f ]
  let runner = "scilla-checker"
  let ignore_predef_args = false
  let json_errors = true
  let gas_limit = Stdint.Uint64.of_int 8000
  let custom_args = [ "-cf"; "-contractinfo" ]
  let additional_libdirs = [ [ "checker"; "good"; "lib" ] ]
  let provide_init_arg = false
  let diff_filter s = s

  let tests =
    [
      "adt_test.scilla";
      "nested-comments.scilla";
      "cashflow_test.scilla";
      "missing-accepts.scilla";
      "multiple-accepts.scilla";
      "one-accept.scilla";
      "one-transition-accepts.scilla";
      "one-transition-might-accept.scilla";
      "no-accept-when-sending-zeros1.scilla";
      "no-accept-when-sending-zeros2.scilla";
      "one-msg2.scilla";
      "one-msg3.scilla";
      "libchain1.scilla";
      "libchain2.scilla";
      "libdiamond.scilla";
      "InstantiatedListUtils.scillib";
      "map_no_inplace_warn.scilla";
      "shadowwarn1.scilla";
      "shadowwarn2.scilla";
      "shadowwarn3.scilla";
      "simple-dex-shadowwarn.scilla";
      "namespace1.scilla";
      "namespace2.scilla";
      "namespace3.scilla";
      "TestLibNS1.scillib";
      "TestLibNS2.scillib";
      "TestLibNS3.scillib";
      "TestLibNS4.scillib";
      "libdiamond2.scilla";
      "map-inplace-update-with-_sender.scilla";
      "backward_definition_procedure.scilla";
      "lib_typing.scilla";
      "lib_typing2.scilla";
      "constraint_scope.scilla";
      "event_reordered_fields.scilla";
      "blowup_1.scilla";
      "blowup_2.scilla";
      "builtin_type_args.scilla";
    ]

  let exit_code : UnixLabels.process_status = WEXITED 0
end)

(* The test here require the `-init` argument. This is required for
 * importing libraries whose addresses are specified in the init JSON *)
module InitArgTests = Scilla_test.Util.DiffBasedTests (struct
  let gold_path dir f = [ dir; "checker"; "good"; "gold"; f ^ ".gold" ]
  let test_path f = [ "checker"; "good"; f ]
  let runner = "scilla-checker"
  let ignore_predef_args = false
  let json_errors = true
  let gas_limit = Stdint.Uint64.of_int 8000
  let custom_args = []
  let provide_init_arg = true
  let diff_filter s = s
  let additional_libdirs = [ [ "checker"; "good"; "lib" ] ]
  let tests = [ "blockchain_import.scilla" ]
  let exit_code : UnixLabels.process_status = WEXITED 0
end)

module ShogiTests = Scilla_test.Util.DiffBasedTests (struct
  let gold_path dir f = [ dir; "checker"; "good"; "gold"; f ^ ".gold" ]
  let test_path f = [ "contracts"; f ]
  let runner = "scilla-checker"
  let ignore_predef_args = false
  let json_errors = true
  let gas_limit = Stdint.Uint64.of_int 8000
  let custom_args = [ "-cf"; "-contractinfo" ]
  let additional_libdirs = [ [ "contracts"; "shogi_lib" ] ]
  let provide_init_arg = false
  let diff_filter s = s
  let tests = [ "shogi.scilla"; "shogi_proc.scilla" ]
  let exit_code : UnixLabels.process_status = WEXITED 0
end)

(* We don't add the "-typeinfo" argument to the main set of "Tests"
 * because that adds a lot of diff noise when things change. *)
module TypeInfoTests = Scilla_test.Util.DiffBasedTests (struct
  let gold_path dir f = [ dir; "checker"; "good"; "gold"; f ^ ".typeinfo.gold" ]
  let test_path f = [ "contracts"; f ]
  let runner = "scilla-checker"
  let ignore_predef_args = false
  let json_errors = true
  let gas_limit = Stdint.Uint64.of_int 8000
  let custom_args = [ "-cf"; "-typeinfo" ]
  let additional_libdirs = []
  let provide_init_arg = false
  let diff_filter s = s
  let tests = [ "map_corners_test.scilla"; "auction.scilla" ]
  let exit_code : UnixLabels.process_status = WEXITED 0
end)

module CallgraphTests = Scilla_test.Util.DiffBasedTests (struct
  let gold_path dir f = [ dir; "checker"; "good"; "gold"; f ^ ".gold" ]
  let test_path f = [ "contracts"; f ]
  let runner = "scilla-checker"
  let ignore_predef_args = false
  let json_errors = false
  let gas_limit = Stdint.Uint64.of_int 8000
  let custom_args = [ "-dump-callgraph-stdout" ]
  let additional_libdirs = []
  let provide_init_arg = false
  let diff_filter s = s
  let tests = [ "callgraph1.scilla"; "callgraph2.scilla" ]
  let exit_code : UnixLabels.process_status = WEXITED 0
end)
