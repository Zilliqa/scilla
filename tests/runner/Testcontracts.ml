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
open OUnit2
open ScillaUtil.FilePathInfix
open TestUtil

let testsuit_gas_limit = "8000"

let succ_code : Caml.Unix.process_status = WEXITED 0
let fail_code : Caml.Unix.process_status = WEXITED 1

(*
 * Build tests to invoke scilla-runner with the right arguments, for
 * multiple test cases, each suffixed with _i up to _n (both inclusive)
 *)
let rec build_contract_tests env name exit_code i n additional_libs =
  if i > n
    then []
  else
    (* Create a contract test with an option to disable JSON validation (fast parsing). *)
    let test disable_validate_json =
      let istr = Int.to_string i in
      let testname = name ^ "_" ^ istr ^
        (if disable_validate_json then "_disable_validate_json" else "") in
      testname >::
      (* function to run scilla-runner and check exit code *)
      (fun test_ctxt ->
        let tests_dir = FilePath.make_relative (Sys.getcwd ()) (env.tests_dir test_ctxt) in
        let contract_dir = tests_dir ^/ "contracts" in
        let dir = tests_dir ^/ "runner" ^/ name in
        let tmpdir = bracket_tmpdir test_ctxt in
        let output_file = tmpdir ^/ name ^ "_output_" ^ istr ^. "json" in
        let args_tmp =
              ["-init"; dir ^/ "init.json";
               "-i"; contract_dir ^/ name ^. "scilla";
               (* stdlib is in src/stdlib *)
               "-libdir"; env.stdlib_dir test_ctxt;
              "-o"; output_file;
              "-gaslimit"; testsuit_gas_limit;
              "-imessage"; dir ^/ "message_" ^ istr ^. "json";
              "-istate" ; dir ^/ "state_" ^ istr ^. "json";
              "-jsonerrors";
              "-iblockchain" ; dir ^/ "blockchain_" ^ istr ^. "json"] in
        let args' =
          List.fold_right additional_libs ~init:args_tmp
            ~f:(fun lib_name cur_args ->
                "-libdir" :: (contract_dir ^/ lib_name) :: cur_args)
        in
        let args =
          if disable_validate_json then "-disable-validate-json" :: args' else args'
        in
        let scillabin = env.bin_dir test_ctxt ^/ "scilla-runner" in
        print_cli_usage (env.print_cli test_ctxt) scillabin args;
        let test_name = name ^ "_" ^ istr in
        let goldoutput_file = dir ^/ "output_" ^ istr ^. "json" in
        let msg = cli_usage_on_err scillabin args in
        assert_command ~exit_code ~use_stderr:true ~ctxt:test_ctxt scillabin args
          ~foutput:(fun s ->
              (* if the test is supposed to succeed we read the output from a file,
                 otherwise we read from the output stream *)
              let out =
                if exit_code = succ_code
                then In_channel.read_all output_file
                else BatStream.to_string s
              in
              if env.update_gold test_ctxt
              then output_updater goldoutput_file test_name out
              else output_verifier goldoutput_file msg (env.print_diff test_ctxt) out))
      in
      (* If this test is expected to succeed, we know that the JSONs are all "good".
       * So test both the JSON parsers, one that does validation, one that doesn't.
       * Both should succeed. *)
      if exit_code = succ_code
      then
        (test true) :: (test false) :: (build_contract_tests env name exit_code (i+1) n additional_libs)
      else
        (test false) :: (build_contract_tests env name exit_code (i+1) n additional_libs)

let build_contract_init_test env exit_code name is_library =
  name ^ "_init" >::
  (fun test_ctxt ->
    let tests_dir = FilePath.make_relative (Sys.getcwd ()) (env.tests_dir test_ctxt) in
    (* Files for the contract are in contract/(crowdfunding|zil-game|etc). *)
    let contract_dir = tests_dir ^/ "contracts" in
    let dir = tests_dir ^/ "runner" ^/ name in
    let extn =
      if is_library then GlobalConfig.StdlibTracker.file_extn_library
      else GlobalConfig.StdlibTracker.file_extn_contract in
    let tmpdir = bracket_tmpdir test_ctxt in
    let output_file = tmpdir ^/ name ^ "_init_output" ^. "json" in
    let args = ["-init"; dir ^/ "init.json";
                (* stdlib is in src/stdlib *)
                "-libdir"; "src" ^/ "stdlib";
                "-i"; contract_dir ^/ name ^. extn;
                "-o"; output_file;
                "-jsonerrors";
                "-gaslimit"; testsuit_gas_limit;
                "-iblockchain"; dir ^/ "blockchain_1.json";]
    in
    let scillabin = env.bin_dir test_ctxt ^/ "scilla-runner" in
    print_cli_usage (env.print_cli test_ctxt) scillabin args;
    let test_name = name ^ "_init" in
    let goldoutput_file = dir ^/ "init_output.json" in
    let msg = cli_usage_on_err scillabin args in
    assert_command ~exit_code ~use_stderr:true ~ctxt:test_ctxt scillabin args
    ~foutput:(fun s ->
        (* if the test is supposed to succeed we read the output from a file,
           otherwise we read from the output stream *)
        let out =
          if exit_code = succ_code
          then In_channel.read_all output_file
          else BatStream.to_string s
        in
        if env.update_gold test_ctxt
        then output_updater goldoutput_file test_name out
        else output_verifier goldoutput_file msg (env.print_diff test_ctxt) out))

let build_misc_tests env =
  let scillabin bin_dir test_ctxt =
    bin_dir test_ctxt ^/ "scilla-runner" in
  let output_file test_ctxt name =
    bracket_tmpdir test_ctxt ^/ name in
  let tests_dir_file testsdir test_ctxt name =
    testsdir test_ctxt ^/ "runner" ^/ "crowdfunding" ^/ name in
  let contracts_dir_file testsdir test_ctxt =
    testsdir test_ctxt ^/ "contracts" ^/ "crowdfunding.scilla" in
  (* Tests for exit 1 on bad json *)
  let test num =
    let snum = Int.to_string num in
    "misc_test_badjson_" ^ snum >::
      (fun test_ctxt ->
        let args = ["-init"; tests_dir_file env.tests_dir test_ctxt ("init_bad" ^ snum ^. "json");
                    "-libdir"; "src" ^/ "stdlib";
                    "-jsonerrors";
                    "-i"; contracts_dir_file env.tests_dir test_ctxt;
                    "-o"; output_file test_ctxt "init_bad" ^ snum ^ "_output" ^. "json";
                    "-iblockchain"; tests_dir_file env.tests_dir test_ctxt ("blockchain_" ^ snum ^. "json")]
        in
        let scillabin = scillabin env.bin_dir test_ctxt in
        print_cli_usage (env.print_cli test_ctxt) scillabin args;
        assert_command ~exit_code:fail_code ~ctxt:test_ctxt scillabin args
      ) in
  [test 1;test 2;test 3]

let add_tests env =
  "contract_tests" >:::[
    "these_tests_must_SUCCEED" >:::[
      "crowdfunding" >:::(build_contract_tests env "crowdfunding" succ_code 1 6 []);
      "crowdfunding_init" >:(build_contract_init_test env succ_code "crowdfunding" false);
      "crowdfunding_proc" >:::(build_contract_tests env "crowdfunding_proc" succ_code 1 6 []);
      "zil-game" >:::(build_contract_tests env "zil-game" succ_code 1 9 []);
      "zil-game_init" >:(build_contract_init_test env succ_code "zil-game" false);
      "creationtest_init" >:(build_contract_init_test env succ_code "creationtest" false);
      "testlib2_init" >:(build_contract_init_test env succ_code "TestLib2" true);
      "cfinvoke" >:::(build_contract_tests env "cfinvoke" succ_code 1 4 []);
      "ping" >:::(build_contract_tests env "ping" succ_code 0 3 []);
      "pong" >:::(build_contract_tests env "pong" succ_code 0 3 []);
      "helloWorld" >:::(build_contract_tests env "helloWorld" succ_code 1 4 []);
      "auction" >:::(build_contract_tests env "auction" succ_code 1 8 []);
      "mappair" >:::(build_contract_tests env "mappair" succ_code 1 7 []);
      "bookstore" >:::(build_contract_tests env "bookstore" succ_code 1 12 []);
      "nonfungible-token" >:::(build_contract_tests env "nonfungible-token" succ_code 1 12 []);
      "nonfungible-token" >:::(build_contract_tests env "nonfungible-token" succ_code 21 27 []);
      "schnorr" >:::(build_contract_tests env "schnorr" succ_code 1 3 []);
      "ecdsa" >:::(build_contract_tests env "ecdsa" succ_code 1 4 []);
      "empty_contract" >::: (build_contract_tests env "empty" succ_code 1 1 []);
      "fungible-token" >:::(build_contract_tests env "fungible-token" succ_code 0 8 []);
      "inplace-map" >:::(build_contract_tests env "inplace-map" succ_code 1 14 []);
      "wallet" >:::(build_contract_tests env "wallet" succ_code 1 11 []);
      "one_msg_test" >::: (build_contract_tests env "one-msg" succ_code 1 1 []);
      "one_msg1_test" >::: (build_contract_tests env "one-msg1" succ_code 1 1 []);
      "simple-dex" >:::(build_contract_tests env "simple-dex" succ_code 1 8 []);
      "shogi" >::: (build_contract_tests env "shogi" succ_code 1 4 ["shogi_lib"]);
      "shogi_proc" >::: (build_contract_tests env "shogi_proc" succ_code 1 4 ["shogi_lib"]);
      "map_key_test" >::: (build_contract_tests env "map_key_test" succ_code 1 1 []);
      "earmarked-coin" >:::(build_contract_tests env "earmarked-coin" succ_code 1 6 []);
    ];
    "these_tests_must_FAIL" >:::[
      "helloWorld_f" >:::(build_contract_tests env "helloWorld" fail_code 5 11 []);
      "mappair" >:::(build_contract_tests env "mappair" fail_code 8 8 []);
      "mappair" >:::(build_contract_tests env "mappair" fail_code 12 14 []);
      "multiple_msgs_test" >::: (build_contract_tests env "multiple-msgs" fail_code 1 1 []);
      "exception-example" >::: (build_contract_tests env "exception-example" fail_code 1 2 []);
      "testlib1_init" >:(build_contract_init_test env fail_code "0x565556789012345678901234567890123456abcd" true);
    ];
    "misc_tests" >::: build_misc_tests env;
  ]

