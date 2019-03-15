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

let stream_to_string (s : char Stream.t) =
  let string_of_chars chars =
    let buf = Buffer.create 16 in
    List.iter ~f:(Buffer.add_char buf) chars;
    Buffer.contents buf
  in
  let result = ref [] in
    Stream.iter (fun value -> result := value :: !result) s;
  let l = List.rev !result in
    string_of_chars l


let succ_code : Caml.Unix.process_status = WEXITED 0
let fail_code : Caml.Unix.process_status = WEXITED 1

(*
 * Build tests to invoke scilla-runner with the right arguments, for
 * multiple test cases, each suffixed with _i up to _n (both inclusive)
 *)
let rec build_contract_tests env name exit_code i n add_additional_lib =
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
          if add_additional_lib
          then ["-libdir"; contract_dir ^/ name ^ "_lib"] @ args_tmp
          else args_tmp
        in
        let args =
          if disable_validate_json then "-disable-validate-json" :: args' else args'
        in
        let scillabin = env.bin_dir test_ctxt ^/ "scilla-runner" in
        print_cli_usage (env.print_cli test_ctxt) scillabin args;
        let goldoutput_file = dir ^/ "output_" ^ istr ^. "json" in
        let out = 
          if exit_code = succ_code
          then
            let _ = assert_command ~exit_code ~use_stderr:true ~ctxt:test_ctxt scillabin args in
            In_channel.read_all output_file
          else
            let outref = ref "" in
            let f = (fun outstream -> outref := stream_to_string outstream) in
            let _ = assert_command ~foutput:f ~exit_code ~use_stderr:true ~ctxt:test_ctxt scillabin args in
            !outref
        in
        if env.update_gold test_ctxt
        then output_updater goldoutput_file (name ^ "_" ^ istr) out
        else output_verifier goldoutput_file (env.print_diff test_ctxt) out
      ) in
      (* If this test is expected to succeed, we know that the JSONs are all "good".
       * So test both the JSON parsers, one that does validation, one that doesn't.
       * Both should succeed. *)
      if exit_code = succ_code
      then
        (test true) :: (test false) :: (build_contract_tests env name exit_code (i+1) n add_additional_lib)
      else
        (test false) :: (build_contract_tests env name exit_code (i+1) n add_additional_lib)

let build_contract_init_test env exit_code name is_library =
  name ^ "_init" >::
  (fun test_ctxt ->
    (* Files for the contract are in contract/(crowdfunding|zil-game|etc). *)
    let contract_dir = env.tests_dir test_ctxt ^/ "contracts" in
    let dir = env.tests_dir test_ctxt ^/ "runner" ^/ name in
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
    let out =
      if exit_code = succ_code
      then
        let _ = assert_command ~exit_code ~use_stderr:true ~ctxt:test_ctxt scillabin args in
        In_channel.read_all output_file
      else
        let outref = ref "" in
        let f = (fun outstream -> outref := stream_to_string outstream) in
        let _ = assert_command ~foutput:f ~exit_code ~use_stderr:true ~ctxt:test_ctxt scillabin args in
        !outref
    in
    let goldoutput_file = dir ^/ "init_output.json" in
    if env.update_gold test_ctxt
    then output_updater goldoutput_file (name ^ "_init") out
    else output_verifier goldoutput_file (env.print_diff test_ctxt) out)

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
      "crowdfunding" >:::(build_contract_tests env "crowdfunding" succ_code 1 6 false);
      "crowdfunding_init" >:(build_contract_init_test env succ_code "crowdfunding" false);
      "zil-game" >:::(build_contract_tests env "zil-game" succ_code 1 9 false);
      "zil-game_init" >:(build_contract_init_test env succ_code "zil-game" false);
      "testlib1_init" >:(build_contract_init_test env fail_code"0x565556789012345678901234567890123456abcd" true);
      "testlib2_init" >:(build_contract_init_test env succ_code "TestLib2" true);
      "cfinvoke" >:::(build_contract_tests env "cfinvoke" succ_code 1 4 false);
      "ping" >:::(build_contract_tests env "ping" succ_code 0 3 false);
      "pong" >:::(build_contract_tests env "pong" succ_code 0 3 false);
      "helloWorld" >:::(build_contract_tests env "helloWorld" succ_code 1 3 false);
      "auction" >:::(build_contract_tests env "auction" succ_code 1 8 false);
      "mappair" >:::(build_contract_tests env "mappair" succ_code 1 7 false);
      "bookstore" >:::(build_contract_tests env "bookstore" succ_code 1 10 false);
      "nonfungible-token" >:::(build_contract_tests env "nonfungible-token" succ_code 1 12 false);
      "nonfungible-token" >:::(build_contract_tests env "nonfungible-token" succ_code 21 27 false);
      "schnorr" >:::(build_contract_tests env "schnorr" succ_code 1 3 false);
      "ecdsa" >:::(build_contract_tests env "ecdsa" succ_code 1 3 false);
      "empty_contract" >::: (build_contract_tests env "empty" succ_code 1 1 false);
      "fungible-token" >:::(build_contract_tests env "fungible-token" succ_code 0 8 false);
      "inplace-map" >:::(build_contract_tests env "inplace-map" succ_code 1 14 false);
      "wallet" >:::(build_contract_tests env "wallet" succ_code 1 11 false);
      "one_msg_test" >::: (build_contract_tests env "one-msg" succ_code 1 1 false);
      "one_msg1_test" >::: (build_contract_tests env "one-msg1" succ_code 1 1 false);
      "simple-dex" >:::(build_contract_tests env "simple-dex" succ_code 1 8 false);
      "shogi" >::: (build_contract_tests env "shogi" succ_code 1 4 true);
      "map_key_test" >::: (build_contract_tests env "map_key_test" succ_code 1 1 false);
    ];
    "these_tests_must_FAIL" >:::[
      "helloWorld_f" >:::(build_contract_tests env "helloWorld" fail_code 4 9 false);
      "mappair" >:::(build_contract_tests env "mappair" fail_code 8 8 false);
      "mappair" >:::(build_contract_tests env "mappair" fail_code 12 14 false);
      "multiple_msgs_test" >::: (build_contract_tests env "multiple-msgs" fail_code 1 1 true);
    ];
    "misc_tests" >::: build_misc_tests env;
  ]

