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


let print_args args =
  List.iter ~f:(Printf.printf "%s ") args; Printf.printf "\n"

let testsuit_gas_limit = "8000"

let succ_code : Caml.Unix.process_status = WEXITED 0
let fail_code : Caml.Unix.process_status = WEXITED 1

(*
 * Build tests to invoke scilla-runner with the right arguments, for
 * multiple test cases, each suffixed with _i up to _n (both inclusive)
 *)
let rec build_contract_tests env name ecode i n add_additional_lib =
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
        let output_file = tmpdir ^/ name ^ "_output_" ^ (Int.to_string  i) ^. "json" in
        let args_tmp =
              ["-init"; dir ^/ "init.json";
               "-i"; contract_dir ^/ name ^. "scilla";
              (* stdlib is in src/stdlib *)
              "-libdir"; "src" ^/ "stdlib";
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
        (* Should this test "-disable-validate-json"? *)
        let args =
          if disable_validate_json then "-disable-validate-json" :: args' else args'
        in
        (if (env.print_cli test_ctxt) then (Printf.printf "\nUsing CLI: %s " "scilla-runner"; print_args args));
        let scillabin = env.bin_dir test_ctxt ^/ "scilla-runner" in
           (* Ensure that the executable exists with ecode *)
           let goldoutput_file = dir ^/ "output_" ^ istr ^. "json" in
           let update_gold = env.update_gold test_ctxt in
           (* Expected success tests *)
           if ecode = succ_code then
             (assert_command ~exit_code:ecode ~ctxt:test_ctxt ~use_stderr:true scillabin args;
             let o = In_channel.read_all output_file in
             let g = In_channel.read_all goldoutput_file in
             if update_gold then
               (Printf.printf "Updating gold output for test %s\n" (name ^ "_" ^ istr);
                Out_channel.write_all goldoutput_file ~data:o);
             (* Compare output.json with a gold output in the contract directory *)
             assert_equal ~cmp:(fun e o -> (String.strip e) = (String.strip o)) ~ctxt:test_ctxt
               ~msg:(Core.sprintf "Output json mismatch\nActual:\n%s\nExpected:\n%s" o g) g o)
           else (* Expected failure tests *)
             let output_verifier s =
                let output = BatStream.to_string s in
                let gold_output = In_channel.read_all goldoutput_file in
                  assert_equal ~cmp:(fun e o -> (String.strip e) = (String.strip o))
                   ~printer:(fun s -> s) gold_output output;
              in
              let output_updater s =
                let output = BatStream.to_string s in
                (Out_channel.write_all goldoutput_file ~data:output;
                 Printf.printf "Updated gold output for test %s\n" (name^"_"^(Int.to_string i)))
              in
              if update_gold then
                assert_command ~exit_code:ecode ~foutput:output_updater ~ctxt:test_ctxt ~use_stderr:true scillabin args
              else
                assert_command ~exit_code:ecode ~foutput:output_verifier ~ctxt:test_ctxt ~use_stderr:true scillabin args
          )
      in
      (* If this test is expected to succeed, we know that the JSONs are all "good".
       * So test both the JSON parsers, one that does validation, one that doesn't. 
       * Both should succeed. *)
      if ecode = succ_code
      then
        (test true) :: (test false) :: (build_contract_tests env name ecode (i+1) n add_additional_lib)
      else
        (test false) :: (build_contract_tests env name ecode (i+1) n add_additional_lib)

let build_contract_init_test env name =
  name ^ "_init" >::
  (fun test_ctxt ->
    (* Files for the contract are in contract/(crowdfunding|zil-game|etc). *)
    let contract_dir = env.tests_dir test_ctxt ^/ "contracts" in
    let dir = env.tests_dir test_ctxt ^/ "runner" ^/ name in
    let tmpdir = bracket_tmpdir test_ctxt in
    let output_file = tmpdir ^/ name ^ "_init_output" ^. "json" in
    let args = ["-init"; dir ^/ "init.json";
                (* stdlib is in src/stdlib *)
                "-libdir"; "src" ^/ "stdlib";
                "-i"; contract_dir ^/ name ^. "scilla";
                "-o"; output_file;
                "-jsonerrors";
                "-gaslimit"; testsuit_gas_limit;
                "-iblockchain"; dir ^/ "blockchain_1.json";]
    in
    if env.print_cli test_ctxt then (Printf.printf "\nUsing CLI: %s " "scilla-runner"; print_args args);
    let scillabin = env.bin_dir test_ctxt ^/ "scilla-runner" in
    (* Ensure that the executable exists with 0 *)
    assert_command ~ctxt:test_ctxt scillabin args;
    let goldoutput_file = dir ^/ "init_output.json" in
    let g = In_channel.read_all goldoutput_file in
    let o = In_channel.read_all output_file in
    let update_gold = env.update_gold test_ctxt in
    if update_gold then begin
      Printf.printf "Updating gold output for test %s\n" (name ^ "_init");
      Out_channel.write_all goldoutput_file ~data:o;
      (* Compare output.json with a gold output in the contract directory *)
      assert_equal ~ctxt:test_ctxt ~msg:(Core.sprintf "Output json mismatch\nActual:\n%s\nExpected:\n%s" o g)
            ~cmp:(fun e o -> (String.strip e) = (String.strip o)) g o
    end)

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
        if env.print_cli test_ctxt then (Printf.printf "\nUsing CLI: %s " "scilla-runner"; print_args args);
        assert_command ~exit_code:fail_code ~ctxt:test_ctxt (scillabin env.bin_dir test_ctxt) args
      ) in
  [test 1;test 2;test 3]

let add_tests env =
  "contract_tests" >:::[
    "these_tests_must_SUCCEED" >:::[
      "crowdfunding" >:::(build_contract_tests env "crowdfunding" succ_code 1 6 false);
      "crowdfunding_init" >:(build_contract_init_test env "crowdfunding");
      "zil-game" >:::(build_contract_tests env "zil-game" succ_code 1 9 false);
      "zil-game_init" >:(build_contract_init_test env "zil-game");
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

