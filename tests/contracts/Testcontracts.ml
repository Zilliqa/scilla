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


open OUnit2
open TestUtil

let rec print_args args =
  match args with
  | [] -> Printf.printf "\n"
  | a :: b ->
    (Printf.printf "%s " a;
    print_args b)

let sep = Filename.dir_sep

let testsuit_gas_limit = 8000

(* 
 * Build tests to invoke scilla-runner with the right arguments, for
 * multiple test cases, each suffixed with _i up to _n (both inclusive)
 *)
let rec build_contract_tests env name ecode i n add_additional_lib =
  if (i > n) 
    then [] 
  else
    (* Create a contract test with an option to disable JSON validation (fast parsing). *)
    let test disable_validate_json = 
      let testname = name ^ "_" ^ (i_to_s  i) ^ 
        (if disable_validate_json then "_disable_validate_json" else "") in
      testname >::
      (* function to run scilla-runner and check exit code *)
      (fun test_ctxt ->
        (* Files for the contract are in examples/contract/(crowdfunding|zil-game|etc). *)
        let tests_dir = FilePath.make_relative (Sys.getcwd()) (env.tests_dir test_ctxt) in
        let dir = tests_dir ^ sep ^ "contracts" ^ sep ^
          name ^ sep in
        let tmpdir = bracket_tmpdir test_ctxt in 
        let output_file = tmpdir ^ sep ^ name ^ "_output_"
                    ^ (i_to_s  i) ^ ".json" in
        let args_tmp =
              ["-init"; dir ^ "init.json"; 
              "-i"; dir ^ "contract.scilla";
              (* stdlib is in src/stdlib *)
              "-libdir"; "src" ^ sep ^ "stdlib";
              "-o"; output_file;
              "-gaslimit"; Core.Int.to_string testsuit_gas_limit;
              "-imessage"; dir ^ "message_" ^ (i_to_s i) ^ ".json";
              "-istate" ; dir ^ "state_" ^ (i_to_s i) ^ ".json";
              "-jsonerrors";
              "-iblockchain" ; dir ^ "blockchain_" ^ (i_to_s i) ^ ".json"] in
        let args' =
          if add_additional_lib
          then ["-libdir"; dir ^ "lib" ^ sep ] @ args_tmp
          else args_tmp
        in
        (* Should this test "-disable-validate-json"? *)
        let args =
          if disable_validate_json then "-disable-validate-json" :: args' else args'
        in
        (if (env.print_cli test_ctxt) then (Printf.printf "\nUsing CLI: %s " "scilla-runner"; print_args args));
        let scillabin = env.bin_dir test_ctxt ^ sep ^ "scilla-runner" in
           (* Ensure that the executable exists with ecode *)
           let goldoutput_file = dir ^ "output_" ^ (i_to_s i) ^ ".json" in
           let update_gold = env.update_gold test_ctxt in
           let exit_succ : Unix.process_status = WEXITED 0 in
           (* Expected success tests *)
           if ecode = exit_succ then
             (assert_command ~exit_code:ecode ~ctxt:test_ctxt ~use_stderr:true scillabin args;
             let o = load_file output_file in
             let g = load_file goldoutput_file in
             if update_gold then
              (Printf.printf "Updating gold output for test %s\n" (name^"_"^(i_to_s i));
                save_to_file o goldoutput_file);
             (* Compare output.json with a gold output in the contract directory *)
             assert_equal ~cmp:(fun e o -> (String.trim e) = (String.trim o)) ~ctxt:test_ctxt
               ~msg:(Core.sprintf "Output json mismatch\nActual:\n%s\nExpected:\n%s" o g) g o)
           else (* Expected failure tests *)
             (let output_verifier s =
                let output = stream_to_string s in
                let gold_output = load_file goldoutput_file in
                  assert_equal ~cmp:(fun e o -> (String.trim e) = (String.trim o))
                   ~printer:(fun s -> s) gold_output output;
              in
              let output_updater s =
                let output = stream_to_string s in
                (save_to_file output goldoutput_file;
                 Printf.printf "Updated gold output for test %s\n" (name^"_"^(i_to_s i)))
              in
              if update_gold then
                assert_command ~exit_code:ecode ~foutput:output_updater ~ctxt:test_ctxt ~use_stderr:true scillabin args
              else
                assert_command ~exit_code:ecode ~foutput:output_verifier ~ctxt:test_ctxt ~use_stderr:true scillabin args
              )
          )
      in
      (* If this test is expected to succeed, we know that the JSONs are all "good".
       * So test both the JSON parsers, one that does validation, one that doesn't. 
       * Both should succeed. *)
      if ecode = WEXITED 0
      then
        (test true) :: (test false) :: (build_contract_tests env name ecode (i+1) n add_additional_lib)
      else
        (test false) :: (build_contract_tests env name ecode (i+1) n add_additional_lib)

let build_contract_init_test env name =
  name ^ "_" ^ "init" >::
  (fun test_ctxt ->
    (* Files for the contract are in examples/contract/(crowdfunding|zil-game|etc). *)
    let dir = env.tests_dir test_ctxt ^ sep ^ "contracts" ^ sep ^
      name ^ sep in
      let tmpdir = bracket_tmpdir test_ctxt in 
      let output_file = tmpdir ^ sep ^ name ^ "_init_output.json" in
      let args = ["-init"; dir ^ "init.json";
                  (* stdlib is in src/stdlib *)
                  "-libdir"; "src" ^ sep ^ "stdlib";
                  "-i"; dir ^ "contract.scilla";
                  "-o"; output_file;
                  "-jsonerrors";
                  "-gaslimit"; Core.Int.to_string testsuit_gas_limit;
                  "-iblockchain"; dir ^ "blockchain_1.json";]
            in
      (if (env.print_cli test_ctxt) then (Printf.printf "\nUsing CLI: %s " "scilla-runner"; print_args args));
      let scillabin = env.bin_dir test_ctxt ^ sep ^ "scilla-runner" in
        (* Ensure that the executable exists with 0 *)
        (assert_command ~ctxt:test_ctxt scillabin args;
          let goldoutput_file = dir ^ "init_output.json" in
          let g = load_file goldoutput_file in
          let o = load_file output_file in
          let update_gold = env.update_gold test_ctxt in
          if update_gold then
          (Printf.printf "Updating gold output for test %s\n" (name^"_"^"init");
            save_to_file o goldoutput_file);
          (* Compare output.json with a gold output in the contract directory *)
          assert_equal ~ctxt:test_ctxt ~msg:(Core.sprintf "Output json mismatch\nActual:\n%s\nExpected:\n%s" o g)
            ~cmp:(fun e o -> (String.trim e) = (String.trim o)) g o);
      ) 

let build_misc_tests env =
  let scillabin bin_dir test_ctxt =
    bin_dir test_ctxt ^ sep ^ "scilla-runner" in
  let output_file test_ctxt name =
    bracket_tmpdir test_ctxt ^ sep ^ name in
  let tests_dir_file testsdir test_ctxt name =
    testsdir test_ctxt ^ sep ^ "contracts" ^ sep ^ "crowdfunding" ^ sep ^ name in

  (* Test for exit 1 on bad json *)
  let test1 = 
    "misc_test_badjson_1" >::
      (fun test_ctxt ->
        let args = ["-init"; tests_dir_file env.tests_dir test_ctxt "init_bad1.json";
                    "-libdir"; "src" ^ sep ^ "stdlib";
                    "-jsonerrors";
                    "-i"; tests_dir_file env.tests_dir test_ctxt "contract.scilla";
                    "-o"; output_file test_ctxt "init_bad1_output.json";
                    "-iblockchain"; tests_dir_file env.tests_dir test_ctxt "blockchain_1.json"]
        in
        (if (env.print_cli test_ctxt) then (Printf.printf "\nUsing CLI: %s " "scilla-runner"; print_args args));
        let expected_code : Unix.process_status = WEXITED 1 in
          assert_command ~exit_code:expected_code ~ctxt:test_ctxt (scillabin env.bin_dir test_ctxt) args
      ) in

    let test2 = 
    "misc_test_badjson_2" >::
      (fun test_ctxt ->
        let args = ["-init"; tests_dir_file env.tests_dir test_ctxt "init_bad1.json";
                    "-libdir"; "src" ^ sep ^ "stdlib";
                    "-jsonerrors";
                    "-i"; tests_dir_file env.tests_dir test_ctxt "contract.scilla";
                    "-o"; output_file test_ctxt "init_bad2_output.json";
                    "-iblockchain"; tests_dir_file env.tests_dir test_ctxt "blockchain_1.json"]
        in
        (if (env.print_cli test_ctxt) then (Printf.printf "\nUsing CLI: %s " "scilla-runner"; print_args args));
        let expected_code : Unix.process_status = WEXITED 1 in
          assert_command ~exit_code:expected_code ~ctxt:test_ctxt (scillabin env.bin_dir test_ctxt) args
      ) in

    let test3 =
    "misc_test_badjson_3" >::
      (fun test_ctxt ->
        let args = ["-init"; tests_dir_file env.tests_dir test_ctxt "init_bad3.json";
                    "-libdir"; "src" ^ sep ^ "stdlib";
                    "-jsonerrors";
                    "-i"; tests_dir_file env.tests_dir test_ctxt "contract.scilla";
                    "-o"; output_file test_ctxt "init_bad2_output.json";
                    "-iblockchain"; tests_dir_file env.tests_dir test_ctxt "blockchain_1.json"]
        in
        (if (env.print_cli test_ctxt) then (Printf.printf "\nUsing CLI: %s " "scilla-runner"; print_args args));
        let expected_code : Unix.process_status = WEXITED 1 in
          assert_command ~exit_code:expected_code ~ctxt:test_ctxt (scillabin env.bin_dir test_ctxt) args
      ) in

      [test1;test2;test3]

let add_tests env = 
    let succ_code : Unix.process_status = WEXITED 0 in
    let fail_code : Unix.process_status = WEXITED 1 in
    let crowdfundingtests = "crowdfunding" >:::(build_contract_tests env "crowdfunding" succ_code 1 6 false) in
    let cfinit_test = "crowdfunding_init" >:(build_contract_init_test env "crowdfunding") in
    let zilgametests = "zil-game" >:::(build_contract_tests env "zil-game" succ_code 1 9 false) in
    let zginit_test = "zil-game_init" >:(build_contract_init_test env "zil-game") in
    let cfinvoketests = "cfinvoke" >:::(build_contract_tests env "cfinvoke" succ_code 1 4 false) in
    let pingtests = "ping" >:::(build_contract_tests env "ping" succ_code 0 3 false) in
    let pongtests = "pong" >:::(build_contract_tests env "pong" succ_code 0 3 false) in
    let helloWorldtests = "helloWorld" >:::(build_contract_tests env "helloWorld" succ_code 1 3 false) in
    let helloWorldtests_f = "helloWorld_f" >:::(build_contract_tests env "helloWorld" fail_code 4 9 false) in
    let auctiontests = "auction" >:::(build_contract_tests env "auction" succ_code 1 8 false) in
    let mappairtests = "mappair" >:::(build_contract_tests env "mappair" succ_code 1 7 false) in
    let bookstoretests = "bookstore" >:::(build_contract_tests env "bookstore" succ_code 1 10 false) in
    let mappairtests_f = "mappair" >:::(build_contract_tests env "mappair" fail_code 8 14 false) in
    let nonfungibletokentests = "nonfungible-token" >:::(build_contract_tests env "nonfungible-token" succ_code 1 12 false) in
    let nonfungibletokentests_expected_f = "nonfungible-token" >:::(build_contract_tests env "nonfungible-token" succ_code 21 27 false) in
    let schnorrtests = "schnorr" >:::(build_contract_tests env "schnorr" succ_code 1 3 false) in
    let ecdsatests = "ecdsa" >:::(build_contract_tests env "ecdsa" succ_code 1 3 false) in
    let emptytests = "empty_contract" >::: (build_contract_tests env "empty" succ_code 1 1 false) in
    let fungibletokentests = "fungible-token" >:::(build_contract_tests env "fungible-token" succ_code 0 8 false) in
    let inplace_map_tests = "inplace-map" >:::(build_contract_tests env "inplace-map" succ_code 1 14 false) in
    let wallettests = "wallet" >:::(build_contract_tests env "wallet" succ_code 1 11 false) in
    let misc_tests = "misc_tests" >::: build_misc_tests env in
    let simpledextests= "simple-dex" >:::(build_contract_tests env "simple-dex" succ_code 1 8 false) in
    let shogi_tests = "shogi" >::: (build_contract_tests env "shogi" succ_code 1 4 true) in
      "contract_tests" >::: [crowdfundingtests;cfinit_test;zilgametests;zginit_test;cfinvoketests;mappairtests; mappairtests_f;
                             misc_tests;pingtests;pongtests;fungibletokentests;helloWorldtests;helloWorldtests_f;
                             auctiontests;emptytests;bookstoretests;nonfungibletokentests_expected_f;nonfungibletokentests;
                             wallettests;schnorrtests;inplace_map_tests;ecdsatests;simpledextests;shogi_tests]

