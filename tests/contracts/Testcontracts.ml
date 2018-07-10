(*
 * Copyright (c) 2018 - present Zilliqa, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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

(* 
 * Build tests to invoke scilla-runner with the right arguments, for
 * multiple test cases, each suffixed with _i up to _n (both inclusive)
 *)
let rec build_contract_tests bindir testsdir pcli name ecode i n =
  if (i > n) 
    then [] 
  else
    let test = name ^"_"^(i_to_s  i) >::
    (* function to run scilla-runner and check exit code *)
      (fun test_ctxt ->
        (* Files for the contract are in examples/contract/(crowdfunding|zil-game|etc). *)
        let dir = testsdir test_ctxt ^ sep ^ "contracts" ^ sep ^
          name ^ sep in
        let tmpdir = bracket_tmpdir test_ctxt in 
        let output_file = tmpdir ^ sep ^ name ^ "_output_"
                    ^ (i_to_s  i) ^ ".json" in
        let args = ["-init"; dir ^ "init.json"; 
              "-i"; dir ^ "contract.scilla";
              (* stdlib is in src/stdlib *)
              "-libdir"; "src" ^ sep ^ "stdlib";
              "-o"; output_file;
              "-imessage"; dir ^ "message_" ^ (i_to_s i) ^ ".json";
              "-istate" ; dir ^ "state_" ^ (i_to_s i) ^ ".json";
              "-iblockchain" ; dir ^ "blockchain_" ^ (i_to_s i) ^ ".json"] in
        (if (pcli test_ctxt) then (Printf.printf "\nUsing CLI: %s " "scilla-runner"; print_args args));
        let scillabin = bindir test_ctxt ^ sep ^ "scilla-runner" in
           (* Ensure that the executable exists with 0 *)
          (assert_command ~exit_code:ecode ~ctxt:test_ctxt ~use_stderr:true scillabin args;
           let goldoutput_file = dir ^ "output_" ^ (i_to_s i) ^ ".json" in
           let g = load_file goldoutput_file in
           if ecode = WEXITED 0 then
             let o = load_file output_file in
             (* Compare output.json with a gold output in the contract directory *)
             assert_equal ~cmp:(fun e o -> (String.trim e) = (String.trim o)) ~ctxt:test_ctxt
               ~msg:(Core.sprintf "Output json mismatch\nActual:\n%s\nExpected:\n%s" o g) g o);
      ) 
      in
      test :: (build_contract_tests bindir testsdir pcli name ecode (i+1) n)

let build_contract_init_test bindir testsdir pcli name =
  name ^ "_" ^ "init" >::
  (fun test_ctxt ->
    (* Files for the contract are in examples/contract/(crowdfunding|zil-game|etc). *)
    let dir = testsdir test_ctxt ^ sep ^ "contracts" ^ sep ^
      name ^ sep in
      let tmpdir = bracket_tmpdir test_ctxt in 
      let output_file = tmpdir ^ sep ^ name ^ "_init_output.json" in
      let args = ["-init"; dir ^ "init.json";
                  (* stdlib is in src/stdlib *)
                  "-libdir"; "src" ^ sep ^ "stdlib";
                  "-i"; dir ^ "contract.scilla";
                  "-o"; output_file;
                  "-iblockchain"; dir ^ "blockchain_1.json";]
            in
      (if (pcli test_ctxt) then (Printf.printf "\nUsing CLI: %s " "scilla-runner"; print_args args));
      let scillabin = bindir test_ctxt ^ sep ^ "scilla-runner" in
        (* Ensure that the executable exists with 0 *)
        (assert_command test_ctxt scillabin args;
          let goldoutput_file = dir ^ "init_output.json" in
          let g = load_file goldoutput_file in
          let o = load_file output_file in
          (* Compare output.json with a gold output in the contract directory *)
          assert_equal ~ctxt:test_ctxt ~msg:"Output json mismatch"
            ~cmp:(fun e o -> (String.trim e) = (String.trim o)) g o);
      ) 

let build_misc_tests bindir testsdir pcli =
  let scillabin bindir test_ctxt =
    bindir test_ctxt ^ sep ^ "scilla-runner" in
  let output_file test_ctxt name =
    bracket_tmpdir test_ctxt ^ sep ^ name in
  let tests_dir_file testsdir test_ctxt name =
    testsdir test_ctxt ^ sep ^ "contracts" ^ sep ^ "crowdfunding" ^ sep ^ name in

  (* Test for exit 1 on bad json *)
  let test1 = 
    "misc_test_badjson_1" >::
      (fun test_ctxt ->
        let args = ["-init"; tests_dir_file testsdir test_ctxt "init_bad1.json";
                    "-libdir"; "src" ^ sep ^ "stdlib";
                    "-i"; tests_dir_file testsdir test_ctxt "contract.scilla";
                    "-o"; output_file test_ctxt "init_bad1_output.json";
                    "-iblockchain"; tests_dir_file testsdir test_ctxt "blockchain_1.json"]
        in
        (if (pcli test_ctxt) then (Printf.printf "\nUsing CLI: %s " "scilla-runner"; print_args args));
        let expected_code : Unix.process_status = WEXITED 1 in
          assert_command ~exit_code:expected_code ~ctxt:test_ctxt (scillabin bindir test_ctxt) args
      ) in

    let test2 = 
    "misc_test_badjson_2" >::
      (fun test_ctxt ->
        let args = ["-init"; tests_dir_file testsdir test_ctxt "init_bad1.json";
                    "-libdir"; "src" ^ sep ^ "stdlib";
                    "-i"; tests_dir_file testsdir test_ctxt "contract.scilla";
                    "-o"; output_file test_ctxt "init_bad2_output.json";
                    "-iblockchain"; tests_dir_file testsdir test_ctxt "blockchain_1.json"]
        in
        (if (pcli test_ctxt) then (Printf.printf "\nUsing CLI: %s " "scilla-runner"; print_args args));
        let expected_code : Unix.process_status = WEXITED 1 in
          assert_command ~exit_code:expected_code ~ctxt:test_ctxt (scillabin bindir test_ctxt) args
      ) in

      [test1;test2]

let add_tests bindir testsdir pcli = 
    let succ_code : Unix.process_status = WEXITED 0 in
    let fail_code : Unix.process_status = WEXITED 1 in
    let crowdfundingtests = "crowdfunding" >:::(build_contract_tests bindir testsdir pcli "crowdfunding" succ_code 1 5) in
    let cfinit_test = "crowdfunding_init" >:(build_contract_init_test bindir testsdir pcli "crowdfunding") in
    let zilgametests = "zil-game" >:::(build_contract_tests bindir testsdir pcli "zil-game" succ_code 1 9) in
    let zginit_test = "zil-game_init" >:(build_contract_init_test bindir testsdir pcli "zil-game") in
    let cfinvoketests = "cfinvoke" >:::(build_contract_tests bindir testsdir pcli "cfinvoke" succ_code 1 4) in
    let pingtests = "ping" >:::(build_contract_tests bindir testsdir pcli "ping" succ_code 0 3) in
    let pongtests = "pong" >:::(build_contract_tests bindir testsdir pcli "pong" succ_code 0 3) in
    let helloWorldtests = "helloWorld" >:::(build_contract_tests bindir testsdir pcli "helloWorld" succ_code 1 3) in
    let helloWorldtests_f = "helloWorld_f" >:::(build_contract_tests bindir testsdir pcli "helloWorld" fail_code 4 8) in
    let auctiontests = "auction" >:::(build_contract_tests bindir testsdir pcli "auction" succ_code 1 8) in
    let mappairtests = "mappair" >:::(build_contract_tests bindir testsdir pcli "mappair" succ_code 1 1) in

    let fungibletokentests = "fungible-token" >:::(build_contract_tests bindir
    testsdir pcli "fungible-token" succ_code 0 8) in
    let misc_tests = "misc_tests" >::: build_misc_tests bindir testsdir pcli in
      "contract_tests" >::: [crowdfundingtests;cfinit_test;zilgametests;zginit_test;cfinvoketests;mappairtests;
                             misc_tests;pingtests;pongtests;fungibletokentests;helloWorldtests;helloWorldtests_f;auctiontests]
