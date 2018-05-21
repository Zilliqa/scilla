open OUnit2

let i_to_s i =
  Printf.sprintf "%d" i

(* load an entire file to memory *)
let load_file f =
    Core.In_channel.read_all f

(* 
 * Build tests to invoke scilla-runner with the right arguments, for
 * multiple test cases, each suffixed with _i up to _n (both inclusive)
 *)
let rec build_contract_tests bindir testsdir name i n =
  if (i > n) 
    then [] 
  else
    let test = name ^"_"^(i_to_s  i) >::
    (* function to run scilla-runner and check exit code *)
      (fun test_ctxt ->
        (* Files for the contract are in examples/contract/(crowdfunding|zil-game|etc). *)
        let dir = testsdir test_ctxt ^ Filename.dir_sep ^ "contracts" ^ Filename.dir_sep ^
          name ^ Filename.dir_sep in
        let tmpdir = bracket_tmpdir test_ctxt in 
        let output_file = tmpdir ^ Filename.dir_sep ^ name ^ "_output_"
                    ^ (i_to_s  i) ^ ".json" in
        let args = ["-init"; dir ^ "init.json"; 
              "-i"; dir ^ "contract.scilla";
              "-o"; output_file;
              "-imessage"; dir ^ "message_" ^ (i_to_s i) ^ ".json";
              "-istate" ; dir ^ "state_" ^ (i_to_s i) ^ ".json";
              "-iblockchain" ; dir ^ "blockchain_" ^ (i_to_s i) ^ ".json"] in
        let scillabin = bindir test_ctxt ^ Filename.dir_sep ^ "scilla-runner" in
           (* Ensure that the executable exists with 0 *)
          (assert_command test_ctxt scillabin args;
           let goldoutput_file = dir ^ "output_" ^ (i_to_s i) ^ ".json" in
           let g = load_file goldoutput_file in
           let o = load_file output_file in
           (* Compare output.json with a gold output in the contract directory *)
           assert_equal ~ctxt:test_ctxt ~msg:"Output json mismatch" g o);
      ) 
      in
      test :: (build_contract_tests bindir testsdir name (i+1) n)

let build_contract_init_test bindir testsdir name =
  name ^ "_" ^ "init" >::
  (fun test_ctxt ->
    (* Files for the contract are in examples/contract/(crowdfunding|zil-game|etc). *)
    let dir = testsdir test_ctxt ^ Filename.dir_sep ^ "contracts" ^ Filename.dir_sep ^
      name ^ Filename.dir_sep in
      let tmpdir = bracket_tmpdir test_ctxt in 
      let output_file = tmpdir ^ Filename.dir_sep ^ name ^ "_init_output.json" in
      let args = ["-init"; dir ^ "init.json";
                  "-i"; dir ^ "contract.scilla";
                  "-o"; output_file;
                  "-iblockchain"; dir ^ "blockchain_1.json";]
            in
      let scillabin = bindir test_ctxt ^ Filename.dir_sep ^ "scilla-runner" in
        (* Ensure that the executable exists with 0 *)
        (assert_command test_ctxt scillabin args;
          let goldoutput_file = dir ^ "init_output.json" in
          let g = load_file goldoutput_file in
          let o = load_file output_file in
          (* Compare output.json with a gold output in the contract directory *)
          assert_equal ~ctxt:test_ctxt ~msg:"Output json mismatch" g o);
      ) 

let build_misc_tests bindir testsdir =
  let scillabin bindir test_ctxt =
    bindir test_ctxt ^ Filename.dir_sep ^ "scilla-runner" in
  let output_file test_ctxt name =
    bracket_tmpdir test_ctxt ^ Filename.dir_sep ^ name in
  let tests_dir_file testsdir test_ctxt name =
    testsdir test_ctxt ^ "Filename.dir_sep" ^ "contracts" ^ Filename.dir_sep ^ name in

  (* Test for exit 1 on bad json *)
  let test1 = 
    "misc_test_badjson_1" >::
      (fun test_ctxt ->
        let args = ["-init"; tests_dir_file testsdir test_ctxt "init_bad1.json";
                    "-i"; tests_dir_file testsdir test_ctxt "contract.scilla";
                    "-o"; output_file test_ctxt "init_bad1_output.json";
                    "-iblockchain"; tests_dir_file testsdir test_ctxt "blockchain_1.json"]
        in
        let expected_code : Unix.process_status = WEXITED 1 in
          assert_command ~exit_code:expected_code ~ctxt:test_ctxt (scillabin bindir test_ctxt) args
      ) in

    let test2 = 
    "misc_test_badjson_2" >::
      (fun test_ctxt ->
        let args = ["-init"; tests_dir_file testsdir test_ctxt "init_bad1.json";
                    "-i"; tests_dir_file testsdir test_ctxt "contract.scilla";
                    "-o"; output_file test_ctxt "init_bad2_output.json";
                    "-iblockchain"; tests_dir_file testsdir test_ctxt "blockchain_1.json"]
        in
        let expected_code : Unix.process_status = WEXITED 1 in
          assert_command ~exit_code:expected_code ~ctxt:test_ctxt (scillabin bindir test_ctxt) args
      ) in

      [test1;test2]

let add_tests bindir testsdir =
    let crowdfundingtests = "crowdfunding" >:::(build_contract_tests bindir testsdir "crowdfunding" 1 5) in
    let cfinit_test = "crowdfunding_init" >:(build_contract_init_test bindir testsdir "crowdfunding") in
    let zilgametests = "zil-game" >:::(build_contract_tests bindir testsdir "zil-game" 1 5) in
    let zginit_test = "zil-game_init" >:(build_contract_init_test bindir testsdir "zil-game") in
    let misc_tests = "misc_tests" >::: build_misc_tests bindir testsdir in
      "contract_tests" >::: [crowdfundingtests;cfinit_test;zilgametests;zginit_test;misc_tests]
