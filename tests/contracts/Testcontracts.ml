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
              "-i"; dir ^ "contract";
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


let add_tests bindir testsdir =
    let crowdfundingtests = "crowdfunding" >:::(build_contract_tests bindir testsdir "crowdfunding" 1 5) in
    let zilgametests = "zil-game" >:::(build_contract_tests bindir testsdir "zil-game" 1 5) in
      "contract_tests" >::: [crowdfundingtests;zilgametests]
