open OUnit2

let i_to_s i =
  Printf.sprintf "%d" i

(* 
 * Build tests to invoke scilla-runner with the right arguments, for
 * multiple test cases, each suffixed with _i up to _n (both inclusive)
 *)
let rec build_contract_tests bindir examplesdir name i n =
  if (i > n) 
    then [] 
  else
    let test = "crowdfunding_"^(Printf.sprintf "%d"  i) >::
      (fun test_ctxt ->
        let dir = examplesdir test_ctxt ^ Filename.dir_sep ^ "contracts" ^ Filename.dir_sep ^
          name ^ Filename.dir_sep in
        let args = ["-init"; dir ^ "init.json"; 
              "-i"; dir ^ "contract";
              "-o"; (bracket_tmpdir test_ctxt) ^ Filename.dir_sep ^ "output_"
                    ^ (i_to_s  i) ^ ".json";
              "-imessage"; dir ^ "message_" ^ (i_to_s i) ^ ".json";
              "-istate" ; dir ^ "state_" ^ (i_to_s i) ^ ".json";
              "-iblockchain" ; dir ^ "blockchain_" ^ (i_to_s i) ^ ".json"] in
        let scillabin = bindir test_ctxt ^ Filename.dir_sep ^ "scilla-runner" in
          assert_command test_ctxt scillabin args
      ) 
      in
      test :: (build_contract_tests bindir examplesdir name (i+1) n)


let add_tests bindir examplesdir =
    let crowdfundingtests = "crowdfunding" >:::(build_contract_tests bindir examplesdir "crowdfunding" 1 5) in
    let zilgametests = "zil-game" >:::(build_contract_tests bindir examplesdir "zil-game" 1 5) in
      "contract_tests" >::: [crowdfundingtests;zilgametests]
