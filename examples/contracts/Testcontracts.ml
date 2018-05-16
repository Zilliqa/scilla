open OUnit2

let i_to_s i =
  Printf.sprintf "%d" i

(* load an entire file to memory *)
let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  (s)
  
(* 
 * Build tests to invoke scilla-runner with the right arguments, for
 * multiple test cases, each suffixed with _i up to _n (both inclusive)
 *)
let rec build_contract_tests bindir examplesdir name i n =
  if (i > n) 
    then [] 
  else
    let test = name ^"_"^(i_to_s  i) >::
    (* function to run scilla-runner and check exit code *)
      (fun test_ctxt ->
        let dir = examplesdir test_ctxt ^ Filename.dir_sep ^ "contracts" ^ Filename.dir_sep ^
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
          (assert_command test_ctxt scillabin args;
           let goldoutput_file = dir ^ "output_" ^ (i_to_s i) ^ ".json" in
           let g = load_file goldoutput_file in
           let o = load_file output_file in
           assert_equal ~ctxt:test_ctxt ~msg:"Output json mismatch" g o);
      ) 
      in
      test :: (build_contract_tests bindir examplesdir name (i+1) n)


let add_tests bindir examplesdir =
    let crowdfundingtests = "crowdfunding" >:::(build_contract_tests bindir examplesdir "crowdfunding" 1 5) in
    let zilgametests = "zil-game" >:::(build_contract_tests bindir examplesdir "zil-game" 1 5) in
      "contract_tests" >::: [crowdfundingtests;zilgametests]
