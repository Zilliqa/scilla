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

let string_of_chars chars = 
  let buf = Buffer.create 16 in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf

let stream_to_string (s : char Stream.t) =
  let result = ref [] in
    Stream.iter (fun value -> result := value :: !result) s;
  let l = List.rev !result in
    string_of_chars l

(* 
 * TODO: How to generate this list dynamically? We know the actual
 * test directory only through "examplesdir test_ctxt", and test_ctxt
 * is available only to the actual test function, not the test generation
 * function. 
 *)
let explist = [
  "addr.scilla"; "app5.scilla"; "builtin1.scilla"; "cons.scilla";
  "hash3.scilla"; "let-error.scilla"; "map3.scilla"; "msg_error2.scilla";
  "option.scilla"; "pm1.scilla"; "app2.scilla"; "app_error1.scilla";
  "builtin2.scilla"; "hash4.scilla"; "let.scilla"; "map4.scilla";
  "msg_error3.scilla"; "pair1.scilla"; "pm2.scilla";
  "app3.scilla"; "app_error2.scilla"; "builtin3.scilla"; "hash1.scilla";
  "map1.scilla"; "map5.scilla"; "msg_error.scilla"; "pair2.scilla";
  "pm3.scilla"; "app4.scilla"; "app.scilla"; "builtin_error1.scilla";
  "hash2.scilla"; "let-builtin.scilla"; "map2.scilla"; "map6.scilla";
  "msg.scilla"; "pair3.scilla"; "pm_app.scilla"
]

let string_printer s = s

let rec build_exp_tests bindir examplesdir el =
  match el with
  | [] -> []
  | f :: r ->
    let test = f  >:: (fun test_ctxt ->
      let evalbin = bindir test_ctxt ^ Filename.dir_sep ^ "eval-runner" in
      let dir = examplesdir test_ctxt ^ Filename.dir_sep ^ "eval" ^ 
        Filename.dir_sep ^ "exp" ^ Filename.dir_sep in
      let input_file = dir ^ f in
      (* Verify standard output of execution with gold file *)
      let goldoutput_file = dir ^ "gold" ^ Filename.dir_sep ^ f ^ ".gold" in
      let output_verifier s =
        let output = stream_to_string s in
        let gold_output = load_file goldoutput_file in
          assert_equal ~printer:string_printer gold_output output
      in
      assert_command ~foutput:output_verifier ~ctxt:test_ctxt evalbin (input_file::[])) in
    test :: build_exp_tests bindir examplesdir r

let add_tests bindir examplesdir =
  let exptests = build_exp_tests bindir examplesdir explist in
    "exptests" >::: exptests