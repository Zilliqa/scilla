open OUnit2

let i_to_s i =
  Printf.sprintf "%d" i

(* load an entire file to memory *)
let load_file f =
    Core.In_channel.read_all f

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
 * test directory only through "testsdir test_ctxt", and test_ctxt
 * is available only to the actual test function, not the test generation
 * function. The only way to make it on-the-fly is for the test itself
 * to scan the dir, which would put all these as one test, instead of
 * treating them separately. That would be bad for reporting failures.
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
  "msg.scilla"; "pair3.scilla"; "pm_app.scilla"; "pm_nesting.scilla";
  "string1.scilla"; "string2.scilla"; "string_error1.scilla";
  "nat_eq_foldl.scilla"; "nat_eq_false.scilla"; "times_two.scilla";
  "fib.scilla"; "id.scilla"; "hof2.scilla"; "hof3.scilla";
  "list_map.scilla"; "list_product.scilla"; "builtin-strings.scilla";
  "list_filter.scilla"; "list_first.scilla"
]

let rec build_exp_tests bindir testsdir pcli el =
  match el with
  | [] -> []
  | f :: r ->
    let test = f  >:: (fun test_ctxt ->
      let evalbin = bindir test_ctxt ^ Filename.dir_sep ^ "eval-runner" in
      let dir = testsdir test_ctxt in
      let input_file = String.concat Filename.dir_sep ["eval"; "exp"; f] in
      (* Verify standard output of execution with gold file *)
      let goldoutput_file = 
        String.concat Filename.dir_sep [dir; "eval"; "exp"; "gold"; f ^ ".gold" ] in
      let output_verifier s =
        let output = stream_to_string s in
        let gold_output = load_file goldoutput_file in
        assert_equal ~cmp:(fun e o -> (String.trim e) = (String.trim o))
          ~printer:(fun s -> s) gold_output output
      in
      (if (pcli test_ctxt) then (Printf.printf "\nUsing CLI: %s %s\n" "eval-runner" input_file));
      assert_command ~foutput:output_verifier ~chdir:dir ~ctxt:test_ctxt evalbin (input_file::[])) in
    test :: build_exp_tests bindir testsdir pcli r

let add_tests bindir testsdir pcli =
  let exptests = build_exp_tests bindir testsdir pcli explist in
    "exptests" >::: exptests
