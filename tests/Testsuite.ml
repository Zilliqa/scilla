open OUnit2

let main =
  let bin_dir_default = (Sys.getcwd () ^ Filename.dir_sep ^ "bin") in
  let tests_dir_default = (Sys.getcwd () ^ Filename.dir_sep ^ "tests") in
  let bin_dir = Conf.make_string "bin_dir" bin_dir_default "directory containing binaries" in
  let tests_dir = Conf.make_string "tests_dir" tests_dir_default "directory containing tests" in
  let print_cli = Conf.make_bool "print_cli" false "print command line arguments used for test(s)" in

  (* Add calls to new tests from here *)
  let contract_tests = Testcontracts.add_tests bin_dir tests_dir print_cli in
  let exp_tests = Testexp.add_tests bin_dir tests_dir print_cli in

  let all_tests = "all_tests" >::: [contract_tests;exp_tests] in
  (* Run all tests *)
  run_test_tt_main all_tests
