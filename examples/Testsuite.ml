open OUnit2

let main =
  let bin_dir_default = (Sys.getcwd () ^ Filename.dir_sep ^ "bin") in
  let examples_dir_default = (Sys.getcwd () ^ Filename.dir_sep ^ "examples") in
  let bin_dir = Conf.make_string "bin_dir" bin_dir_default "directory containing binaries" in
  let examples_dir = Conf.make_string "examples_dir" examples_dir_default "directory containing examples" in
  (* Add calls to new tests from here *)
  let contract_tests = Testcontracts.add_tests bin_dir examples_dir in
    run_test_tt_main contract_tests
