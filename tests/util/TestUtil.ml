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

open Core_kernel
open! Int.Replace_polymorphic_compare
open OUnit2
open ScillaUtil.FilePathInfix

(* Helper funcation borrowed from Batteries library *)
let stream_to_string fl =
  let buf = Buffer.create 4096 in
  Stream.iter (Buffer.add_char buf) fl;
  Buffer.contents buf

type tsuite_env = {
  tests_dir : test_ctxt -> string;
  stdlib_dir : test_ctxt -> string;
  print_cli : test_ctxt -> bool;
  update_gold : test_ctxt -> bool;
  print_diff : test_ctxt -> bool;
  ext_ipc_server : test_ctxt -> string;
  server : test_ctxt -> bool;
}

let run_tests tests =
  let tests_dir_default = Sys.getcwd () ^/ "tests" in
  let stdlib_dir_default = Sys.getcwd () ^/ "src" ^/ "stdlib" in
  let ext_ipc_server_default = "" in
  let tests_dir =
    Conf.make_string "tests_dir" tests_dir_default "Directory containing tests"
  in
  let stdlib_dir =
    Conf.make_string "stdlib_dir" stdlib_dir_default
      "Directory containing stdlib"
  in
  let print_cli =
    Conf.make_bool "print_cli" false
      "Print command line arguments used for test(s)"
  in
  let update_gold =
    Conf.make_bool "update_gold" false
      "Ignore compare mismatch and update gold file(s)"
  in
  let print_diff =
    Conf.make_bool "print_diff" false
      "Print the diff between gold file and actual output"
  in
  let server = Conf.make_bool "server" false "Run tests in server-mode" in
  let ext_ipc_server =
    Conf.make_string "ext_ipc_server" ext_ipc_server_default
      "Address of external IPC server for IPC tests. Ensure that \"-runner \
       sequential\" is set"
  in

  let env : tsuite_env =
    {
      tests_dir;
      stdlib_dir;
      print_cli;
      update_gold;
      print_diff;
      server;
      ext_ipc_server;
    }
  in
  run_test_tt_main ("all_tests" >::: List.map ~f:(( |> ) env) tests)

let output_verifier goldoutput_file msg print_diff output =
  (* load all data from file *)
  let gold_output = In_channel.read_all goldoutput_file in
  let pp_diff fmt =
    let config =
      let open Patdiff_lib.Configuration in
      parse (Config.t_of_sexp (Sexp.of_string default))
    in
    let open Patdiff_lib in
    let gold = { Patdiff_core.name = goldoutput_file; text = gold_output } in
    let out = { Patdiff_core.name = "test output"; text = output } in
    let open Patdiff_lib.Compare_core in
    match diff_strings config ~prev:gold ~next:out with
    | `Same -> ()
    | `Different s ->
        (* s contains ANSI color codes *)
        Format.pp_force_newline fmt ();
        Format.pp_print_string fmt s
  in
  if print_diff then
    assert_equal
      ~cmp:(fun e o -> String.(strip e = strip o))
      ~pp_diff:(fun fmt _ -> pp_diff fmt)
      gold_output output ~msg
  else
    assert_equal
      ~cmp:(fun e o -> String.(strip e = strip o))
      ~printer:(fun s -> s)
      gold_output output ~msg

let output_updater goldoutput_file test_name data =
  Out_channel.write_all goldoutput_file ~data;
  Printf.printf "Updated gold output for test %s\n" test_name

let prepare_cli_usage bin args = bin ^ " " ^ String.concat ~sep:" " args

let cli_usage_on_err bin args =
  "Command " ^ prepare_cli_usage bin args ^ " failed.\n"

let print_cli_usage flag bin args =
  if flag then Printf.printf "\nUsing CLI: %s\n" (prepare_cli_usage bin args)

module type TestSuiteInput = sig
  val tests : string list

  val gold_path : string -> string -> string list

  val test_path : string -> string list

  val runner : string

  val ignore_predef_args : bool

  val exit_code : Unix.process_status

  val additional_libdirs : string list list

  val gas_limit : Stdint.uint64

  val custom_args : string list

  val provide_init_arg : bool
end

module DiffBasedTests (Input : TestSuiteInput) = struct
  open Input

  let build_exp_tests env =
    List.map ~f:(fun fname ->
        fname >:: fun test_ctxt ->
        let open FilePath in
        let dir = env.tests_dir test_ctxt in
        let input_file = make_filename (test_path fname) in
        let init_file =
          make_filename (test_path (chop_extension fname ^ ".json"))
        in
        (* Verify standard output of execution with gold file *)
        let goldoutput_file = make_filename (gold_path dir fname) in
        let additional_dirs = List.map ~f:make_filename additional_libdirs in
        let stdlib = make_relative dir (env.stdlib_dir test_ctxt) in
        let path = string_of_path @@ (stdlib :: additional_dirs) in
        let args' =
          if ignore_predef_args then custom_args @ [ input_file ]
          else
            custom_args
            @ [
                "-libdir";
                path;
                "-jsonerrors";
                input_file;
                "-gaslimit";
                Stdint.Uint64.to_string gas_limit;
              ]
        in
        let args =
          if provide_init_arg then args' @ [ "-init"; init_file ] else args'
        in
        let msg = cli_usage_on_err runner args in
        print_cli_usage (env.print_cli test_ctxt) runner args;
        assert_command
          ~foutput:(fun s ->
            let out = stream_to_string s in
            if env.update_gold test_ctxt then
              output_updater goldoutput_file input_file out
            else
              output_verifier goldoutput_file msg (env.print_diff test_ctxt) out)
          ~exit_code ~use_stderr:true ~chdir:dir ~ctxt:test_ctxt runner args)

  let all_tests env = "exptests" >::: build_exp_tests env tests
end
