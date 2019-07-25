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
open OUnit2
open ScillaUtil.FilePathInfix

type tsuite_env =
  { bin_dir : test_ctxt -> string;
    tests_dir : test_ctxt -> string;
    stdlib_dir : test_ctxt -> string;
    print_cli : test_ctxt -> bool;
    update_gold : test_ctxt -> bool;
    print_diff : test_ctxt -> bool;
  }

let output_verifier goldoutput_file msg print_diff output =
  (* load all data from file *)
  let gold_output = In_channel.read_all goldoutput_file in
  let pp_diff fmt =
    let config =
      let open Patdiff_lib.Configuration in
      parse (Config.t_of_sexp (Sexp.of_string default)) in
    let open Patdiff_lib in
    let gold = {Patdiff_core.name = goldoutput_file; text = gold_output} in
    let out = {Patdiff_core.name = "test output"; text = output} in
    let open Patdiff_lib.Compare_core in
    match diff_strings config ~old:gold ~new_:out with
    | `Same -> ()
    | `Different s ->  (* s contains ANSI color codes *)
        Format.pp_force_newline fmt ();
        Format.pp_print_string fmt s
  in
  if print_diff then
    assert_equal ~cmp:(fun e o -> (String.strip e) = (String.strip o))
      ~pp_diff:(fun fmt _ -> pp_diff fmt) gold_output output ~msg
  else
    assert_equal ~cmp:(fun e o -> (String.strip e) = (String.strip o))
      ~printer:(fun s -> s) gold_output output ~msg

let output_updater goldoutput_file test_name data =
  Out_channel.write_all goldoutput_file ~data;
  Printf.printf "Updated gold output for test %s\n" test_name

let prepare_cli_usage bin args =
  bin ^ " " ^ (String.concat ~sep:" " args)

let cli_usage_on_err bin args =
  "Command " ^ prepare_cli_usage bin args ^ " failed.\n"

let print_cli_usage flag bin args =
  if flag then begin
    Printf.printf "\nUsing CLI: %s\n" (prepare_cli_usage bin args);
  end

module type TestSuiteInput = sig
  val tests : string list
  val gold_path : string -> string -> string list
  val test_path : string -> string list
  val runner : string
  val exit_code : Unix.process_status
  val additional_libdirs : string list list
  val custom_args : string list
end

module DiffBasedTests(Input : TestSuiteInput) = struct
  open Input

  let build_exp_tests env = List.map ~f:(fun fname ->
    fname  >:: (fun test_ctxt ->
      let evalbin = env.bin_dir test_ctxt ^/ runner in
      let dir = env.tests_dir test_ctxt in
      let input_file = FilePath.make_filename (test_path fname) in
      (* Verify standard output of execution with gold file *)
      let goldoutput_file = FilePath.make_filename (gold_path dir fname) in
      let open FilePath in
      let additional_dirs = List.map ~f:make_filename additional_libdirs in
      let stdlib = env.stdlib_dir test_ctxt in
      let path = string_of_path @@ stdlib :: additional_dirs in
      let args = custom_args @ ["-libdir";path;"-jsonerrors";input_file;"-gaslimit";"4002000"] in
      let msg = cli_usage_on_err evalbin args in
      print_cli_usage (env.print_cli test_ctxt) evalbin args;
      assert_command
        ~foutput:(fun s ->
            let out = BatStream.to_string s in
            if env.update_gold test_ctxt
            then output_updater goldoutput_file input_file out
            else output_verifier goldoutput_file msg (env.print_diff test_ctxt) out
          )
        ~exit_code ~use_stderr:true ~chdir:dir ~ctxt:test_ctxt evalbin args))

  let add_tests env =
    "exptests" >::: build_exp_tests env tests
end
