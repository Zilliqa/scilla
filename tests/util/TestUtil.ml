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

let print_args args =
  List.iter ~f:(Printf.printf "%s ") args; Printf.printf "\n"

type tsuite_env =
  { bin_dir : test_ctxt -> string;
    tests_dir : test_ctxt -> string;
    stdlib_dir : test_ctxt -> string;
    print_cli : test_ctxt -> bool;
    update_gold : test_ctxt -> bool;
    print_diff : test_ctxt -> bool;
  }

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

  let build_exp_tests env = List.map ~f:(fun f ->
    f  >:: (fun test_ctxt ->
      let evalbin = env.bin_dir test_ctxt ^/ runner in
      let dir = env.tests_dir test_ctxt in
      let input_file = FilePath.make_filename (test_path f) in
      (* Verify standard output of execution with gold file *)
      let goldoutput_file = FilePath.make_filename (gold_path dir f) in
      let output_verifier out =
        (* load all data from file *)
        let gold_output = In_channel.read_all goldoutput_file in
        let output = BatStream.to_string out in
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
        if env.print_diff test_ctxt then
          assert_equal ~cmp:(fun e o -> (String.strip e) = (String.strip o))
            ~pp_diff:(fun fmt _ -> pp_diff fmt) gold_output output
        else
          assert_equal ~cmp:(fun e o -> (String.strip e) = (String.strip o))
            ~printer:(fun s -> s) gold_output output
      in
      let output_updater s =
        Out_channel.write_all goldoutput_file ~data:(BatStream.to_string s);
        Printf.printf "Updated gold for test %s\n" input_file
      in
      let open FilePath in
      let additional_dirs = List.map ~f:make_filename additional_libdirs in
      let stdlib = env.stdlib_dir test_ctxt in
      let path = string_of_path @@ stdlib :: additional_dirs in
      let args = custom_args @ ["-libdir";path;"-jsonerrors";input_file] in
      if env.print_cli test_ctxt then (Printf.printf "\nUsing CLI: "; print_args args);
      assert_command
        ~foutput:(if env.update_gold test_ctxt
                  then output_updater
                  else output_verifier)
        ~exit_code:exit_code ~use_stderr:true ~chdir:dir ~ctxt:test_ctxt evalbin args))

  let add_tests env =
    "exptests" >::: build_exp_tests env tests
end
