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


open OUnit2

let i_to_s i =
  Printf.sprintf "%d" i

(* load an entire file to memory *)
let load_file f =
    Core.In_channel.read_all f

(* save string to file *)
let save_to_file s f =
  let open Core.Out_channel in
  with_file f ~f:(fun channel -> s |> output_string channel)

let string_of_chars chars = 
  let buf = Buffer.create 16 in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf

let stream_to_string (s : char Stream.t) =
  let result = ref [] in
    Stream.iter (fun value -> result := value :: !result) s;
  let l = List.rev !result in
    string_of_chars l

type tsuite_env = 
  { bin_dir : test_ctxt -> string;
    tests_dir : test_ctxt -> string;
    stdlib_dir : test_ctxt -> string;
    print_cli : test_ctxt -> bool;
    update_gold : test_ctxt -> bool;
  }

module type TestSuiteInput = sig
  val tests : string list
  val gold_path : string -> string -> string list
  val test_path : string -> string list
  val runner : string
  val use_stdlib : bool
end

module DiffBasedTests(Input : TestSuiteInput) = struct
  open Input

  let rec build_exp_tests env el =
  match el with
  | [] -> []
  | f :: r ->
    let test = f  >:: (fun test_ctxt ->
      let evalbin = env.bin_dir test_ctxt ^ Filename.dir_sep ^ runner in
      let dir = env.tests_dir test_ctxt in
      let input_file = String.concat Filename.dir_sep (test_path f) in
      (* Verify standard output of execution with gold file *)
      let goldoutput_file = 
        String.concat Filename.dir_sep (gold_path dir f)  in
      let output_verifier s =
        let output = stream_to_string s in
        let gold_output = load_file goldoutput_file in
        assert_equal ~cmp:(fun e o -> (String.trim e) = (String.trim o))
          ~printer:(fun s -> s) gold_output output
      in
      let output_updater s =
        let output = stream_to_string s in
        (save_to_file output goldoutput_file;
        Printf.printf "Updated gold for test %s\n" input_file);
      in
      let libdir = FilePath.make_relative dir (env.stdlib_dir test_ctxt) in
      let args = if use_stdlib then [input_file;libdir] else [input_file] in
      (if (env.print_cli test_ctxt) then
        if use_stdlib then
          (Printf.printf "\nUsing CLI: %s %s %s\n" runner input_file libdir)
        else
          (Printf.printf "\nUsing CLI: %s %s\n" runner input_file));
      let update_gold = env.update_gold test_ctxt in
      if update_gold then
        assert_command ~foutput:output_updater ~chdir:dir ~ctxt:test_ctxt evalbin (args)
      else
        assert_command ~foutput:output_verifier ~chdir:dir ~ctxt:test_ctxt evalbin (args)) in
    test :: build_exp_tests env r

  let add_tests env =
    let exptests = build_exp_tests env tests in
    "exptests" >::: exptests
    
end
