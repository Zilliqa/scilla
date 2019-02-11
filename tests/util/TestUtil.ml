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
  val exit_code : Unix.process_status
  val additional_libdirs : string list list
  val custom_args : string list
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



      let default_std_lib =
        match Sys.getenv_opt GlobalConfig.StdlibTracker.scilla_stdlib_env with
        | Some _ -> None
        | None -> Some (env.stdlib_dir test_ctxt)
      in
      let std_lib_dirs =
        match additional_libdirs with
        | [] -> default_std_lib
        | _ ->
            let additional_paths = List.map (fun path -> String.concat Filename.dir_sep path) additional_libdirs  in
            let additional_paths_str = String.concat ";" additional_paths in
            match default_std_lib with
            | None -> Some additional_paths_str
            | Some path -> Some (path ^ ";" ^ additional_paths_str) in

      let common_args =
        match std_lib_dirs with
        | None -> ["-jsonerrors";input_file]
        | Some libdirs -> ["-libdir";libdirs;"-jsonerrors";input_file] in
      let args = custom_args @ common_args in
      (if (env.print_cli test_ctxt) then
         (Printf.printf "\nUsing CLI: "; List.iter (fun arg -> Printf.printf "%s " arg) args);
      );
      let update_gold = env.update_gold test_ctxt in
      if update_gold then
        assert_command ~exit_code:exit_code ~use_stderr:true ~foutput:output_updater ~chdir:dir ~ctxt:test_ctxt evalbin (args)
      else
        assert_command ~exit_code:exit_code ~use_stderr:true ~foutput:output_verifier ~chdir:dir ~ctxt:test_ctxt evalbin (args)) in
    test :: build_exp_tests env r

  let add_tests env =
    let exptests = build_exp_tests env tests in
    "exptests" >::: exptests
    
end
