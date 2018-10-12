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



open Printf
open Syntax
open GlobalConfig
open ErrorUtils
open PrettyPrinters
open DebugMessage

(* Parse external libraries "names" (by looking for in StdlibTracker). *)
let import_libs names =
  List.map (fun id -> 
    let name = get_id id in
    let sloc = get_rep id in
    let errmsg = (sprintf "%s. " ("Failed to import library " ^ name)) in
    let dir = StdlibTracker.find_lib_dir name in
    let open Core in 
    let f = match dir with
      | None -> perr @@ scilla_error_to_string
         (mk_error1 (errmsg ^ "Not found.\n") sloc); exit 1
      | Some d -> d ^ Filename.dir_sep ^ name ^ ".scilla" in
    try
      let parse_lib = FrontEndParser.parse_file ScillaParser.lmodule f  in
      match parse_lib with
      | None -> perr @@ scilla_error_to_string
          (mk_error1 (errmsg ^ "Failed to parse.\n") sloc); exit 1
      | Some lib ->
        plog (sprintf "%s\n" "Successfully imported external library " ^ name);
        lib
    with | _ -> perr @@ scilla_error_to_string 
          (mk_error1 (errmsg ^ "Failed to parse.\n") sloc); exit 1
    ) names 

let stdlib_not_found_err () =
  (perr @@ scilla_error_to_string (mk_error0 
    ("A path to Scilla stdlib not found. Please set " ^ StdlibTracker.scilla_stdlib_env ^ 
     " environment variable, or pass through command-line argument for this script.\n" ^
     "Example:\n" ^ Sys.argv.(0) ^ " list_sort.scilla -libdir ./src/stdlib/\n"));
   exit 1)

(* Parse all libraries that can be found in ldirs. *)
let import_all_libs ldirs  =
  (* Get list of scilla libraries in dir *)
  let get_lib_list dir =
    if not (Sys.file_exists dir) then
      (perr @@ scilla_error_to_string (mk_error0 "Invalid stdlib director provided");
       exit 1);
    let files = Array.to_list (Sys.readdir dir) in
    List.fold_right (fun file names ->
      if Filename.extension file = ".scilla"
      then 
        let name = Filename.remove_extension (Filename.basename file) in
          asId name :: names
      else
        names) files []
  in
  (* Make a list of all libraries and parse them through import_libs above. *)
  let names = List.fold_right (fun dir names ->
    let names' = get_lib_list dir in
      List.append names names') ldirs []
  in
    import_libs names 

type runner_cli = {
  input_file : string;
  stdlib_dirs : string list;
}

let parse_cli () =
  let usage = " -libdir /path/to/stdlib [-simple-errors] input.scilla" in
  let r_stdlib_dir = ref "" in
  let r_input_file = ref "" in
  let r_json_errors = ref false in
  let speclist = [
    ("-libdir", Arg.String (fun x -> r_stdlib_dir := x), "Path to stdlib");
    ("-jsonerrors", Arg.Unit (fun () -> r_json_errors := true), "Print errors in JSON format");
  ] in 
  (* Only one input file allowed, so the last anonymous argument will be *it*. *)
  let anon_handler s = r_input_file := s in
  let () = Arg.parse speclist anon_handler ("Usage:\n" ^ usage) in
  if !r_input_file = "" then
    (DebugMessage.perr @@ "Usage:\n" ^ Sys.argv.(0) ^ usage ^ "\n"; exit 1);
  GlobalConfig.set_use_json_errors !r_json_errors;
  let stdlib_dirs = if !r_stdlib_dir = "" then [] else String.split_on_char ';' !r_stdlib_dir in
  { input_file = !r_input_file; stdlib_dirs = stdlib_dirs; }
