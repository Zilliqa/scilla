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
open DebugMessage

(* Parse external libraries "names" (by looking for in StdlibTracker). *)
let import_libs names =
  List.map (fun id -> 
    let name = get_id id in
    let errmsg = (sprintf "%s. " ("Failed to import library " ^ name)) in
    let dir = StdlibTracker.find_lib_dir name in
    let open Core in 
    let f = match dir with
      | None -> perr (errmsg ^ "Not found.\n") ; exit 1
      | Some d -> d ^ Filename.dir_sep ^ name ^ ".scilla" in
    try
      let parse_lib = FrontEndParser.parse_file ScillaParser.lmodule f in
      match parse_lib with
      | None -> perr (errmsg ^ "Failed to parse.\n"); exit 1
      | Some lib ->
        plog (sprintf "%s\n" "Successfully imported external library " ^ name);
        lib
    with | _ -> perr (errmsg ^ "Failed to parse.\n"); exit 1
    ) names 

(* Parse all libraries that can be found in ldirs. *)
let import_all_libs ldirs =
  (* Get list of scilla libraries in dir *)
  let get_lib_list dir =
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

(* Treat 2nd command line argument as a list of stdlib dirs
 * and add it to StdlibTracker to be tracked. *)
let add_cmd_stdlib () =
  (* If we have a 2nd command line argument, that is a list
     of directories to stdlib. Add that to the stdlib tracker. *)
  if (Array.length Sys.argv) == 3
  then
    (let dirs = Sys.argv.(2) in
    let dir_list = String.split_on_char ';' dirs in
    StdlibTracker.add_stdlib_dirs dir_list)

let stdlib_not_found_err () =
  printf "\n%s\n"
  ("A path to Scilla stdlib not found. Please set " ^ StdlibTracker.scilla_stdlib_env ^ 
    " environment variable, or pass as the second command-line argument for this script.\n" ^
    "Example:\n" ^ Sys.argv.(0) ^ " list_sort.scilla ./src/stdlib/\n");
   exit 1
