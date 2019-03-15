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


open Core
open Printf
open Syntax
open GlobalConfig
open ErrorUtils
open PrettyPrinters
open DebugMessage
open ScillaUtil.FilePathInfix

(* Find (by looking for in StdlibTracker) and parse library named "id". *)
let import_lib id =
  let name = get_id id in
  let errmsg = sprintf "Failed to import library %s. " name in
  let sloc = get_rep id in
  let fname = match StdlibTracker.find_lib_dir name with
    | None -> fatal_error @@ mk_error1(errmsg ^ "Not found.\n") sloc
    | Some d -> d ^/ name ^. StdlibTracker.file_extn_library
  in
    match FrontEndParser.parse_file ScillaParser.lmodule fname with
    | Error s -> fatal_error (s @ (mk_error1 "Failed to parse.\n") sloc)
    | Ok lmod ->
        plog (sprintf "Successfully imported external library %s\n" name);
        lmod

(* Import all libraries in "names" (and their dependences).
 * The order of the returned libraries is an RPO traversal
 * over the dependence graph generated out of "names".
 *)
let import_libs names =
  let rec importer names stack importedl =
    List.fold_left ~f:(fun (ilibs, importedl) l ->
      let name = get_id l in
      if List.mem stack name ~equal:(=) then
        fatal_error @@ mk_error1 (sprintf "Cyclic dependence found when importing %s." name) (get_rep l) else
      if List.mem importedl name ~equal:(=) then (ilibs, importedl) else
      let ilib = import_lib l in
      let (ilibs'', importedl'') = importer ilib.elibs (name :: stack) (name :: importedl) in
      (* Order in which we return the list of imported libraries is important. *)
      (ilibs @ ilibs'' @ [ilib], importedl'')
    ) ~init:([], importedl) names
  in
  let (libs, _) = importer names [] [] in
  (* Return library list rather than lmodule list. *)
  List.map ~f:(fun (l : ParserUtil.ParsedSyntax.lmodule) -> l.libs) libs

let stdlib_not_found_err () =
  fatal_error (mk_error0
    ("A path to Scilla stdlib not found. Please set " ^ StdlibTracker.scilla_stdlib_env ^
     " environment variable, or pass through command-line argument for this script.\n" ^
     "Example:\n" ^ Sys.argv.(0) ^ " list_sort.scilla -libdir ./src/stdlib/\n"))

(* Parse all libraries that can be found in ldirs. *)
let import_all_libs ldirs  =
  (* Get list of scilla libraries in dir *)
  let get_lib_list dir =
    (* We don't throw an error if dir is invalid,
     * to be consistent with the behaviour of StdlibTracker.find_lib_dir.
     *)
    if not (Caml.Sys.file_exists dir) then [] else

    let files = Array.to_list (Sys.readdir dir) in
    List.fold_right files ~f:(fun file names ->
      if FilePath.get_extension file = StdlibTracker.file_extn_library
      then
        let name = FilePath.chop_extension (FilePath.basename file) in
          asId name :: names
      else
        names) ~init:[]
  in
  (* Make a list of all libraries and parse them through import_lib above. *)
  let names = List.fold_right ldirs ~f:(fun dir names ->
    let names' = get_lib_list dir in
      List.append names names') ~init:[]
  in
    (* We don't need to look for dependences of imported libraries
     * because we are importing _all_ libraries that we can find. *)
  List.map names ~f:(fun l -> (import_lib l).libs)

type runner_cli = {
  input_file : string;
  stdlib_dirs : string list;
  init_file : string;
  cf_flag : bool;
  p_contract_info : bool;
}


let parse_cli () =
  let r_stdlib_dir = ref [] in
  let r_input_file = ref "" in
  let r_init_file = ref "" in
  let r_json_errors = ref false in
  let r_contract_info = ref false in
  let r_cf = ref false in
  let speclist = [
    ("-version", Arg.Unit (fun () -> 
        DebugMessage.pout
          (sprintf "Scilla version: %s\n" PrettyPrinters.scilla_version_string);
          if true then exit 0; (* if "true" to avoid warning on exit 0 *)
          ()
      ), "Print Scilla version and exit");
    ("-libdir", Arg.String (fun s ->
           r_stdlib_dir := !r_stdlib_dir @ FilePath.path_of_string s
        ),
      "Path(s) to libraries separated with ':' (';' on windows)");
    ("-init", Arg.String (fun x -> r_init_file := x), "Path to initialization json");
    ("-cf", Arg.Unit (fun () -> r_cf := true), "Run cashflow checker and print results.");
    ("-jsonerrors", Arg.Unit (fun () -> r_json_errors := true), "Print errors in JSON format");
    ("-contractinfo", Arg.Unit (fun () -> r_contract_info := true), "Print various contract information");
  ] in 

  let mandatory_usage = "Usage:\n" ^ Sys.argv.(0) ^ " -libdir /path/to/stdlib input.scilla\n" in
  let optional_usage = String.concat ~sep:"\n  "
    (List.map ~f:(fun (flag,_,desc) -> flag ^ " " ^ desc) speclist) in
  let usage = mandatory_usage ^ "\n  " ^ optional_usage ^ "\n" in

  (* Only one input file allowed, so the last anonymous argument will be *it*. *)
  let anon_handler s = r_input_file := s in
  let () = Arg.parse speclist anon_handler mandatory_usage in
  if !r_input_file = "" then fatal_error (mk_error0 usage);
  GlobalConfig.set_use_json_errors !r_json_errors;
  { input_file = !r_input_file; stdlib_dirs = !r_stdlib_dir; cf_flag = !r_cf;
    p_contract_info = !r_contract_info; init_file = !r_init_file }
