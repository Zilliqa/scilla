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
open Printf
open GlobalConfig
open ErrorUtils
open PrettyPrinters
open DebugMessage
open ScillaUtil.FilePathInfix
open Literal
open Disambiguate
open ParserUtil

(* Parser for contracts and libraries *)
module RULocalFEParser = FrontEndParser.ScillaFrontEndParser (LocalLiteral)
module RULocalParser = RULocalFEParser.Parser
module RULocalSyntax = RULocalFEParser.FESyntax
module RULocalType = RULocalSyntax.SType
module RULocalIdentifier = RULocalSyntax.SIdentifier
module RULocalName = RULocalIdentifier.Name
module RUDisambiguation = ScillaDisambiguation (ParserRep) (ParserRep)

(* Parser for jsons *)
module RUGlobalFEParser = FrontEndParser.ScillaFrontEndParser (GlobalLiteral)
module RUGlobalParser = RUGlobalFEParser.Parser
module RUGlobalSyntax = RUGlobalFEParser.FESyntax
module RUGlobalIdentifier = RUGlobalSyntax.SIdentifier
module RUGlobalName = RUGlobalIdentifier.Name

let get_init_this_address_and_extlibs filename =
  if not (Caml.Sys.file_exists filename) then (
    plog (sprintf "Invalid init json %s file" filename);
    (None, []) )
  else
    try
      let this_address, name_addr_pairs =
        JSON.ContractState.get_init_this_address_and_extlibs filename in
      if
        List.contains_dup
          ~compare:(fun a b -> String.compare (fst a) (fst b))
          name_addr_pairs
      then
        fatal_error
        @@ mk_error0
          (sprintf "Duplicate extlib map entries in init JSON file %s."
             filename)
      else (this_address, name_addr_pairs)
    with Invalid_json s ->
      fatal_error
        (s @ mk_error0 (sprintf "Unable to parse JSON file %s. " filename))

(* Find (by looking for in StdlibTracker) and parse library named "id.scillib".
 * If "id.json" exists, parse it's extlibs info and provide that also. *)
let import_lib name sloc =
  let errmsg = sprintf "Failed to import library %s. " name in
  let fname, this_address, initf =
    match StdlibTracker.find_lib_dir name with
    | None -> fatal_error @@ mk_error1 (errmsg ^ "Not found.\n") sloc
    | Some d ->
        let libf = d ^/ name ^. StdlibTracker.file_extn_library in
        let initf = d ^/ name ^. "json" in
        let init_this_address, extlibs = get_init_this_address_and_extlibs initf in
        (* If this_address is unspecified in the init file, then use the base filename without extension as the address *)
        let this_address = Option.value init_this_address ~default:name in
        (libf, this_address, extlibs)
  in
  match RULocalFEParser.parse_file RULocalParser.Incremental.lmodule fname with
  | Error s -> fatal_error (s @ (mk_error1 "Failed to parse.\n") sloc)
  | Ok lmod ->
      plog (sprintf "Successfully imported external library %s\n" name);
      (lmod, this_address, initf)

let import_libs names_and_namespaces init_address_map =
  let rec importer names_and_namespaces address_map stack =
    let imported_libs_rev =
      List.fold_left names_and_namespaces ~init:[]
        ~f:(fun libacc_rev (libname, _) ->
            let open RULocalIdentifier in
            if List.mem stack (get_id libname) ~equal:[%equal : RULocalName.t] then
              let errmsg =
                match List.Assoc.find init_address_map (as_string libname) ~equal:String.(=) with
                | Some filename -> 
                    sprintf "Cyclic dependence found when importing %s (mapped to %s)."
                      (as_error_string libname) filename
                | None -> sprintf "Cyclic dependence found when importing %s." (as_error_string libname)
              in fatal_error @@ mk_error1 errmsg (get_rep libname)
            else
              let ilib_address =
                Option.value (List.Assoc.find address_map (as_string libname) ~equal:String.(=))
                  ~default:(as_string libname)
              in
              let ilib, this_address, ilib_import_map = import_lib (ilib_address) (get_rep libname) in
              let import_ilibs = importer ilib.elibs ilib_import_map (get_id libname :: stack) in
              (* Transform local names to global names *)
              match RUDisambiguation.disambiguate_lmodule ilib import_ilibs address_map this_address with
              | Error s -> fatal_error (s @ (mk_error1 (sprintf "Failed to disambiguate imported library %s.\n" (as_string libname)) (get_rep libname)))
              | Ok dis_lib -> 
                  let libnode = { RUGlobalSyntax.libn = dis_lib.libs; RUGlobalSyntax.deps = import_ilibs } in
                  libnode :: libacc_rev)
    in
    List.rev imported_libs_rev
  in
  importer names_and_namespaces init_address_map []

let stdlib_not_found_err ?(exe_name = Sys.argv.(0)) () =
  fatal_error
    (mk_error0
       ( "A path to Scilla stdlib not found. Please set "
         ^ StdlibTracker.scilla_stdlib_env
         ^ " environment variable, or pass through command-line argument for \
            this script.\n" ^ "Example:\n" ^ exe_name
         ^ " list_sort.scilla -libdir ./src/stdlib/\n" ))

(* Parse all libraries that can be found in ldirs. *)
let import_all_libs ldirs =
  (* Get list of scilla libraries in dir *)
  let get_lib_list dir =
    (* We don't throw an error if dir is invalid,
     * to be consistent with the behaviour of StdlibTracker.find_lib_dir.
     *)
    if not (Caml.Sys.file_exists dir) then []
    else
      let files = Array.to_list (Sys.readdir dir) in
      List.filter_map files ~f:(fun file ->
          let open FilePath in
          if check_extension file StdlibTracker.file_extn_library then
            let lib_name = chop_extension (basename file) in
            Some (RULocalIdentifier.mk_loc_id
                    (RULocalName.parse_simple_name lib_name), None (* no import-as *))
          else None)
  in
  (* Make a list of all libraries and parse them through import_lib above. *)
  let names = List.concat_map ldirs ~f:get_lib_list in
  import_libs names []

type runner_cli = {
  input_file : string;
  stdlib_dirs : string list;
  gas_limit : Stdint.uint64;
  (* Run gas use analysis? *)
  gua_flag : bool;
  init_file : string option;
  cf_flag : bool;
  cf_token_fields : string list;
  p_contract_info : bool;
  p_type_info : bool;
}

let parse_cli args ~exe_name =
  let r_stdlib_dir = ref [] in
  let r_gas_limit = ref None in
  let r_input_file = ref "" in
  let r_init_file = ref None in
  let r_json_errors = ref false in
  let r_gua = ref false in
  let r_contract_info = ref false in
  let r_type_info = ref false in
  let r_cf = ref false in
  let r_cf_token_fields = ref [] in
  let r_validate_json = ref true in

  let speclist =
    [
      ( "-version",
        Arg.Unit
          (fun () ->
             DebugMessage.pout
               (sprintf "Scilla version: %s\n"
                  PrettyPrinters.scilla_version_string);
             if true then exit 0;
             (* if "true" to avoid warning on exit 0 *)
             ()),
        "Print Scilla version and exit" );
      ( "-libdir",
        Arg.String
          (fun s -> r_stdlib_dir := !r_stdlib_dir @ FilePath.path_of_string s),
        "Path(s) to libraries separated with ':' (';' on windows)" );
      ( "-gaslimit",
        Arg.String
          (fun i ->
             let g = try Some (Stdint.Uint64.of_string i) with _ -> None in
             r_gas_limit := g),
        "Gas limit" );
      ( "-gua",
        Arg.Unit (fun () -> r_gua := true),
        "Run gas use analysis and print use polynomial." );
      ( "-init",
        Arg.String (fun x -> r_init_file := Some x),
        "Path to initialization json" );
      ( "-cf",
        Arg.Unit (fun () -> r_cf := true),
        "Run cashflow checker and print results" );
      ( "-cf-token-field",
        Arg.String (fun s -> r_cf_token_fields := s :: !r_cf_token_fields),
        "Make the cashflow checker consider a field to be money (implicitly \
         sets -cf)" );
      ( "-jsonerrors",
        Arg.Unit (fun () -> r_json_errors := true),
        "Print errors in JSON format" );
      ( "-contractinfo",
        Arg.Unit (fun () -> r_contract_info := true),
        "Print various contract information" );
      ( "-typeinfo",
        Arg.Unit (fun () -> r_type_info := true),
        "Print types of variables with location" );
      ( "-disable-validate-json",
        Arg.Unit (fun () -> r_validate_json := false),
        "Disable validation of input JSONs" );
    ]
  in

  let mandatory_usage =
    "Usage:\n" ^ exe_name
    ^ " -gaslimit <limit> -libdir /path/to/stdlib input.scilla\n"
  in
  let optional_usage =
    String.concat ~sep:"\n "
      (List.map ~f:(fun (flag, _, desc) -> flag ^ " " ^ desc) speclist)
  in
  let usage = mandatory_usage ^ "\n  " ^ optional_usage ^ "\n" in

  (* Only one input file allowed, so the last anonymous argument will be *it*. *)
  let anon_handler s = r_input_file := s in
  let () =
    match args with
    | None -> Arg.parse speclist anon_handler mandatory_usage
    | Some argv -> (
        try
          Arg.parse_argv ~current:(ref 0)
            (List.to_array @@ (exe_name :: argv))
            speclist anon_handler mandatory_usage
        with Arg.Bad msg -> fatal_error_noformat (Printf.sprintf "%s\n" msg) )
  in
  if String.is_empty !r_input_file then fatal_error_noformat usage;
  let gas_limit =
    match !r_gas_limit with Some g -> g | None -> fatal_error_noformat usage
  in
  if not @@ List.is_empty !r_cf_token_fields then r_cf := true;
  GlobalConfig.set_use_json_errors !r_json_errors;
  GlobalConfig.set_validate_json !r_validate_json;
  {
    input_file = !r_input_file;
    stdlib_dirs = !r_stdlib_dir;
    gas_limit;
    gua_flag = !r_gua;
    p_contract_info = !r_contract_info;
    cf_flag = !r_cf;
    cf_token_fields = !r_cf_token_fields;
    init_file = !r_init_file;
    p_type_info = !r_type_info;
  }
