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
open ParserUtil.ParsedSyntax
open ScillaUtil.FilePathInfix

let get_init_extlibs filename =
  if not (Caml.Sys.file_exists filename)
  then
    (plog (sprintf "Invalid init json %s file" filename); [])
  else
    try JSON.ContractState.get_init_extlibs filename with
    | Invalid_json s ->
      (* Inability to fetch extlibs info from init json shouldn't be fatal error. *)
      plog (scilla_error_to_string s);
      []

(* Find (by looking for in StdlibTracker) and parse library named "id.scillib".
 * If "id.json" exists, parse it's extlibs info and provide that also. *)
let import_lib id =
  let name = get_id id in
  let errmsg = sprintf "Failed to import library %s. " name in
  let sloc = get_rep id in
  let (fname, initf) = match StdlibTracker.find_lib_dir name with
    | None -> fatal_error @@ mk_error1(errmsg ^ "Not found.\n") sloc
    | Some d -> 
      let libf = d ^/ name ^. StdlibTracker.file_extn_library in
      let initf = d ^/ name ^. "json" in
        (libf, get_init_extlibs initf)
  in
    match FrontEndParser.parse_file ScillaParser.lmodule fname with
    | Error s -> fatal_error (s @ (mk_error1 "Failed to parse.\n") sloc)
    | Ok lmod ->
        plog (sprintf "Successfully imported external library %s\n" name);
        (lmod, initf)

(* light-weight namespaces. prefix all entries in lib with their namespace. *)
let namespace_prefix lib ns =
  match ns with
  | None -> lib
  | Some name' ->
    let name = get_id name' in
    let initenv = [] in
    let (rev_entries, _) = List.fold lib.lentries ~init:([], initenv)
    ~f:(fun (accentries, accenv) entry ->
      (* check if id is in env and prefix it with a namespace. *)
      let check_and_prefix_id env id =
        if List.mem env (get_id id) ~equal:(=)
        then
          let nname = name ^ "." ^ (get_id id) in
          asIdL nname (get_rep id)
        else
          id
      in
      let check_and_prefix_string env cname =
        if List.mem env cname ~equal:(=) then name ^ "." ^ cname else cname
      in
      let rename_in_type t env =
        let rec recurser t =
          match t with
          | PrimType _ | TypeVar _ | Unit -> t
          | MapType (kt, vt) -> MapType (recurser kt, recurser vt)
          | FunType (t1, t2) -> FunType (recurser t1, recurser t2)
          | PolyFun (tvar, t) -> PolyFun (tvar, recurser t)
          | ADT (tname, tlist) ->
            let tname' = check_and_prefix_string env tname in
            let tlist' = List.map tlist ~f:(fun t -> recurser t) in
            ADT(tname', tlist')
        in
        recurser t
      in
      let rec rename_in_expr (e, eloc) env =
        (match e with
        | Literal _ -> (e, eloc)
        | Var v  ->
           (Var (check_and_prefix_id env v), eloc)
        | Let (i, t, elhs, erhs) ->
          let elhs' = rename_in_expr elhs env in
          (* "i" get's a local binding now, don't rename it in rhs. *)
          let env' = List.filter env ~f:((<>) (get_id i)) in
          let erhs' = rename_in_expr erhs env' in
          (Let (i, t, elhs', erhs'), eloc)
        | Message spl ->
          let rename_in_payload pl = (match pl with | MLit _ -> pl | MVar v -> MVar (check_and_prefix_id env v)) in
          let spl' = List.map spl ~f:(fun (s, pl) -> (s, rename_in_payload pl)) in
          (Message spl', eloc)
        | Fun (i, t, exp) ->
          let env' = List.filter env ~f:((<>) (get_id i)) in
          let t' = rename_in_type t env' in
          let exp' = rename_in_expr exp env' in
          (Fun (i, t', exp'), eloc)
        | App (i, ils) ->
          let i' = check_and_prefix_id env i in
          let ils' = List.map ils ~f:(check_and_prefix_id env) in
          (App (i', ils'), eloc)
        | Constr (cname, tl, idl) ->
          let cname' = check_and_prefix_string env cname in
          let tl' = List.map tl ~f:(fun t -> rename_in_type t env) in
          let idl' = List.map idl ~f:(check_and_prefix_id env) in
          (Constr(cname', tl', idl'), eloc)
        | MatchExpr (pv, pelist) ->
          (* Update pattern and returns a list of newly bounds. *)
          let rec rename_in_pattern env pat =
            (match pat with
            | Wildcard -> (pat, [])
            | Binder i -> (pat, [(get_id i)])
            | Constructor (c, plist) ->
              let c' = if List.mem env c ~equal:(=) then name ^ "." ^ c else c in
              let (plist', blist) = List.unzip @@ List.map plist ~f:(rename_in_pattern env) in
              let blist' = List.concat blist in
              (Constructor(c', plist'), blist')
            )
          in
          (* Update id being matched *)
          let pv' = check_and_prefix_id env pv in
          (* Get updated pattern and their branches *)
          let pelist' = List.map pelist ~f:(fun (pat, expr) ->
            let pat', binds = rename_in_pattern env pat in
            (* remove all binds from env *)
            let env' = List.filter env ~f:(fun a -> not (List.mem binds a ~equal:(=))) in
            (pat', rename_in_expr expr env')
          ) in
          MatchExpr(pv', pelist'), eloc
        | Builtin (b, idl) ->
          let idl' = List.map idl ~f:(check_and_prefix_id env) in
          (Builtin (b, idl'), eloc)
        | TFun (ti, exp) -> (TFun (ti, rename_in_expr exp env), eloc)
        | TApp (i, tl) ->
          let tl' = List.map tl ~f:(fun t -> rename_in_type t env) in
          (TApp(check_and_prefix_id env i, tl'), eloc)
        | Fixpoint (i, t, e) ->
          let env' = List.filter env ~f:((<>) (get_id i)) in
          let exp' = rename_in_expr e env' in
          (Fixpoint (i, t, exp'), eloc)
        )
      in
      match entry with
      | LibTyp (i, ctrs) ->
        (* from this point, env has "i" and all constructors, to be renamed. *)
        let env' = (get_id i) :: accenv in
        let env'' = List.fold ctrs ~init:env' ~f:(fun envacc ctr ->
          (get_id ctr.cname) :: envacc
        ) in
        let ctrs' = List.map ctrs ~f:(fun ctr ->
            let cname' = check_and_prefix_id env'' ctr.cname in
            let c_arg_types' = List.map ctr.c_arg_types ~f:(fun t -> rename_in_type t env'') in
            {cname = cname'; c_arg_types = c_arg_types'}
        ) in
        let entry' = LibTyp(check_and_prefix_id env'' i, ctrs') in
        (entry' :: accentries, env'')
      | LibVar (i, exp) ->
        (* from this point, env has "i", to be renamed. *)
        let env' = (get_id i) :: accenv in
        let entry' = LibVar (check_and_prefix_id env' i, rename_in_expr exp env') in
        (* we're appending entries in the reverse order. *)
        (entry' :: accentries, env')
    ) in
    { lib with lentries = List.rev rev_entries }


(* Import all libraries in "names" (and their dependences).
 * The order of the returned libraries is an RPO traversal
 * over the dependence graph generated out of "names".
 *)
let import_libs names init_file =
  let rec importer names name_map stack =
    let mapped_names =
      List.map names ~f:(fun (n, namespace) ->
        (match List.Assoc.find name_map ~equal:(=) (get_id n) with
        | Some n' ->
         (* Use a known source location for the mapped id. *)
          (asIdL n' (get_rep n), n, namespace)
        | None -> (n, n, namespace))
      )
    in
    List.fold_left ~f:(fun libacc (name, mapped_name, namespace) ->
      if List.mem stack (get_id name) ~equal:(=) then
        let errmsg = 
          if get_id (mapped_name) = (get_id name) then
            sprintf "Cyclic dependence found when importing %s." (get_id name)
          else 
            sprintf "Cyclic dependence found when importing %s (mapped to %s)."
              (get_id (mapped_name)) (get_id name)
        in
        fatal_error @@ mk_error1 errmsg (get_rep name)
      else
      let (ilib, ilib_import_map) = import_lib name in
      let ilib' = { ilib with libs = namespace_prefix ilib.libs namespace } in
      let ilibs'' = importer ilib'.elibs ilib_import_map ((get_id name) :: stack) in
      let libnode = { libn = ilib'.libs; deps = ilibs'' } in
      (* Order in which we return the list of imported libraries is important. *)
      (libacc @ [libnode])
    ) ~init:[] mapped_names
  in
  let name_map =
    match init_file with
    | Some f -> get_init_extlibs f
    | None -> []
  in
  importer names name_map []

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
          (asId name, None (* no import-as *)) :: names
      else
        names) ~init:[]
  in
  (* Make a list of all libraries and parse them through import_lib above. *)
  let names = List.fold_right ldirs ~f:(fun dir names ->
    let names' = get_lib_list dir in
      List.append names names') ~init:[]
  in
  import_libs names None

type runner_cli = {
  input_file : string;
  stdlib_dirs : string list;
  init_file : string option;
  cf_flag : bool;
  p_contract_info : bool;
}


let parse_cli () =
  let r_stdlib_dir = ref [] in
  let r_input_file = ref "" in
  let r_init_file = ref None in
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
    ("-init", Arg.String (fun x -> r_init_file := Some x), "Path to initialization json");
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
