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
open Syntax
open TypeUtil

(*****************************************************************)
(*                    Library type caching                       *)
(*****************************************************************)

module StdlibTypeCacher (Q : MakeTEnvFunctor) (R : QualifiedTypes) = struct

  module MakeTEnv = Q(R)
  open MakeTEnv

  type t = MakeTEnv.TEnv.t

  let to_json_string lib_name lib_entries =
    (* Let's output to a JSON with the following format:
     * {
     *   "name" : "foo",
     *   "entries" : [
     *      { "name" : "foo1", "type" "Int32", "loc" : "" },
     *      { "name" : "foo2", "type" : "Int32 -> Int64", "loc", ""}
     *    ]
     * }
     *)
     let entries = 
      List.map ~f:(fun (n, t) ->
        let name_s = n in
        let loc_s = get_loc_str (rr_loc t) in
        let type_s = rr_pp t in
        `Assoc [ 
          ("name", `String name_s);
          ("type", `String type_s);
          ("loc", `String loc_s)
        ]
      ) lib_entries in
      (* Compose final result. *)
      `Assoc [
        ("name", `String lib_name);
        ("entries", `List entries)
      ]

  let parse_json j =
    let open Yojson.Basic.Util in
    let name = member "name" j in
    let entries = member "entries" j in 
    match name, entries with
    | `String _, `List elj ->
      (* Conver the list of JSONs to a list of TEnv.tenv entries. *)
      let el = List.fold_right ~f:(fun ej acc ->
        let name_j, type_j, loc_j = 
          member "name" ej, member "type" ej, member "loc" ej in
        let e_o = 
          (match name_j, type_j, loc_j with
          | `String name_s, `String type_s, `String _ ->
              (* Printf.printf "Parsing type: %s\n" type_s; *)
              (try
                let typ = FrontEndParser.parse_type type_s in
                let loc = dummy_loc in (* TODO: parse loc_s *)
                let id = asIdL name_s loc in
                Some (id, typ)
               with
               | _ -> None
              )
          | _ -> (* TODO: report useful error messages. *)
            None
          ) in
        (* Accummulate None or the list of entries. *)
        (match e_o, acc with
        | Some e, Some l -> Some (e::l)
        | _ -> None)
      ) ~init:(Some []) elj in
        el
    | _, _ -> None


  (* Get type info for "lib" from cache, if it exists. *)
  let get_lib_tenv_cache (tenv : t) lib =
    let lib_name = get_id lib.lname in
    let open GlobalConfig.StdlibTracker in
    let dir_o = find_lib_dir lib_name in
    match dir_o with
    | Some dir ->
      (* See if file lib_name.json exists and load from it. *)
      let file_name = (dir ^ Filename.dir_sep ^ lib_name ^ ".json") in
      if Caml.Sys.file_exists file_name then
        let j = Yojson.Basic.from_file file_name in
        let entries_opt = parse_json j in
        match entries_opt with
        | Some entries ->
          Some (TEnv.addTs (TEnv.copy tenv) entries)
        | None ->
          (* Error parsing JSON. TODO: print to logger. *)
          None
      else
        None
    | None -> None

  (* Store type info tenv, for "lib" in the cache. *)
  let cache_lib_tenv (tenv : t) lib =
    (* 1. Carefully separate out only lib's entries from tenv *)
    let entry_names =
      List.map ~f:(fun {lname;_} -> (get_id lname)) lib.lentries
    in
    (* OCaml's List.mem is not according to online docs. Why? *)
    let list_mem l a = List.exists l ~f:(fun b -> b = a) in
    let lib_entries = List.filter ~f:(fun e -> list_mem entry_names (fst e)) (TEnv.to_list tenv) in

    (* 2. Write back to cache. *)
    let open GlobalConfig.StdlibTracker in
    let lib_name = get_id lib.lname in
    let dir_o = find_lib_dir lib_name in
    match dir_o with
    | Some dir ->
      let j = to_json_string lib_name lib_entries in
      let js = Yojson.pretty_to_string j in
      Out_channel.with_file (dir ^ Filename.dir_sep ^ lib_name ^ ".json")
       ~f:(fun channel -> js |> Out_channel.output_string channel)
    | None ->
      (* TODO: add log to DebugMessage.plog. *)
      ()

end