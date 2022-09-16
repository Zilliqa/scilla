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
open Scilla_base
open Identifier
open Literal

type json_info = {
  file : string;  (** Path to [init.json] *)
  contract_name : string;  (** Name of the appropriate contract *)
}

(** The ScillaJSONMerger module allows the user to merge multiple [init.json]
    files that contains initialization options for contracts. *)
module ScillaJSONMerger = struct
  module StringSet = Set.Make (String)

  (** Name of the currently merged contract. *)
  let g_contract_name = ref ""

  (** Merge configuration file with replacements information.
      It has the following format:
        contract_name |-> (line_num |-> (replacee |-> replacement)) *)
  let g_config = ref @@ Map.empty (module String)

  (** Parses merge config to the map in the following format: vname |-> value *)
  let parse_json_merge_config = function
    | None -> Map.empty (module String)
    | Some (config : Config.config) ->
        List.fold_left config.json_replacements
          ~init:(Map.empty (module String))
          ~f:(fun m r -> Map.set m ~key:r.vname ~data:r.value)

  (** Sets [Config.config] as a global configuration for the merge. *)
  let set_merge_config (c : Config.config option) =
    parse_json_merge_config c |> fun c -> g_config := c

  (** Set a global contract name based on the given [name]. *)
  let set_contract_name name = g_contract_name := Util.get_contract_name name

  (** Generates a conflict name and emits a warning. *)
  let set_conflict_vvalue vvalues vname =
    ErrorUtils.warn0
      (Printf.sprintf
         "Name collision: Please specify the value of `%s` in the \
          configuration file"
         vname)
      Util.disambiguate_warning_level;
    Set.to_list vvalues
    |> List.sort ~compare:String.compare
    |> String.concat ~sep:"|" |> Printf.sprintf "(%s)"

  (** Renames [vname] started with [_]. Its values must be the same in all the
      contracts or explicitly specified in the configuration file. *)
  let rename_special_vname renames_map vname vvalue =
    let has_single_vvalue s =
      (phys_equal 1 @@ Set.length s) && String.equal (Set.min_elt_exn s) vvalue
    in
    let has_replacement_in_config () = Map.mem !g_config vname in
    match Map.find renames_map vname with
    | Some vvalues when has_single_vvalue vvalues -> (renames_map, vvalue)
    | Some _vvalues when has_replacement_in_config () ->
        (renames_map, Map.find_exn !g_config vname)
    | Some vvalues ->
        let vvalues' = StringSet.add vvalues vvalue in
        let vvalue' = set_conflict_vvalue vvalues' vname in
        let renames_map' = Map.set renames_map ~key:vname ~data:vvalues' in
        (renames_map', vvalue')
    | None ->
        let renames_map' =
          Map.set renames_map ~key:vname ~data:(StringSet.singleton vvalue)
        in
        (renames_map', vvalue)

  (** Renames [vname] entry from the JSON file. *)
  let rename_vname renames_map vname vvalue =
    match vname |> String.to_list with
    | [] -> (vname, renames_map, vvalue)
    | '_' :: _ ->
        (* Identifiers started with '_' are special and should have the same
           value in all merged contracts. *)
        let renames_map', vvalue' =
          rename_special_vname renames_map vname vvalue
        in
        (vname, renames_map', vvalue')
    | _ -> (Printf.sprintf "%s_%s" !g_contract_name vname, renames_map, vvalue)

  (** Renames [ty] entry from the JSON file. *)
  let rename_ty (ty : JSON.JSONType.t) = ty

  (** Merges JSON files to a single JSON. *)
  let merge_jsons (files : json_info list) =
    List.fold_left files
      ~init:([], Map.empty (module String))
      ~f:(fun (acc, renames_map) ji ->
        set_contract_name ji.contract_name;
        let contract_values, _external_values =
          JSON.ContractState.get_json_data ji.file
        in
        let contract_values', renames_map' =
          List.fold_left contract_values ~init:([], renames_map)
            ~f:(fun (acc, renames_map) (vname, ty, value) ->
              let vvalue = PrettyPrinters.pp_literal_json value in
              let name, renames_map', vvalue' =
                rename_vname renames_map vname vvalue
              in
              let vname' = (GlobalName.SimpleGlobal name, name) in
              let value' = GlobalLiteral.StringLit vvalue' in
              let ty' = rename_ty ty in
              (acc @ [ (vname', ty', value') ], renames_map'))
        in
        (acc @ contract_values', renames_map'))
    |> fun (values, _) -> JSON.ContractState.state_to_string values

  let run (config : Config.config option) (files : json_info list) =
    set_merge_config config;
    let json = merge_jsons files in
    ( json,
      ErrorUtils.get_warnings () |> PrettyPrinters.scilla_warning_to_sstring )
end
