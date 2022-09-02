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

(** The ScillaJSONProduct module allows the user to merge multiple
    [init.json] files that contains initialization options for
    contracts. *)
module ScillaJSONProduct = struct
  (** Name of the currently merged contract. *)
  let g_contract_name = ref ""

  (** Product configuration file with replacements information.
      It has the following format:
        contract_name |-> (line_num |-> (replacee |-> replacement)) *)
  let g_config = ref @@ Map.empty (module String)

  (** Sets [Config.config] as a global configuration for the product. *)
  let set_product_config (c : Config.config option) =
    Util.parse_product_config c |> fun c -> g_config := c

  (** Set global contract name based on the given [name]. *)
  let set_contract_name name = g_contract_name := Util.get_contract_name name

  (** Renames [vname] entry from the JSON file. *)
  let rename_vname vname =
    match vname |> String.to_list with
    | [] | '_' :: _ ->
        (* Identifiers started with '_' are special and should not be
           renamed. *)
        vname
    | _ -> Printf.sprintf "%s_%s" !g_contract_name vname

  (** Renames [ty] entry from the JSON file. *)
  let rename_ty (ty : JSON.JSONType.t) = ty

  (** Renames [value] entry from the JSON file. *)
  let rename_value lit : GlobalLiteral.t = lit

  (** Merges JSON files to a single JSON. *)
  let merge_jsons (files : json_info list) =
    List.fold_left files ~init:[] ~f:(fun acc ji ->
        set_contract_name ji.contract_name;
        let contract_values, _external_values =
          JSON.ContractState.get_json_data ji.file
        in
        let contract_values' =
          List.fold_left contract_values ~init:[]
            ~f:(fun acc (vname, ty, value) ->
              let name = rename_vname vname in
              let vname' = (GlobalName.SimpleGlobal name, name) in
              let ty' = rename_ty ty in
              let value' = rename_value value in
              acc @ [ (vname', ty', value') ])
        in
        acc @ contract_values')
    |> JSON.ContractState.state_to_string

  let run (config : Config.config option) (files : json_info list) =
    set_product_config config;
    ( merge_jsons files,
      ErrorUtils.get_warnings () |> PrettyPrinters.scilla_warning_to_sstring )
end
