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

(** Warning level used for rename conflicts. *)
let disambiguate_warning_level = 2

(** Generates an unique contract name based on the given [basename].
    This is required to merge contracts with the same name. *)
let get_contract_name =
  let visited_contracts = ref @@ Map.empty (module String) in
  fun basename ->
    match Map.find !visited_contracts basename with
    | Some next_cnt ->
        visited_contracts :=
          Map.set !visited_contracts ~key:basename ~data:(next_cnt + 1);
        Printf.sprintf "%s_%d" basename next_cnt
    | None ->
        visited_contracts := Map.set !visited_contracts ~key:basename ~data:0;
        basename
