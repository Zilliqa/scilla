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
open ScillaUtil
open Env

let from_json_file path =
  path
  |> In_channel.read_all
  |> Spec_j.spec_of_string

let read ~env dir =
  let open FilePathInfix in
  let dir = env.contracts_dir ^/ dir in
  from_json_file @@ dir ^/ "bench.json"

let read_all env =
  env.contracts_dir
  |> Sys.ls_dir
  (* |> List.filter ~f:Sys.is_directory_exn *)
  |> List.map ~f:(read ~env)
