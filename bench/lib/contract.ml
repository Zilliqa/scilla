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
open ScillaUtil.FilePathInfix
open Env

let from_json_file path =
  path
  |> In_channel.read_all
  |> Config_j.contract_of_string

let read ~path name =
  from_json_file @@ path ^/ name ^/ "bench.json"

let read_group group ~env =
  let open Config_j in
  let path = env.benchmarks_dir ^/ Option.value group.path ~default:"" in
  let keep s =
    Sys.is_directory_exn (path ^/ s) &&
    (List.is_empty group.included || List.mem group.included s ~equal:String.equal) &&
    not (List.mem group.excluded s ~equal:String.equal) in
  env.benchmarks_dir
  |> Sys.ls_dir
  |> List.filter ~f:keep
  |> List.map ~f:(read ~path)
