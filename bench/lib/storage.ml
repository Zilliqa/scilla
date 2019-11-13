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

let mk_path dir ~env =
  env.Env.results_dir ^/ dir

let only_dirs ~env =
  List.filter ~f:(fun dir -> Sys.is_directory_exn @@ mk_path dir ~env)

let ls ~env =
  env.Env.results_dir
  |> Sys.ls_dir
  |> Timestamp.sort_desc
  |> only_dirs ~env

let latest ~timestamp ~current ~env =
  (* List paths containing benchmark results *)
  let paths = ls ~env in
  (* Helper function to find a dir with
     the latest (previous) benchmark results *)
  let find_latest () =
    let paths = match current with
    | None -> paths
    | Some s -> List.filter paths ~f:(fun dir -> dir <> s)
    in List.hd paths
  in match timestamp with
  | None -> find_latest ()
  | Some s -> List.find paths ~f:(fun p -> p = s)
