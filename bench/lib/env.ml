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

open ScillaUtil

type t =
  { bin_dir : string;
    stdlib_dir : string;
    benchmarks_dir : string;
    tmp_dir : string;
    state_mode : StateService.service_mode;
  }

let mk ~sock_addr =
  let open FilePathInfix in
  let cwd = Sys.getcwd () in
  let bin_dir = cwd ^/ "bin" in
  let benchmarks_dir = cwd ^/ "bench" ^/ "benchmarks" in
  let tmp_dir = Filename.get_temp_dir_name () in
  let stdlib_dir = cwd ^/ "src" ^/ "stdlib" in
  let state_mode = match sock_addr with
    | Some addr -> StateService.IPC addr
    | None -> StateService.Local
  in { bin_dir; benchmarks_dir; tmp_dir;
       stdlib_dir; state_mode
     }
