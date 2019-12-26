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
open Core_bench.Simplified_benchmark
open ScillaUtil.FilePathInfix

let save res ~path =
  let name = Util.sanitize res.Result.benchmark_name in
  let filename = path ^/ name ^. "sexp" in
  let data = res |> Result.sexp_of_t |> Sexp.to_string in
  Out_channel.write_all filename ~data

let load path =
  path
  |> In_channel.read_all
  |> Sexp.of_string
  |> Result.t_of_sexp
