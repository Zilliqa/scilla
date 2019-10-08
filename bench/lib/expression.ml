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
open Core_bench
open ScillaUtil.FilePathInfix
open Env
open Config_t

let mk (group : expression_group) ~env =
  let cwd = Sys.getcwd () in
  let mk_bench name =
    (* Example: ./bin/eval-runner -libdir src/stdlib tests/eval/exp/good/let.scilexp *)
    let input = cwd ^/ group.path ^/ name ^. "scilexp" in
    let prog = env.bin_dir ^/ "eval-runner" in
    let args = [
      "-libdir"; env.stdlib_dir;
      "-gaslimit"; string_of_int group.gas_limit;
      input;
    ] in
    let run () = Runner.exec ~prog ~args in
    Bench.Test.create ~name run
  in
  group.tests
  |> List.map ~f:mk_bench
  |> Bench.Test.create_group ~name:("exp/" ^ group.name)
