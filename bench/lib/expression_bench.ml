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

let to_exp_name path =
  match Filename.split_extension path with
  | name, Some "scilexp" -> Some name
  | _ -> None

let ls_exp_names path = path |> Sys.ls_dir |> List.filter_map ~f:to_exp_name

let mk (group : expression_group) ~env =
  let cwd = Sys.getcwd () in
  let mk_bench name =
    (* Example:
       ./bin/eval-runner -gaslimit 10000 -libdir src/stdlib tests/eval/exp/good/let.scilexp *)
    let input = cwd ^/ group.path ^/ name ^. "scilexp" in
    let args =
      [
        "-libdir";
        env.stdlib_dir;
        "-gaslimit";
        string_of_int group.gas_limit;
        input;
      ]
    in
    let run () = Eval.run (Some args) ~exe_name:"eval-runner" in
    Bench.Test.create ~name run
  in
  let names =
    (* If there no tests listed explicitly then just load all
       files with ".scilexp" extension from the specified path *)
    match group.tests with [] -> ls_exp_names group.path | names -> names
  in
  let tests = List.map names ~f:mk_bench in
  (* All benchmark names for standalone
     expressions start with the "exp/" prefix *)
  Bench.Test.create_group tests ~name:("exp/" ^ group.name)
