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
open Scilla_bench

(* The basic idea is to have 3 kinds of benchmarks:
   - Benchmarks for standalone closed expressions
   - Contract (and transition) benchmarks --
     high-level performance regression tests that invoke the scilla-runner executable.
   - Internal benchmarks for performance-critical modules --
     those are inline micro-benchmarks written using ppx_let and core_bench that
     have little or no effect on the execution or module initialization time.

   For the contract benchmarks we have 2 modes:
   - IPC -- uses external server for state management
   - Local -- full state provided as an input to the scilla-runner

   Benchmarks are described using the simple JSON format and organized as follows:
   - Standalone (expression) and contract benchmarks are listed in the config.json file
   - Each contract benchmark is kept in a separate directory which contains the bench.json file
     that serves as a benchmark specification (see the config.atd for details)
   - Transitions should be placed in the "transitions" directory inside
     the corresponding benchmark directory. See the "ipc" benchmark for example.
*)

let bench ~params ~env =
  let open Params in
  (* Prepare the environment and load tests *)
  let cfg = Config.read env in
  (* Load contract and expression benchmarks *)
  let tests = Tests.load ~params ~cfg ~env in
  (* Run the benchmarks or just list them *)
  if params.list
  then Tests.list tests
  else Tests.exec tests ~params ~env

let () =
  bench
  |> Bench_command.mk
  |> Command.run
