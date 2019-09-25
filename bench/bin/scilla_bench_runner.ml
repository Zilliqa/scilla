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
open Scilla_bench

(* The basic idea is to have two kinds of benchmarks:
   - Contract (and transition) benchmarks --
     high-level performance regression tests that invoke the scilla-runner executable.
   - Internal benchmarks for performance-critical modules --
     those are inline micro-benchmarks written using ppx_let and core_bench that
     have little or no effect on the execution or module initialization time.

   For the contract benchmarks we have 2 modes:
   - IPC -- uses external server for state management
   - Local -- full state provided as an input to the scilla-runner

   Contract benchmarks are described using the
   simple json format and organized as follows:
   - Each benchmark is kept in a separate directory which contains the bench.json file
     that serves as a benchmark specification (see the spec.atd for details)
   - Transitions should be placed in the "transitions" directory inside
     the corresponding benchmark directory

   See the "ipc" benchmark for example.
*)

let () =
  let open Core_bench in
  let open Spec_t in
  (* TODO: Get sock_addr from CLI somehow *)
  let sock_addr = "/home/vyorkin/zilliqa.sock" in
  let env = Env.mk sock_addr in
  let specs = Spec.read_all env in
  let spec = List.hd_exn specs in
  let benchmarks = List.mapi
      spec.transitions
      ~f:(fun i tr -> Transition.mk ~env spec tr i) in
  let command = Bench.make_command benchmarks in
  Command.run command
