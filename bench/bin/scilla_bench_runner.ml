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

open Core
open Core_bench
open Scilla_bench

type callback_bench = params:Tests.params -> env:Env.t -> unit

(** Create a [Command.t] that executes a given callback
    which is passed parameters parsed from the command line *)
let mk_param bench =
  let open Command.Spec in
  let open Command.Let_syntax in
  let re = Arg_type.create Re2.create_exn in
  (* Perform a few iterations for each benchmark by default *)
  let quota = Quota.Num_calls 30 in
  (* Default time per run threshold value (in percentage) *)
  let threshold = 5.0 in
  [%map_open
    let suites = flag "-suite" (listed Suite.arg_type)
        ~doc:"SUITE Type of the benchmark suite to run. \
              [-suite] can be specified multiple times."
    and quota = flag "-quota" (optional_with_default quota Quota.arg_type)
        ~doc:(sprintf "<INT>x|<SPAN> Quota allowed per test. May be a number of runs \
                       (e.g. 1000x or 1e6x) or a time span (e.g. 10s or 500ms). \
                       Default %s."
                (Quota.to_string quota))
    and regex = flag "-matching" (optional re)
        ~doc:"REGEX Run only benchmarks matching the given regex."
    and list = flag "-list" no_arg
        ~doc:"List benchmark names without running them."
    and save = flag "-save" (optional_with_default true bool)
        ~doc:" Save benchmark results."
    and display = flag "-display" (optional_with_default true bool)
        ~doc:" Display benchmark results."
    and compare = flag "-compare" (optional_with_default true bool)
        ~doc:" Compare benchmark results and output the difference."
    and timestamp = flag "-timestamp" (optional string)
        ~doc:" Timestamp of benchmark results to compare with. If not given, \
              the latest (previous) results will be used for comparison."
    and threshold = flag "-threshold" (optional_with_default threshold float)
        ~doc: " Time per run delta threshold value (in percentage)."
    and ci = flag "-ci" no_arg
        ~doc:" Exit with non-zero code if any time per run delta exceeds the threshold."
    and sock_addr = flag "-ipcaddress" (optional string)
        ~doc:"SOCKET Address for IPC communication with blockchain for state access."
    in
    fun () ->
      (* Run all benchmark suites in case nothing is selected *)
      let suites = match suites with
        | [] -> Suite.all
        | ss -> ss in
      let env = Env.mk ~sock_addr in
      let params = Tests.make_params
          ~suites ~quota ?regex ~list
          ~save ~display ~compare
          ~threshold ~ci ?timestamp ()
      in
      bench ~params ~env
  ]

let mk_command bench =
  (* Since we don't want to expose all of the core_bench options,
     here we use the [Command.basic] directly *)
  Command.basic
    ~summary:"Run Scilla benchmarks"
    (mk_param bench)

let bench ~params ~env =
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
  |> mk_command
  |> Command.run
