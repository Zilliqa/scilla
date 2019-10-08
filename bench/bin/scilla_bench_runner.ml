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
open ScillaUtil
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

type ctx =
  { regex: Re2.t option;
    list: bool;
    sock_addr: string option;
    analysis_configs: Bench.Analysis_config.t list;
    display_config: Bench.Display_config.t;
    save_to_file: (Bench.Measurement.t -> string) option;
    run_config: Bench.Run_config.t;
  }

let mk_contract_benchmarks ~env ~cfg =
  let open Config_t in
  (* Create a benchmark group given the [contract] and [group] *)
  let mk_bench_group contract ~group =
    contract.transitions
    |> List.mapi ~f:(Transition.mk ~contract ~group ~env)
    |> Bench.Test.create_group ~name:("contract/" ^ group.name ^ "/" ^ contract.name)
  in
  (* Create a benchmark group out of
     the given [group] of benchmark contracts *)
  let to_bench_group group =
    group
    |> Contract.read_group ~env
    |> List.map ~f:(mk_bench_group ~group)
  in
  List.concat_map cfg.contracts ~f:to_bench_group

let run ctx =
  let env = Env.mk ~sock_addr:ctx.sock_addr in
  let cfg = Config.read env in
  let expressions = List.map cfg.expressions ~f:(Expression.mk ~env) in
  let contracts = mk_contract_benchmarks ~env ~cfg in
  Bench.bench
    ~run_config:ctx.run_config
    ~analysis_configs:ctx.analysis_configs
    ~display_config:ctx.display_config
    ?save_to_file:ctx.save_to_file
    (expressions @ contracts)

let command =
  let open Command.Spec in
  let open Command.Let_syntax in
  let re = Arg_type.create Re2.create_exn in
  Bench.make_command_ext
    ~summary:"Run Scilla benchmarks."
    [%map_open
      let regex = flag "-matching" (optional re) ~doc:"REGEX Select benchmarks matching given regex."
      and list = flag "-list" no_arg ~doc:"List benchmark names without running them"
      and sock_addr = flag "-ipcaddress" (optional string) ~doc:"Socket address for IPC communication with blockchain for state access"
      in fun (analysis_configs, display_config, mode) -> (
          match mode with
          | `From_file _ ->
              failwith "This executable is for running benchmarks, not analyzing saved measurements."
          | `Run (save_to_file, run_config) ->
              let ctx =
                { regex; list; sock_addr;
                  analysis_configs; display_config;
                  save_to_file; run_config;
                } in
              run ctx
        )
    ]

let () = Command.run command
