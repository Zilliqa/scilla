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

type callback_bench = params:Params.t -> env:Env.t -> unit

(* Create a command line parameters specification *)
let mk_param bench =
  let open Command.Spec in
  let open Command.Let_syntax in
  let re = Arg_type.create Re2.create_exn in
  [%map_open
    let suites = flag "-suite" (listed Suite.arg_type)
        ~doc:"SUITE Type of the benchmark suite to run. \
              [-suite] can be specified multiple times."
    and quota = flag "-quota"
        (optional_with_default Defaults.time_quota_float float)
        ~doc:(sprintf
                "SECS Time quota allowed per test (default %s)."
                (Time.Span.to_string Defaults.time_quota))
    and regex = flag "-matching" (optional re)
        ~doc:"REGEX Run only benchmarks matching the given regex."
    and list = flag "-list" no_arg
        ~doc:"List benchmark names without running them."
    and save = flag "-save" (optional_with_default Defaults.save bool)
        ~doc:" Save benchmark data."
    and display = flag "-display" (optional_with_default Defaults.display bool)
        ~doc:" Display benchmarking results."
    and compare = flag "-compare" (optional_with_default Defaults.compare bool)
        ~doc:" Compare benchmarking results and output the difference."
    and timestamp = flag "-timestamp" (optional string)
        ~doc:" Timestamp of measurements to compare with. If not given, \
              the latest (previous) measurements will be used for comparison."
    and sock_addr = flag "-ipcaddress" (optional string)
        ~doc:"SOCKET Address for IPC communication \
              with blockchain for state access."
    in
    fun () ->
      (* Run all benchmark suites in case nothing is selected *)
      let suites = match suites with
        | [] -> Suite.all
        | ss -> ss in
      let env = Env.mk ~sock_addr:sock_addr in
      let params = Params.mk
          ~suites ~quota ~regex ~list
          ~save ~display ~compare ~timestamp
      in
      bench ~params ~env
  ]

let mk bench =
  (* Since we don't want to expose all of the core_bench options,
     here we use the [Command.basic] directly *)
  Command.basic
    ~summary:"Run Scilla benchmarks"
    (mk_param bench)
