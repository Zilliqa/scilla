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
open Config_t
open ScillaUtil.FilePathInfix
open Env

module Args = struct
  type t = {
    index : int;
    init : string;
    state : string;
    blockchain : string;
    message : string;
    gas_limit: string;
    lib_dir : string;
    input : string;
    output : string;
  }

  let mk index (tr : transition) ~(contract : contract) ~group ~env =
    let subpath = Option.value group.path ~default:"" in
    let dir = env.benchmarks_dir ^/  subpath ^/ contract.name  in
    let input = dir ^/ contract.input in
    let mk_path name def fallback =
      let open Option in
      let spec_path = map def ~f:(fun f -> dir ^/ f) in
      let tr_path = map name ~f:(fun f -> dir ^/ "transitions" ^/ f) in
      let file_path = first_some tr_path spec_path in
      value file_path ~default:(dir ^/ fallback) in
    let init = mk_path tr.init contract.init "init.json" in
    let state = mk_path tr.state contract.state "state.json" in
    let blockchain = mk_path tr.blockchain contract.blockchain "blockchain.json" in
    let message =
      tr.message
      |> Option.value ~default:(tr.name ^. "json")
      |> (fun f -> dir ^/ "transitions" ^/ f) in
    let gas_limit = string_of_int contract.gas_limit in
    let istr = string_of_int index in
    let output = env.tmp_dir ^/ tr.name ^ "_output_" ^ istr ^. "json" in
    { lib_dir = env.stdlib_dir;
      index; init; state;
      blockchain; message; gas_limit;
      input; output
    }

  let to_list args =
    [ "-init"; args.init;
      "-i"; args.input;
      "-libdir"; args.lib_dir;
      "-gaslimit"; args.gas_limit;
      "-imessage"; args.message;
      "-jsonerrors";
      "-iblockchain"; args.blockchain;
      "-o"; args.output
    ]
end

let mk index tr ~contract ~group ~env =
  (* Prepare a CLI args to run the given transition benchmark *)
  let args = Args.mk index tr ~contract ~group ~env in
  let base_args = Args.to_list args in
  let extra_args =
    match env.state_mode with
    | StateService.Local -> ["-istate"; args.state]
    | StateService.IPC sock_addr ->
        if contract.ipc then
          (* Initialize IPC state server *)
          let balance = StateIPCTest.setup_and_initialize
              ~start_mock_server:false
              ~sock_addr
              ~state_json_path:args.state in
          ["-ipcaddress"; sock_addr; "-balance"; balance]
        else
          ["-istate"; args.state]
  in
  let run () = Runner.exec
      ~prog:(env.bin_dir ^/ "scilla-runner")
      ~args:(base_args @ extra_args) in
  Bench.Test.create ~name:tr.name run
