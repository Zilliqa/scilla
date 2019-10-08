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

module Info = struct
  type t = {
    name : string;
    index : int;
    init : string;
    state : string;
    blockchain : string;
    message : string;
    output : string;
  }

  let mk group (contract : contract) (index : int) (tr : transition) ~env =
    let subpath = Option.value group.path ~default:"" in
    let dir = env.benchmarks_dir ^/ subpath ^/ contract.name in
    let mk_path name def fallback =
      let spec_path = Option.map def ~f:(fun f -> dir ^/ f) in
      let tr_path = Option.map name ~f:(fun f -> dir ^/ "transitions" ^/ f) in
      let path = Option.first_some tr_path spec_path in
      Option.value path ~default:(dir ^/ fallback) in

    let init = mk_path tr.init contract.init "init.json" in
    let state = mk_path tr.state contract.state "state.json" in
    let blockchain = mk_path tr.blockchain contract.blockchain "blockchain.json" in
    let message =
      tr.message
      |> Option.value ~default:(tr.name ^. "json")
      |> (fun f -> dir ^/ "transitions" ^/ f) in
    let istr = string_of_int index in
    let output = env.tmp_dir ^/ tr.name ^ "_output_" ^ istr ^. "json" in
    { name = tr.name; index; init; state; blockchain; message; output }
end

let mk index tr ~contract ~group ~env =
  let info = Info.mk ~env group contract index tr in
  let subpath = Option.value group.path ~default:"" in
  let input = env.benchmarks_dir ^/ subpath ^/ contract.name ^/ contract.input in
  let base_args =
    [ "-init"; info.init;
      "-i"; input;
      "-libdir"; env.stdlib_dir;
      "-gaslimit"; string_of_int contract.gas_limit;
      "-imessage"; info.message;
      "-jsonerrors";
      "-iblockchain"; info.blockchain;
      "-o"; info.output
    ] in
  let extra_args =
    match env.state_mode with
    | StateService.Local -> ["-istate"; info.state]
    | StateService.IPC sock_addr ->
        if contract.ipc then
          (* Initialize IPC state server *)
          let balance = StateIPCTest.setup_and_initialize
              ~start_mock_server:false
              ~sock_addr
              ~state_json_path:info.state in
          ["-ipcaddress"; sock_addr; "-balance"; balance]
        else
          ["-istate"; info.state]
  in
  let prog = env.bin_dir ^/ "scilla-runner" in
  (* Get a CLI args to run the given transition benchmark *)
  let args = base_args @ extra_args in
  let run () = Runner.exec ~prog ~args in
  Bench.Test.create ~name:info.name run
