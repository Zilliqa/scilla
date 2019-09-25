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
open Spec_t
open ScillaUtil
open FilePathInfix
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

  let mk (spec : spec) (tr : transition) index ~env =
    let istr = string_of_int index in
    let dir = env.contracts_dir ^/ spec.name in
    let mk_path name def fallback =
      let open Option in
      let spec_path = map def ~f:(fun f -> dir ^/ f) in
      let tr_path = map name ~f:(fun f -> dir ^/ "transitions" ^/ f) in
      let path = first_some tr_path spec_path in
      value path ~default:(dir ^/ fallback) in
    let init = mk_path tr.init spec.init "init.json" in
    let state = mk_path tr.state spec.state "state.json" in
    let blockchain = mk_path tr.blockchain spec.blockchain "blockchain.json" in
    let message =
      tr.message
      |> Option.value ~default:(tr.name ^. "json")
      |> (fun f -> dir ^/ "transitions" ^/ f) in
    let output = env.tmp_dir ^/ tr.name ^ "_output_" ^ istr ^. "json" in
    { name = tr.name; index; init; state; blockchain; message; output }
end

let mk ~env (spec : spec) (tr : transition) index =
  let info = Info.mk ~env spec tr index in
  let input = env.contracts_dir ^/ spec.name ^/ spec.input in
  let base_args =
    [ "-init"; info.init;
      "-i"; input;
      "-libdir"; env.stdlib_dir;
      "-gaslimit"; string_of_int spec.gas_limit;
      "-imessage"; info.message;
      "-jsonerrors";
      "-iblockchain"; info.blockchain;
      "-o"; info.output
    ] in
  let extra_args =
    match spec.state_mode with
    | `Local ->
        ["-istate"; info.state]
    | `IPC ->
        (* Initialize IPC state server *)
        let balance = StateIPCTest.setup_and_initialize
            ~start_mock_server:false
            ~sock_addr:env.sock_addr
            ~state_json_path:info.state in
        ["-ipcaddress"; env.sock_addr; "-balance"; balance] in
  let prog = env.bin_dir ^/ "scilla-runner" in
  (* Get a CLI args to run the given transition benchmark *)
  let args = base_args @ extra_args in
  (* print_endline @@ String.concat ~sep:" " args; *)
  let run () =
    let pi = Unix.create_process ~prog ~args in
    Unix.waitpid_exn pi.pid in
  Bench.Test.create ~name:info.name run
