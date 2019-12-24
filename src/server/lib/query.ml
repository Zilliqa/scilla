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

module Runner = struct
  (** Holds all the arguments to be provided for the
      Scilla execution. The query parameters are identical to
      the CLI arguments of the scilla-runner executable. *)
  type t = {
    init : string [@key "-init"];
    state : string option [@key "-istate"];
    ipc_address : string option [@key "-ipcaddress"];
    message : string [@key "-imessage"];
    blockchain : string [@key "-iblockchain"];
    output : string option [@key "-o"];
    input : string [@key "-i"];
    libdir : string [@key "-libdir"];
    gas_limit : string [@key "-gaslimit"];
    balance: string [@key "-balance"];
  } [@@deriving rpcty, show]

  (** Makes the [Runner.args] that could be
      passed to the [Runner.run] function. *)
  let to_cli_args argv =
    let open Runner in
    let libdirs =
      if argv.libdir = ""
      then []
      else Str.split (Str.regexp "[;:]") argv.libdir in
    { input_init = argv.init;
      input_state = Option.value argv.state ~default:"";
      input_message = argv.message;
      input_blockchain = argv.blockchain;
      output = Option.value argv.output ~default:"";
      input = argv.input;
      libdirs;
      gas_limit = Stdint.Uint64.of_string argv.gas_limit;
      balance = Stdint.Uint128.of_string argv.balance;
      ipc_address = Option.value argv.ipc_address ~default:"";
      pp_json = false;
    }
end

(* module Checker = ... *)
