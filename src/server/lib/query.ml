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

module Runner = struct
  (** Holds all the arguments to be provided for the
      Scilla execution. The query parameters are identical to
      the CLI arguments of the scilla-runner executable. *)
  type t = {
    init : string;
    ipc_address : string;
    message : string;
    blockchain : string;
    input : string;
    output : string;
    libdirs : string list;
    gas_limit : int;
    balance: int;
  } [@@deriving rpcty]

  (** Makes the CLI args that could be passed to the scilla-runner. *)
  let to_cli argv = ()
end

(* module Checker = ... *)
