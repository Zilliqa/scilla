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

open Idl

module API (R : RPC) = struct
  open R

  let description = Interface.{
    name = "API";
    namespace = None;
    description = ["This is a functor used to generate clients and servers that follow the JSON RPC protocol"];
    version=(1,0,0);
  }

  let implementation = implement description

  let runner_argv = Param.mk
    ~name:"argv"
    ~description:["Scilla execution parameters"]
    Query.Runner.t

  (* The return value of this JSON-RPC method will be a JSON,
      identical to today’s output JSON emitted by scilla-runner. *)
  let runner_return = Param.mk Rpc.Types.string

  let runner_error = Err.rpc_err

  let runner = declare "runner" ["Execute Scilla"]
    (runner_argv @-> returning runner_return runner_error)
end
