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
open IPCUtil

module IPCIdl (R : RPC) = struct
  open R

  let description =
    Interface.
      {
        name = "IPCIdl";
        namespace = None;
        description =
          [
            "This is a functor used to generate Clients and Servers that \
             follow the json rpc protocol";
          ];
        version = (1, 0, 0);
      }

  let implementation = implement description

  let query = Param.mk ~name:"query" Rpc.Types.string

  let value = Param.mk ~name:"value" Rpc.Types.string

  (* The return value for `fetchStateValue` will be a pair (found : bool, value : string)
   * "value" is valid only if "found && !query.ignoreval" *)
  (* TODO: [@warning "-32"] doesn't seem to work for "unused" types. *)
  type _fetch_ret_t = bool * string [@@deriving rpcty]

  (* defines `typ_of__fetch_ret_t` *)

  let return_fetch =
    Param.mk
      { name = ""; description = [ "(found,value)" ]; ty = typ_of__fetch_ret_t }

  let return_update = Param.mk Rpc.Types.unit

  let fetch_state_value =
    declare "fetchStateValue"
      [ "Fetch state value from blockchain" ]
      (query @-> returning return_fetch RPCError.err)

  let update_state_value =
    declare "updateStateValue"
      [ "Update state value in blockchain" ]
      (query @-> value @-> returning return_update RPCError.err)
end
