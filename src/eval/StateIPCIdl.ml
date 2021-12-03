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

  let scilla_type = Param.mk ~name:"scilla_type" Rpc.Types.string

  let addr = Param.mk ~name:"addr" Rpc.Types.string

  let query_name = Param.mk ~name:"query_name" Rpc.Types.string

  let query_args = Param.mk ~name:"query_args" Rpc.Types.string

  (* The return value for `fetchStateValue` will be a pair (found : bool, value : string)
   * "value" is valid only if "found && !query.ignoreval" *)
  (* TODO: [@warning "-32"] doesn't seem to work for "unused" types. *)
  type _fetch_ret_t = bool * string [@@deriving rpcty]

  (* The return value for `fetchExternalStateValue will be a triple
   * (found : bool, value : string, type : string). "value" is valid only
   * if "found && !query.ignoreval" *)
  type _fetch_ext_ret_t = bool * string * string [@@deriving rpcty]

  (* defines `typ_of__fetch_ret_t` *)

  let return_fetch =
    Param.mk
      { name = ""; description = [ "(found,value)" ]; ty = typ_of__fetch_ret_t }

  let return_ext_fetch =
    Param.mk
      {
        name = "";
        description = [ "(found,value,type)" ];
        ty = typ_of__fetch_ext_ret_t;
      }

  let return_update = Param.mk Rpc.Types.unit

  let fetch_state_value =
    declare "fetchStateValue"
      [ "Fetch state value from blockchain" ]
      (query @-> returning return_fetch RPCError.err)

  let fetch_ext_state_value =
    declare "fetchExternalStateValue"
      [ "Fetch state value of another contract from the blockchain" ]
      (addr @-> query @-> returning return_ext_fetch RPCError.err)

  let fetch_bcinfo =
    declare "fetchBlockchainInfo"
      [ "Fetch various information about the current blockchain state" ]
      (query_name @-> query_args @-> returning return_fetch RPCError.err)

  let set_bcinfo =
    declare "setBlockchainInfo" [ "Set blockchain info" ]
      (query_name @-> query_args @-> value
      @-> returning return_update RPCError.err)

  (* This is a utility to test the testsuite server with JSON data.
   * It isn't part of the Zilliqa<->Scilla IPC protocol. *)
  let set_ext_state_value =
    declare "setExternalStateValue"
      [
        "Set state value and field type of another contract from the blockchain";
      ]
      (addr @-> query @-> value @-> scilla_type
      @-> returning return_update RPCError.err)

  let update_state_value =
    declare "updateStateValue"
      [ "Update state value in blockchain" ]
      (query @-> value @-> returning return_update RPCError.err)
end
