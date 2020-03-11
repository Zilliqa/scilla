open Core_kernel
open! Int.Replace_polymorphic_compare
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

  let addr = Param.mk ~name:"addr" Rpc.Types.string

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

  let update_state_value =
    declare "updateStateValue"
      [ "Update state value in blockchain" ]
      (query @-> value @-> returning return_update RPCError.err)
end
