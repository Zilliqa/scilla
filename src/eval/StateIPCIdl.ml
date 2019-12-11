open Idl

(* This error matches for the error returned by jsonrpccpp *)
module RPCError = struct
  type err_t = { code : int ; message : string } [@@deriving rpcty] (* defines `typ_of_err_t` *)
  exception RPCErrorExn of err_t

  let err = Error.{
      def = err_t;
      raiser = (function | e -> raise (RPCErrorExn (e)));
      matcher = (function | RPCErrorExn e -> Some e | _ -> None)
    }
end

module IPCIdl(R: RPC) = struct
  open R

  let description = Interface.{
    name = "IPCIdl";
    namespace = None;
    description = ["This is a functor used to generate Clients and Servers that follow the json rpc protocol" ];
    version=(1,0,0);
  }
  
  let implementation = implement description

  let query = Param.mk ~name: "query" Rpc.Types.string
  let value = Param.mk ~name: "value" Rpc.Types.string
  (* The return value for `fetchStateValue` will be a pair (found : bool, value : string)
   * "value" is valid only if "found && !query.ignoreval" *)
  (* TODO: [@warning "-32"] doesn't seem to work for "unused" types. *)
  type _fetch_ret_t = (bool * string) [@@deriving rpcty] (* defines `typ_of__fetch_ret_t` *)
  let return_fetch = Param.mk { name = ""; description = ["(found,value)"]; ty = typ_of__fetch_ret_t }
  let return_update = Param.mk Rpc.Types.unit

  let fetch_state_value = declare "fetchStateValue" ["Fetch state value from blockchain"]
    (query @-> returning return_fetch RPCError.err)
  let update_state_value = declare "updateStateValue" ["Update state value in blockchain"]
    (query @-> value @-> returning return_update RPCError.err)
end