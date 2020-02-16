open Core
open Idl

(* This error matches for the error returned by jsonrpccpp *)
module RPCError = struct
  type err_t = { code : int; message : string } [@@deriving rpcty]

  (* defines `typ_of_err_t` *)

  exception RPCErrorExn of err_t

  let err =
    Error.
      {
        def = err_t;
        raiser = (function e -> raise (RPCErrorExn e));
        matcher = (function RPCErrorExn e -> Some e | _ -> None);
      }

  let rpc_of_t t = Rpcmarshal.marshal typ_of_err_t t
end

(** Send msg via output channel [oc] with a delimiting character "0xA". *)
let send_delimited oc msg =
  let msg' = msg ^ "\n" in
  Out_channel.output_string oc msg';
  Out_channel.flush oc
