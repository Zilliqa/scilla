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
