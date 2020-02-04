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
open Api
open DebugMessage

module U = Unix
module M = Idl.IdM
module IDL = Idl.Make (M)
module Client = API (IDL.GenClient ())

(* Send data to the socket. *)
let send socket msg =
  let ic = U.in_channel_of_descr socket in
  let oc = U.out_channel_of_descr socket in
  IPCUtil.send_delimited oc msg;
  Caml.input_line ic

let rpc ~sock_path call =
  let socket = U.(socket ~domain:PF_UNIX ~kind:SOCK_STREAM ~protocol:0) in
  U.connect socket ~addr:(U.ADDR_UNIX sock_path);
  let msg = Jsonrpc.string_of_call ~version:Jsonrpc.V2 call in
  ptrace @@ Printf.sprintf "\nSending: %s\n" msg;
  let response =
    Util.protect_reraise
      ~f:(fun () -> send socket msg)
      ~finally:(fun () -> U.close socket)
  in
  ptrace @@ Printf.sprintf "\nResponse: %s\n" response;
  Jsonrpc.response_of_string response
