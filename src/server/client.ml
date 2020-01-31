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

let rpc ~sock_path call =
  let socket = U.(socket ~domain:PF_UNIX ~kind:SOCK_STREAM ~protocol:0) in
  let addr = U.ADDR_UNIX sock_path in
  U.connect socket ~addr;
  let ic = U.in_channel_of_descr socket in
  let oc = U.out_channel_of_descr socket in
  let msg_buf = Jsonrpc.string_of_call ~version:Jsonrpc.V2 call in
  ptrace @@ Printf.sprintf "\nSending: %s\n" msg_buf;
  (* Send data to the socket. *)
  IPCUtil.send_delimited oc msg_buf;
  let response = Caml.input_line ic in
  U.close socket;
  ptrace @@ Printf.sprintf "\nResponse: %s\n" response;
  Jsonrpc.response_of_string response
