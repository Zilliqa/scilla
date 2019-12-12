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

module M = Idl.IdM
module IDL = Idl.Make(M)
module Client = API(IDL.GenClient ())

let rpc ~sock_path (call: Rpc.call) : Rpc.response =
  let socket = Unix.socket ~domain: Unix.PF_UNIX ~kind: Unix.SOCK_STREAM ~protocol:0 in
  let addr = Unix.ADDR_UNIX sock_path in
  Unix.connect socket ~addr;
  let ic = Unix.in_channel_of_descr socket in
  let oc = Unix.out_channel_of_descr socket in
  let msg_buf = Jsonrpc.string_of_call ~version: Jsonrpc.V2 call in
  pout @@ Printf.sprintf "Sending: %s\n" msg_buf;
  Out_channel.flush stdout;
  Out_channel.(output_string oc msg_buf; flush oc);
  let response = Caml.input_line ic in
  Unix.close socket;
  pout @@ Printf.sprintf "Response: %s\n" response;
  Out_channel.flush stdout;
  Jsonrpc.response_of_string response
