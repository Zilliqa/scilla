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
open Scilla_eval
open Api
module U = Unix
module M = Idl.IdM
module IDL = Idl.Make (M)

module IDLClient = API (IDL.GenClient ())

(* Send data to the socket. *)
let send socket msg =
  let ic = U.in_channel_of_descr socket in
  let oc = U.out_channel_of_descr socket in
  IPCUtil.send_delimited oc msg;
  let str = Caml.input_line ic in
  Jsonrpc.response_of_string str

let rpc ~sock_path call =
  let socket = U.(socket ~domain:PF_UNIX ~kind:SOCK_STREAM ~protocol:0) in
  U.connect socket ~addr:(U.ADDR_UNIX sock_path);
  let msg = Jsonrpc.string_of_call ~version:Jsonrpc.V2 call in
  Util.protect_reraise
    ~f:(fun () -> send socket msg)
    ~finally:(fun () -> U.close socket)

let mk_params args =
  Rpc.[ Dict [ ("argv", Enum (List.map args ~f:(fun s -> String s))) ] ]

let mk_call name ~sock_path args =
  rpc ~sock_path @@ Rpc.call name (mk_params args)

let run = mk_call "run"

let check = mk_call "check"
