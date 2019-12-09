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

open Api
open DebugMessage

module M = Idl.IdM
module IDL = Idl.Make(M)

module Client = API(IDL.GenClient ())
module Cmds = API(Cmdlinergen.Gen ())

module Cmd = struct
  open Cmdliner.Term

  let mk_default ~version =
    let doc = "The Scilla server CLI" in
    ret (const (fun _ -> `Help (`Pager, None)) $ const ()),
    info "cli" ~version ~doc

  let mk_server ~sock_path ~num_pending =
    const @@ Server.start ~sock_path ~num_pending $ const (),
    info "scilla-server" ~doc:"Start the scilla-server"
end

let binary_rpc ~sock_path (call: Rpc.call) : Rpc.response M.t =
  let sockaddr = Unix.ADDR_UNIX sock_path in
  let socket = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.connect socket sockaddr;
  let ic = Unix.in_channel_of_descr socket in
  let oc = Unix.out_channel_of_descr socket in
  let msg_buf = Jsonrpc.string_of_call ~version: Jsonrpc.V2 call in
  pout @@ Printf.sprintf "Sending: %s\n" msg_buf;
  output_string oc msg_buf;
  flush oc;
  let response = Caml.input_line ic in
  Unix.close socket;
  pout @@ Printf.sprintf "Response: %s\n" response;
  M.return @@ Jsonrpc.response_of_string response

let mk_cli ~sock_path ~num_pending () =
  let rpc = binary_rpc ~sock_path in
  let def = Cmd.mk_default ~version:"1.0.0" in
  let srv = Cmd.mk_server ~sock_path ~num_pending in
  Cmdliner.Term.eval_choice def (
    srv
    :: List.map
      (fun t -> let (term, info) = t rpc in (Cmdliner.Term.(term $ const ()), info))
      (Cmds.implementation ())
    )
