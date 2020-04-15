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
open Scilla_base
open Scilla_eval
open DebugMessage
open ErrorUtils
open Api
open IPCUtil

(* You can swap the RPC engine, by using a different monad here,
   note however that if you are using an asynchronous one, like
   lwt or async, you should also use their specific IO functions
   including the print functions.

   You can easily put [ExnM] here and the code would stay unchanged. *)
module M = Idl.IdM
module IDL = Idl.Make (M)

module Server = API (IDL.GenServer ())

(* Makes a handler that executes the given [callback] with [args] and returns it. **)
let mk_handler callback args =
  (* Force the -jsonerrors flag *)
  let args = "-jsonerrors" :: args in
  try IDL.ErrM.return @@ callback (Some args)
  with FatalError msg ->
    IDL.ErrM.return_err RPCError.{ code = 0; message = msg }

(* Request handler. *)
let handler rpc conn =
  let ic = Unix.in_channel_of_descr conn in
  let oc = Unix.out_channel_of_descr conn in
  let msg = Caml.input_line ic in
  let req = Jsonrpc.call_of_string msg in
  (* Here we're calling [M.run] to make sure that we are running the process,
     this is not much of a problem with [IdM] or [ExnM], but in general we
     should ensure that the computation is started by a runner *)
  let res =
    try M.run (rpc req)
    with _ ->
      Rpc.failure
        (RPCError.rpc_of_t
           RPCError.
             { code = 0; message = "scilla-server: incorrect invocation" })
  in
  let str = Jsonrpc.string_of_response ~version:Jsonrpc.V2 res in
  IPCUtil.send_delimited oc str

(* Listen on the given [sock_path] and process requests.
   The [num_pending] is the maximal number of pending requests. *)
let setup ~sock_path ~num_pending =
  (* Remove any existing socket file *)
  Unix.(try unlink sock_path with Unix_error (ENOENT, _, _) -> ());
  (* Ensure that socket directory exists *)
  Unix.mkdir_p ~perm:0o0755 (Filename.dirname sock_path);
  let socket = Unix.(socket ~domain:PF_UNIX ~kind:SOCK_STREAM ~protocol:0) in
  Unix.bind socket ~addr:(Unix.ADDR_UNIX sock_path);
  Unix.listen socket ~backlog:num_pending;
  pout @@ Printf.sprintf "Scilla Server is listening on %s\n" sock_path;
  Out_channel.flush stdout;
  socket

let rec serve rpc ~socket =
  let conn, _ = Unix.accept socket in
  handler rpc conn;
  Unix.close conn;
  serve rpc ~socket

let sock_path = "/tmp/scilla-server.sock"

let num_pending = 5

let start ?(sock_path = sock_path) ?(num_pending = num_pending) =
  pout "Starting Scilla server...\n";
  Out_channel.flush stdout;
  let runner args =
    let output, _ = Runner.run args ~exe_name:"scilla-runner" in
    Yojson.Basic.pretty_to_string output
  in
  (* Handlers *)
  Server.runner @@ mk_handler runner;
  Server.checker @@ mk_handler (Checker.run ~exe_name:"scilla-checker");
  (* Generate the "rpc" function from the implementation,
     that given an [Rpc.call], calls the implementation of that RPC method and
     performs the marshalling and unmarshalling. We need to connect this
     function to a real server that responds to client requests *)
  let rpc = IDL.server Server.implementation in
  (* Setup and listen the socket *)
  let socket = setup ~sock_path ~num_pending in
  (* Accept connections and handle requests *)
  serve rpc ~socket
