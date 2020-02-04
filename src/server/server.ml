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
open DebugMessage
open ErrorUtils
open Api

(* You can swap the RPC engine, by using a different monad here,
   note however that if you are using an asynchronous one, like
   lwt or async, you should also use their specific IO functions
   including the print functions.

   You can easily put [ExnM] here and the code would stay unchanged. *)
module M = Idl.IdM
module IDL = Idl.Make (M)

module Server = API (IDL.GenServer ())

(* Makes a handler that executes the given [callback] with [args],
   caches the result using the LRU cache and returns it. **)
let mk_handler ~name ~callback args =
  let open IDL.ErrM in
  ptrace
  @@ Printf.sprintf "\n%s request:\n %s\n" name (String.concat ~sep:" " args);
  try
    let result = callback @@ Some args in
    pout @@ Printf.sprintf "\n%s response:\n %s\n" name result;
    (* TODO: implement AST caching here *)
    return result
  with FatalError msg -> return_err (Idl.DefaultError.InternalError msg)

(* Request handler. *)
let handler rpc conn =
  let ic = Unix.in_channel_of_descr conn in
  let oc = Unix.out_channel_of_descr conn in
  let req = Jsonrpc.call_of_string (Caml.input_line ic) in
  (* Here we're calling [M.run] to make sure that we are running the process,
     this is not much of a problem with [IdM] or [ExnM], but in general we
     should ensure that the computation is started by a runner *)
  let res = M.run (rpc req) in
  IPCUtil.send_delimited oc (Jsonrpc.string_of_response res)

(* Listen on the given [sock_path] and process requests.
   The [num_pending] is the maximal number of pending requests. *)
let serve rpc ~sock_path ~num_pending =
  let module U = Unix in
  (* Remove any existing socket file *)
  U.(try unlink sock_path with Unix_error (ENOENT, _, _) -> ());
  (* Ensure that socket directory exists *)
  U.mkdir_p ~perm:0o0755 (Filename.dirname sock_path);
  let socket = U.(socket ~domain:PF_UNIX ~kind:SOCK_STREAM ~protocol:0) in
  U.bind socket ~addr:(U.ADDR_UNIX sock_path);
  U.listen socket ~backlog:num_pending;
  pout @@ Printf.sprintf "Server is listening on %s\n" sock_path;
  Out_channel.flush stdout;
  while true do
    let conn, _ = U.accept socket in
    ignore
    @@ Thread.create
         (fun () ->
           (* Always close the connection no matter what *)
           Util.protect_reraise
             ~f:(fun () -> handler rpc conn)
             ~finally:(fun () -> U.close conn))
         ()
  done

let sock_path = "/tmp/scilla-server.sock"
let num_pending = 5

let start ?(sock_path = sock_path) ?(num_pending = num_pending) () =
  let runner args =
    let output, _ = Runner.run args in
    Yojson.Basic.to_string output
  in
  (* Handlers *)
  Server.runner @@ mk_handler ~name:"Runner" ~callback:runner;
  Server.checker @@ mk_handler ~name:"Checker" ~callback:Checker.run;
  (* Generate the "rpc" function from the implementation,
     that given an [Rpc.call], calls the implementation of that RPC method and
     performs the marshalling and unmarshalling. We need to connect this
     function to a real server that responds to client requests *)
  let rpc = IDL.server Server.implementation in
  (* Listen the socket, accept connections and handle requests *)
  serve rpc ~sock_path ~num_pending
