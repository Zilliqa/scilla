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
open Api

(* You can swap the RPC engine, by using a different monad here,
   note however that if you are using an asynchronous one, like
   lwt or async, you should also use their specific IO functions
   including the print functions.

   You can easily put [ExnM] here and the code would stay unchanged.*)
module M = Idl.IdM
module IDL = Idl.Make(M)
module Server = API(IDL.GenServer ())

(** Command handler that runs Scilla with the
    given [argv] and returns the resulting JSON. *)
let runner _argv =
  (* TODO: Validate [argv], use [invalid_query]*)
  (* TODO: implement AST caching here *)
  IDL.ErrM.return "it works"

(** Request handler. *)
let handler rpc conn =
  let ic = Unix.in_channel_of_descr conn in
  let oc = Unix.out_channel_of_descr conn in
  let req = Jsonrpc.call_of_string (Caml.input_line ic) in
  let res = rpc req |> M.run in
  Util.send_delimited oc (Jsonrpc.string_of_response res)

(** Listen on the given [sock_path] and process requests.
    The [num_pending] is the maximal number of pending requests. *)
let serve rpc ~sock_path ~num_pending =
  (try Unix.unlink sock_path with Unix.Unix_error(Unix.ENOENT, _, _) -> ());
  (* Ensure that socket directory exists *)
  Util.mkdir_rec ~dir:(Filename.dirname sock_path) ~perm:0o0755;
  let socket = Unix.socket ~domain:Unix.PF_UNIX ~kind:Unix.SOCK_STREAM ~protocol:0 in
  Unix.bind socket ~addr:(Unix.ADDR_UNIX sock_path);
  Unix.listen socket ~backlog:num_pending;
  pout @@ Printf.sprintf "Listening on %s\n" sock_path;
  Out_channel.flush stdout;
  while true do
    let conn, _ = Unix.accept socket in
    let _ = Thread.create
      (fun () ->
        Util.protect_reraise
          (* Here we're calling [M.run] to make sure that we are running the process,
             this is not much of a problem with [IdM] or [ExnM], but in general we
             should ensure that the computation is started by a runner *)
          ~f:(fun () -> handler rpc conn)
          (* Close the connection no matter what *)
          ~finally:(fun () -> Unix.close conn)
      ) ()
    in ()
  done

(** Start the server. *)
let start ~sock_path ~num_pending () =
  GlobalConfig.(
    set_log_file "./_build/logs/scilla-server.log";
    set_debug_level Debug_Normal
  );
  pout "Starting scilla server...\n";
  Out_channel.flush stdout;

  (* Handlers: *)
  Server.runner runner;
  (* Add more handlers here, for example:
     Server.checker checker; *)

  (* Generate the "rpc" function from the implementation,
     that given an Rpc.call, calls the implementation of that RPC method and
     performs the marshalling and unmarshalling. We need to connect this
     function to a real server that responds to client requests *)
  let rpc = IDL.server Server.implementation in
  (* Listen the socket, accept connections and handle requests *)
  serve rpc ~sock_path ~num_pending
