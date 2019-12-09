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

(* You can swap the RPC engine, by using a different monad here,
   note however that if you are using an asynchronous one, like
   lwt or async, you should also use their specific IO functions
   including the print functions.

   You can easily put ExnM here and the code would stay unchanged.*)
module M = Idl.IdM
module IDL = Idl.Make(M)
module Server = API(IDL.GenServer ())

(** Command handler that runs Scilla with the
    given [argv] and returns the resulting JSON. *)
let runner argv =
  IDL.ErrM.return "it works"

(** Helper function to create a directory with
    the given permissions if it doesn't already exist. *)
let mkdir_rec dir perm =
  let rec p_mkdir dir =
    let p_name = Filename.dirname dir in
    if p_name <> "/" && p_name <> "."
    then p_mkdir p_name;
    (try Unix.mkdir dir perm with Unix.Unix_error(Unix.EEXIST, _, _) -> ()) in
  p_mkdir dir

(** Request handler. *)
let handler fn conn =
  let (>>=) = M.bind in
  let ic = Unix.in_channel_of_descr conn in
  let oc = Unix.out_channel_of_descr conn in
  let request = Jsonrpc.call_of_string (Caml.input_line ic) in
  fn request >>= fun result ->
  output_string oc result;
  flush oc;
  M.return ()

(** Utility and general non-specific server bits and bobs. *)
let finally f g =
  try
    let r = f () in g (); r
  with e ->
    g (); raise e

(** Listen on the given [sock_path] and process requests.
    The [num_pending] is the maximal number of pending requests. *)
let serve fn ~sock_path ~num_pending =
  (try Unix.unlink sock_path with Unix.Unix_error(Unix.ENOENT, _, _) -> ());
  (* Ensure that socket directory exists *)
  mkdir_rec (Filename.dirname sock_path) 0o0755;
  let socket = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.bind socket (Unix.ADDR_UNIX sock_path);
  Unix.listen socket num_pending;
  pout @@ Printf.sprintf "Listening on %s" sock_path;
  while true do
    let conn, _ = Unix.accept socket in
    let (_: Thread.t) = Thread.create
      (fun () ->
        finally
          (* Here we're calling [M.run] to make sure that we are running the process,
             this is not much of a problem with [IdM] or [ExnM], but in general we
             should ensure that the computation is started by a runner *)
          (fun () -> conn |> handler fn |> M.run)
          (* Close the connection no matter what *)
          (fun () -> Unix.close conn)
      ) ()
    in ()
  done

(** Start the server. *)
let start ~sock_path ~num_pending () =
  Server.runner runner;
  (* Add more handlers here, for example: Server.checker checker; *)
  (* Generate the "rpc" function from the implementation,
     that given an Rpc.call, calls the implementation of that RPC method and
     performs the marshalling and unmarshalling. We need to connect this
     function to a real server that responds to client requests *)
  let rpc = IDL.server Server.implementation in
  (* Helper function that runs the corresponding RPC handler,
     then converts it's result to a string and returns it *)
  let fn x = M.(rpc x >>= fun r -> r |> Jsonrpc.string_of_response |> return) in
  (* Listen the socket, accept connections and handle requests (RPC's) *)
  serve fn ~sock_path ~num_pending
