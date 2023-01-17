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
open Scilla_server_lib

let cmd =
  Command.basic ~summary:"Scilla server"
    Command.Let_syntax.(
      let%map_open sock_path =
        flag "-socket"
          (optional_with_default Server.sock_path string)
          ~doc:"SOCKET Address for communication with the server"
      and num_pending =
        flag "-num-pending"
          (optional_with_default Server.num_pending int)
          ~doc:"NUM_PENDING Maximum number of pending requests"
      and daemonise = flag "-daemonise" no_arg ~doc:"Run Scilla in background"
      and logs_path =
        flag "-logs" (optional string)
          ~doc:"PATH Path to save logs in daemon mode"
      in
      fun () ->
        Server.start ~server_implementation:Server.default_server_implementation
          ~sock_path ~num_pending ~daemonise ~logs_path)

let () = Command_unix.run cmd
