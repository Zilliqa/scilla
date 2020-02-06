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

let mk_cmd cb ~summary =
  Command.basic ~summary
    Command.Let_syntax.(
      let%map_open sock_path =
        flag "-socket"
          (optional_with_default Server.sock_path string)
          ~doc:"SOCKET Address for communication with the server"
      and argv = flag "-argv" (required string) ~doc:"" in
      let args = String.split argv ~on:' ' in
      fun () -> print_string @@ cb ~sock_path args)

let run =
  mk_cmd Client.run
    ~summary:"Execute contract"

let check =
  mk_cmd Client.check
    ~summary:"Parse a contract and perform a number of static checks including typechecking"

let cmd_group =
  Command.group ~summary:"Scilla client"
    [ ("run", run); ("check", check) ]

let () = Command.run cmd_group
