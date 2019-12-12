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

module Cmds = API(Cmdlinergen.Gen ())

module Cmd = struct
  open Cmdliner.Term

  let default ~version =
    let doc = "The Scilla server CLI" in
    ret (const (fun _ -> `Help (`Pager, None)) $ const ()),
    info "cli" ~version ~doc

  let server ~sock_path ~num_pending =
    const @@ Server.start ~sock_path ~num_pending $ const (),
    info "start" ~doc:"Start Scilla server"
end

let run ~sock_path ~num_pending =
  let open Cmdliner in
  let rpc = Client.rpc ~sock_path in
  let def = Cmd.default ~version:"1.0.0" in
  let srv = Cmd.server ~sock_path ~num_pending in
  let mk_cmd fn =
    let (term, info) = fn rpc in
    Term.(term $ const ()), info in
  let impl = Cmds.implementation () in
  let calls = List.map impl ~f:mk_cmd  in
  let cmds = srv :: calls in
  Term.eval_choice def cmds
