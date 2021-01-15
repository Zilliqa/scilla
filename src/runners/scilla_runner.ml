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
open ErrorUtils
open RunnerCLI

let output_to_string output ~args =
  if args.pp_json then Yojson.Basic.pretty_to_string output
  else Yojson.Basic.to_string output

let () =
  try
    let output, args = Runner.run None ~exe_name:(Sys.get_argv ()).(0) in
    let str = output_to_string output ~args in
    if String.is_empty args.output then DebugMessage.pout str
    else
      Out_channel.with_file args.output ~f:(fun ch ->
          Out_channel.output_string ch str)
  with FatalError msg -> exit_with_error msg
