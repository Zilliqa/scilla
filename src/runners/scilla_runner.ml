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

let output_to_string output ~args =
  if args.Runner.pp_json then
    Yojson.Basic.pretty_to_string output
  else
    Yojson.Basic.to_string output

let () =
  let args = Cli.parse () in
  let output = Runner.run args in
  let str = output_to_string ~args output in
  Out_channel.with_file args.Runner.output
    ~f:(fun ch -> Out_channel.output_string ch str)
