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

let fmt = "%Y%m%d%H%M%S"
let zone = force Time.Zone.local

let format time = Time.format time fmt ~zone

let parse = Time.parse ~fmt ~zone

let mk () = format @@ Time.now ()

let sort_desc paths =
  paths
  |> List.map ~f:parse
  |> List.sort ~compare:Time.compare
  |> List.rev_map ~f:format

let%test "roundtrip" =
  let ts = Time.now () in
  format ts = format (parse (format ts))
