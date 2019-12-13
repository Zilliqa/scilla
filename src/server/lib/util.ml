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

let protect_reraise ~f ~finally =
  try
    let r = f () in finally (); r
  with e ->
    finally (); raise e

let mkdir_rec ~dir ~perm =
  let rec p_mkdir dir =
    let p_name = Filename.dirname dir in
    if p_name <> "/" && p_name <> "."
    then p_mkdir p_name;
    (try Unix.mkdir dir ~perm with Unix.Unix_error(Unix.EEXIST, _, _) -> ()) in
  p_mkdir dir

let send_delimited oc msg =
  let msg' = msg ^ "\n" in
  Out_channel.output_string oc msg';
  Out_channel.flush oc
