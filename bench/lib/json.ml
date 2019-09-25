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
open Yojson
open Syntax

let exn_wrapper thunk =
  try
    thunk()
  with
  | Json_error s
  | Basic.Util.Undefined (s, _)
  | Basic.Util.Type_error (s, _)
    -> failwith s
  | e -> failwith (Exn.to_string e)

let from_file f =
  let thunk () = Basic.from_file f in
  exn_wrapper thunk

let member m j =
  let thunk () = Basic.Util.member m j in
  exn_wrapper thunk

let to_list j =
  let thunk () = Basic.Util.to_list j in
  exn_wrapper thunk

let to_string j =
  let thunk() = Basic.Util.to_string j in
  exn_wrapper thunk

let rec to_pb t j =
  let open Ipcmessage_types in
  match t with
  | MapType (_, vt) ->
      let kvlist = to_list j in
      let kvlist' = List.map kvlist ~f:(fun kvj ->
          let kj = member "key" kvj in
          let vj = member "val" kvj in
          let kpb =  Basic.pretty_to_string kj in
          let vpb = to_pb vt vj in
          (kpb, vpb)
        ) in
      Mval ({m = kvlist' })
  | _ -> Bval (Bytes.of_string (Basic.pretty_to_string j))
