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

(* A fast JSON parser for states that performs no validations. *)
open Core
open Yojson
open ErrorUtils
open TypeUtil
open Datatypes
module JSONTypeUtilities = TypeUtilities
module JSONIdentifier = TypeUtil.TUIdentifier
module JSONName = JSONIdentifier.Name
module JSONType = TypeUtil.TUType
module JSONLiteral = TypeUtil.TULiteral
open JSONTypeUtilities

(*************************************)
(***** Exception and wrappers ********)
(*************************************)

let json_exn_wrapper ?filename thunk =
  try thunk () with
  | Yojson.Json_error s
  | Yojson.Basic.Util.Undefined (s, _)
  | Yojson.Basic.Util.Type_error (s, _) ->
      raise (mk_invalid_json ~kind:s ?inst:None)
  | _ -> (
      match filename with
      | Some f ->
          raise
            (mk_invalid_json
               ~kind:(Printf.sprintf "Unknown error parsing JSON %s" f)
               ?inst:None)
      | None ->
          raise
            (mk_invalid_json
               ~kind:(Printf.sprintf "Unknown error parsing JSON")
               ?inst:None))

let member_exn m j =
  let thunk () = Basic.Util.member m j in
  let v = json_exn_wrapper thunk in
  match v with
  | `Null -> raise (mk_invalid_json ~kind:"Member not found in json" ~inst:m)
  | j -> j

let to_string_exn j =
  let thunk () = Basic.Util.to_string j in
  json_exn_wrapper thunk

let constr_pattern_arg_types_exn dt cname =
  match constr_pattern_arg_types dt cname with
  | Error emsg -> raise (Invalid_json emsg)
  | Ok s -> s

let lookup_adt_name_exn name =
  match DataTypeDictionary.lookup_name (JSONIdentifier.get_id name) with
  | Error emsg -> raise (Invalid_json emsg)
  | Ok s -> s

