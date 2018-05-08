(*
 * Copyright (c) 2018 - present Zilliqa, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open Syntax
open Core
open Yojson
let jobj_to_statevar (json : Basic.json) =
  let open Basic.Util in
  let n = member "vname" json |> to_string in
  let t = member "type" json |> to_string in
  let v = member "value" json |> to_string in
  let tt = match t with
    | "String" -> Some (StringLit(v))
    | "Int" -> Some (IntLit(v))
    | "BNum" -> Some (BNum(v))
    | "Address" -> Some (Address(v))
    | "Sha256" -> Some (Sha256(v))
    | _ -> None
    in 
  match tt with
    | Some ttt -> Some (n, ttt)
    | None -> None

module StateInput = struct

(** Returns a list of (vname:string,value:literal) items
    Invalid inputs in the json are ignored **)
 let get_json_data (filename : string) : (string * literal) list =
  let json = Basic.from_file filename in
  (* input json is a list of key/value pairs *)
  let jlist = json |> Basic.Util.to_list in
  (* map the json list to a tuple (name,type,value) option *)
  let olist = List.map jlist ~f:jobj_to_statevar in
  let filtered_list = List.filter olist ~f:(fun o -> match o with | Some x -> true | None -> false) in
  List.map filtered_list ~f:(fun o -> match o with | Some x -> x | None -> assert false)
end
