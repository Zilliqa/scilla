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
    (* see Syntax.literal_tag *)
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

let state_to_json (state : (string*literal)) : Basic.json =
  let (vname, lit) = state in
  `Assoc [ 
    ("vname", `String vname) ; 
    ("type", `String (literal_tag lit));
    ("value", `String (pp_literal lit))
  ]

let rec slist_to_json (l : (string * literal) list) j = 
  match l with
  | [] -> j
  | s :: remaining -> 
    let sj = state_to_json s in
    let remj = slist_to_json remaining j in
      sj :: remj


module ContractState = struct

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

(** Prints a list of state variables (string, literal)
    as a json to the specified output filename. 
    pp enables pretty printing. **)
let put_json_data ?(pp=false) (filename : string) (states : (string * literal) list ) : unit =
  let jsonl = slist_to_json states [] in
  let json = `List jsonl in
  if pp
  then
    let str = Basic.pretty_to_string json in
      let channel = Out_channel.create filename in
        Out_channel.output_string channel str;
        Out_channel.close channel;
  else
    Basic.to_file filename json

end
