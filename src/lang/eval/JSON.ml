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

let rec mapvalues_from_json ktype vtype l = 
  let open Basic.Util in
  match l with
  | first :: remaining ->
      let kval = member "key" first |> to_string in
      let vval = member "val" first |> to_string in
      let keylit = 
        (match ktype with
        | "String" -> Some (StringLit kval)
        | "Int" -> Some (IntLit kval)
        | "BNum" -> Some (BNum kval)
        | "Address" -> Some (Address kval)
        | "Sha256" -> Some (Sha256 kval)
        | _ -> None) in
      let vallit = 
        (match vtype with
        | "String" -> Some (StringLit vval)
        | "Int" -> Some (IntLit vval)
        | "BNum" -> Some (BNum vval)
        | "Address" -> Some (Address vval)
        | "Sha256" -> Some (Sha256 vval)
        | _ -> None) in
      let vlist = mapvalues_from_json ktype vtype remaining in
      (match keylit, vallit with
       | Some kl, Some vl -> (kl, vl) :: vlist
       | _ -> vlist)
  | [] -> []

let jobj_to_statevar json =
  let open Basic.Util in
  let n = member "vname" json |> to_string in
  let t = member "type" json |> to_string in
  if t <> "Map"
  then
    let v = member "value" json |> to_string in
    (match t with
      (* see Syntax.literal_tag *)
      | "String" -> Some (StringLit(v))
      | "Int" -> Some (IntLit(v))
      | "BNum" -> Some (BNum(v))
      | "Address" -> Some (Address(v))
      | "Sha256" -> Some (Sha256(v))
      | _ -> None) |> Option.map ~f:(fun x -> (n, x))
  else
    (* Handle Map separately. Map is a `List of `Assoc jsons, with
     * the first `Assoc specifying the map's from/to types. *)
    let v = member "value" json in
      match v with
      | `List vli ->
         (match vli with 
          | first :: remaining ->
              let ktype = member "keyType" first |> to_string in
              let vtype = member "valType" first |> to_string in
              let kvallist = mapvalues_from_json ktype vtype remaining in
                Some (n, Map (kvallist))
          | _ -> None
         )
      | _ -> None
    

let rec mapvalues_to_json ms = 
  match ms with
  | kv :: remaining ->
    let (k, v) = kv in
    let kjson = "key", (literal_to_json k) in
    let vjson = "val", (literal_to_json v) in
    let kv_json = `Assoc (kjson :: vjson :: []) in
      kv_json :: (mapvalues_to_json remaining)
  | [] -> []

and literal_to_json lit = 
  match lit with
  | StringLit (x) | IntLit (x)| BNum (x) | Address (x) | Sha256 (x) -> `String (x)
  | Map [] -> `Null
  | Map (kv :: remaining) ->
    let (k, v) = kv in
    let kjson = "keyType", `String (literal_tag k) in
    let vjson =  "ValType", `String (literal_tag v) in
    let mtype_json = `Assoc (kjson :: vjson :: []) in
    let kv_json = mapvalues_to_json (kv :: remaining) in
    (* The output state variable for a map has the from/to type
     * as the first map entry and the actual entries follow *)
      `List (mtype_json :: kv_json)
  | ADTValue _ (* Handle ADT *)
  | _ -> `Null

let state_to_json state =
  let (vname, lit) = state in
  `Assoc [ 
    ("vname", `String vname) ; 
    ("type", `String (literal_tag lit));
    ("value", (literal_to_json lit))
  ]

let rec slist_to_json l j = 
  match l with
  | [] -> j
  | s :: remaining -> 
    let sj = state_to_json s in
    let remj = slist_to_json remaining j in
      sj :: remj


module ContractState = struct

(** Returns a list of (vname:string,value:literal) items
    Invalid inputs in the json are ignored **)
let get_json_data filename  =
  let json = Basic.from_file filename in
  (* input json is a list of key/value pairs *)
  let jlist = json |> Basic.Util.to_list in
  (* map the json list to a tuple (vname,value) option *)
  let olist = List.map jlist ~f:jobj_to_statevar in
  let filtered_list = List.filter olist ~f:Option.is_some in
  List.map filtered_list ~f:(function Some x -> x | None -> assert false)

(** Prints a list of state variables (string, literal)
    as a json to the specified output filename.
    pp enables pretty printing. **)
let put_json_data ?(pp = false) filename states =
  let jsonl = slist_to_json states [] in
  let json = `List jsonl in
  if pp
  then
    Out_channel.with_file filename ~f:(fun channel -> 
        pretty_to_string json |> Out_channel.output_string channel)
  else
    to_file filename json

end
