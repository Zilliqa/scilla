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
        | "Hash" -> Some (Sha256 kval)
        | _ -> None) in
      let vallit = 
        (match vtype with
        | "String" -> Some (StringLit vval)
        | "Int" -> Some (IntLit vval)
        | "BNum" -> Some (BNum vval)
        | "Address" -> Some (Address vval)
        | "Hash" -> Some (Sha256 vval)
        | _ -> None) in
      let vlist = mapvalues_from_json ktype vtype remaining in
      (match keylit, vallit with
       | Some kl, Some vl -> (kl, vl) :: vlist
       | _ -> vlist)
  | [] -> []

let rec json_to_adtargs tjs ajs =
  let open Basic.Util in
  match tjs, ajs with
  | (tj :: tr), (aj :: ar) ->
      let tjs = to_string tj in
      let ajs = to_string aj in
      let argS = 
      (match tjs with
        | "String" -> Some (StringLit ajs)
        | "Int" -> Some (IntLit ajs)
        | "BNum" -> Some (BNum ajs)
        | "Address" -> Some (Address ajs)
        | "Hash" -> Some (Sha256 ajs)
        | _ -> None
      ) in
      Printf.printf "tjs:%s\n" tjs;
      Printf.printf "Hello: %s\n" (Bool.to_string (Option.is_some argS));
      let (trem, arem) = json_to_adtargs tr ar in
      (match argS with
        | Some l -> ((PrimType tjs) :: trem, l :: arem)
        | None -> [], []
      )
  | _ -> [], []

let jobj_to_statevar json =
  let open Basic.Util in
  let n = member "vname" json |> to_string in
  let t = member "type" json |> to_string in
  match t with
  | "Map" ->
    (* Handle Map separately. Map is a `List of `Assoc jsons, with
     * the first `Assoc specifying the map's from/to types. *)
    let v = member "value" json in
    (match v with
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
    )
  | "ADT" ->
    let v = member "value" json in
    (match v with
      | `Assoc adt ->
          let constr = member "constructor" v |> to_string in
          let argtypes = member "argtypes" v |> to_list in
          let arguments = member "arguments" v |> to_list in
          let (tlist, arglit) = json_to_adtargs argtypes arguments in
            Some (n, ADTValue (constr, tlist, arglit))
      | _ -> None
    )
  | _ ->  
    let v = member "value" json |> to_string in
    (match t with
      (* see Syntax.literal_tag *)
      | "String" -> Some (StringLit(v))
      | "Int" -> Some (IntLit(v))
      | "BNum" -> Some (BNum(v))
      | "Address" -> Some (Address(v))
      | "Hash" -> Some (Sha256(v))
      | _ -> None) |> Option.map ~f:(fun x -> (n, x))

let typ_to_string t = 
  match t with
  | PrimType t -> t
  (* TODO: Support other typ *)
  | _ -> "Unknown"

let rec mapvalues_to_json ms = 
  match ms with
  | kv :: remaining ->
    let (k, v) = kv in
    let kjson = "key", (literal_to_json k) in
    let vjson = "val", (literal_to_json v) in
    let kv_json = `Assoc (kjson :: vjson :: []) in
      kv_json :: (mapvalues_to_json remaining)
  | [] -> []

and adtargs_to_json tlist vlist =
  match tlist, vlist with
  | t1 :: tn, v1 :: vn ->
    let (j1, j2) = `String (typ_to_string t1) , literal_to_json v1 in
    let (jtn, jvn)= adtargs_to_json tn vn in
      (j1 :: jtn), (j2 :: jvn)
  | _ -> ([], [])

and literal_to_json lit = 
  match lit with
  | StringLit (x) | IntLit (x)| BNum (x) | Address (x) | Sha256 (x) -> `String (x)
  | Map [] -> `Null
  | Map (kv :: remaining) ->
    let (k, v) = kv in
    let kjson = "keyType", `String (literal_tag k) in
    let vjson =  "valType", `String (literal_tag v) in
    let mtype_json = `Assoc (kjson :: vjson :: []) in
    let kv_json = mapvalues_to_json (kv :: remaining) in
    (* The output state variable for a map has the from/to type
     * as the first map entry and the actual entries follow *)
      `List (mtype_json :: kv_json)
  | ADTValue (n, t, v) ->
      let (argtl, argl) = adtargs_to_json t v in
        `Assoc [
          ("constructor", `String n);
          ("argtypes", `List argtl);
          ("arguments", `List argl)
        ]
  | _ -> `Null

let state_to_json state =
  let (vname, lit) = state in
  `Assoc [ 
    ("vname", `String vname) ; 
    ("type", `String (literal_tag lit));
    ("value", (literal_to_json lit))
  ]

let rec slist_to_json l = 
  match l with
  | [] -> []
  | s :: remaining -> 
    let sj = state_to_json s in
    let remj = slist_to_json remaining in
      sj :: remj

let get_string_literal l =
  match l with
  | StringLit sl -> Some sl
  | _ -> None

let get_int_literal l =
  match l with
  | IntLit il -> Some il
  | _ -> None

module ContractState = struct

(** Returns a list of (vname:string,value:literal) items
    Invalid inputs in the json are ignored **)
let get_json_data filename  =
  let json = Basic.from_file filename in
  (* input json is a list of key/value pairs *)
  let jlist = json |> Basic.Util.to_list in
  (* map the json list to a tuple (vname,value) option *)
  let olist = List.map jlist ~f:jobj_to_statevar in
  List.fold_right olist ~init:[]
    ~f:(fun o z -> match o with Some x -> x :: z | None -> z)

(* Get a json object from given states *)
let state_to_json states = 
  let jsonl = slist_to_json states in
    `List jsonl

(** 
  ** Prints a list of state variables (string, literal)
  ** as a json and returns it as a string.
  ** pp enables pretty printing.
  **)
let state_to_string ?(pp = false) states =
  let json = state_to_json states in
  if pp
  then
    pretty_to_string json
  else
    to_string json

end

module Message = struct

(** Parses and returns a list of (pname,pval), with
  "_tag" and "_amount" at the beginning of this list.
  Invalid inputs in the json are ignored **)
let get_json_data filename =
  let open Basic.Util in
  let json = Basic.from_file filename in
  let tags = member "_tag" json |> to_string in
  let amounts = member "_amount" json |> to_string in
  (* Make tag and amount into a literal *)
  let tag = ("_tag", StringLit(tags)) in
  let amount = ("_amount", IntLit(amounts)) in
  let pjlist = member "params" json |> to_list in
  let plist = List.map pjlist ~f:jobj_to_statevar in
  let params = List.fold_right plist ~init:[]
    ~f:(fun o z -> match o with Some x -> x :: z | None -> z) in
    tag :: amount :: params

(* Same as message_to_jstring, but instead gives out raw json, not it's string *)
let message_to_json message =
  (* extract out "_tag" and "_amount" parts of the message *)
  let (_, taglit) = List.find_exn message ~f:(fun (x, _) -> x = "_tag") in
  let (_, amountlit) = List.find_exn message ~f:(fun (x, _) -> x = "_amount") in
  let tags = get_string_literal taglit in
  let amounts = get_int_literal amountlit in
  (* Get a list without either of these components *)
  let filtered_list = List.filter message ~f:(fun (x, _) -> not ((x = "_tag") || (x = "_amount"))) in
    `Assoc [("_tag", `String (BatOption.get tags)); 
                 ("_amount", `String (BatOption.get amounts));
                 ("params", `List (slist_to_json filtered_list))] 

  (** 
  ** Prints a message (string, literal) as a json to the 
  ** and returns the string. pp enables pretty printing.
  ** The difference b/w this and the one in ContractState 
  ** is that this has a mandatory "_tag" and "_amount" field,
  ** with the actual params themselves in an array json with
  ** name "params" (as described in comment in .mli file).
  **)
let message_to_jstring ?(pp = false) message =
  let j = message_to_json message in
  if pp
  then
    Basic.pretty_to_string j
  else
    Basic.to_string j

end

module BlockChainState = struct

  (**  Returns a list of (vname:string,value:literal) items
   **  from the json in the input filename. Invalid inputs in the json are ignored.
   **  This is different from ContractState only w.r.t. validating that all
   **  all variables are from a pre-determined set of actual block chain state. **)
let get_json_data filename  =
  let json = Basic.from_file filename in
  (* input json is a list of key/value pairs *)
  let jlist = json |> Basic.Util.to_list in
  (* map the json list to a tuple (vname,value) option *)
  let olist = List.map jlist ~f:jobj_to_statevar in
    List.fold_right olist ~init:[]
      ~f:(fun o z -> match o with Some x -> x :: z | None -> z)
  (* TODO: Validate for only block chain variables *)

end
