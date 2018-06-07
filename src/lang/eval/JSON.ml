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

exception Invalid_json of string
let addr_len = 40
let hash_len = 64

let member_exn m j =
  let open Basic.Util in
  let v = member m j in
  match v with
  | `Null -> raise (Invalid_json ("Member '" ^ m ^ "' not found in json"))
  | j -> j

let lit_exn n =
  let s, re, l = 
    match n with
    | IntLit l | BNum l ->
      l, Str.regexp "[0-9]+$", 0
    | Address a ->
        a, Str.regexp "0x[0-9a-f]+$", addr_len+2
    | Sha256 s ->
        s, Str.regexp "0x[0-9a-f]+$", hash_len+2
    | StringLit s -> s, Str.regexp ".*", 0
    | _ -> "", Str.regexp "", 0
  in
  if (Str.string_match re s 0)
  then
    (if l <> 0 && (String.length s) <> l
     then
      raise (Invalid_json ("Invalid " ^ literal_tag n ^ " : " ^ s ^ " in json"))
     else
      n
    )
  else
    raise (Invalid_json ("Invalid " ^ literal_tag n ^ " : " ^ s ^ " in json"))

let rec json_to_adtargs tjs ajs =
  let open Basic.Util in
  match tjs, ajs with
  | (tj :: tr), (aj :: ar) ->
      let tjs = to_string tj in
      let ajs = to_string aj in
      let argS = 
        (match tjs with
         | "String" -> Some (lit_exn(StringLit ajs))
         | "Int" -> Some (lit_exn(IntLit ajs))
         | "BNum" -> Some (lit_exn(BNum ajs))
         | "Address" -> Some (lit_exn(Address ajs))
         | "Hash" -> Some (lit_exn(Sha256 ajs))
         | _ -> None
        ) in
      let (trem, arem) = json_to_adtargs tr ar in
      (match argS with
       | Some l -> ((PrimType tjs) :: trem, l :: arem)
       | None -> [], []
      )
  | _ -> [], []

let rec read_adt_json j =
  let open Basic.Util in
  match j with
  | `Assoc adt ->
      let constr = member_exn "constructor" j |> to_string in
      let argtypes = member_exn "argtypes" j |> to_list in
      let arguments = member_exn "arguments" j |> to_list in
      let (tlist, arglit) = json_to_adtargs argtypes arguments in
      Some (ADTValue (constr, tlist, arglit))
  | _ -> None

(* Map is a `List of `Assoc jsons, with
 * the first `Assoc specifying the map's from/to types.*)
and read_map_json j =
  let open Basic.Util in
  match j with
  | `List vli ->
      (match vli with 
       | first :: remaining ->
           let ktype = member_exn "keyType" first |> to_string in
           let vtype = member_exn "valType" first |> to_string in
           let kvallist = mapvalues_from_json ktype vtype remaining in
           Some (Map kvallist)
       | _ -> None
      )
  | _ -> None

and mapvalues_from_json ktype vtype l = 
  let open Basic.Util in
  match l with
  | first :: remaining ->
      let kval = member_exn "key" first |> to_string in
      let keylit = 
        (match ktype with
         | "String" -> Some (lit_exn(StringLit kval))
         | "Int" -> Some (lit_exn(IntLit kval))
         | "BNum" -> Some (lit_exn(BNum kval))
         | "Address" -> Some (lit_exn(Address kval))
         | "Hash" -> Some (lit_exn(Sha256 kval))
         | _ -> None) in
        let vjson = member_exn "val" first in
      let vallit =
         (match vtype with
          | "Map" ->
            read_map_json vjson
          | "ADT" ->
            read_adt_json vjson
          | _ ->
            let vval = vjson |> to_string in
            (match vtype with
             | "String" -> Some (lit_exn(StringLit vval))
             | "Int" -> Some (lit_exn(IntLit vval))
             | "BNum" -> Some (lit_exn(BNum vval))
             | "Address" -> Some (lit_exn(Address vval))
             | "Hash" -> Some (lit_exn(Sha256 vval))
             | _ -> None)
         ) in
        let vlist = mapvalues_from_json ktype vtype remaining in
        (match keylit, vallit with
         | Some kl, Some vl -> (kl, vl) :: vlist
         | _ -> vlist)
  | [] -> []

let jobj_to_statevar json =
  let open Basic.Util in
  let n = member_exn "vname" json |> to_string in
  let t = member_exn "type" json |> to_string in
  match t with
  | "Map" ->
    (* Handle Map separately. Map is a `List of `Assoc jsons, with
     * the first `Assoc specifying the map's from/to types. *)
    let v = member "value" json in
    let vl = read_map_json v in
    (match vl with
    | Some vlm ->
        Some (n, vlm)
    | None ->
        None
    )
  | "ADT" ->
    let v = member_exn "value" json in
    let vl = read_adt_json v in
    (match vl with
    | Some vlm ->
        Some (n, vlm)
    | None ->
        None
    )
  | _ ->  
    let v = member_exn "value" json |> to_string in
    (match t with
      (* see Syntax.literal_tag *)
      | "String" -> Some (lit_exn(StringLit v))
      | "Int" -> Some (lit_exn(IntLit v))
      | "BNum" -> Some (lit_exn(BNum v))
      | "Address" -> Some (lit_exn(Address v))
      | "Hash" -> Some (lit_exn(Sha256 v))
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
  let tags = member_exn "_tag" json |> to_string in
  let amounts = member_exn "_amount" json |> to_string in
  (* Make tag and amount into a literal *)
  let tag = ("_tag", StringLit(tags)) in
  let amount = ("_amount", IntLit(amounts)) in
  let pjlist = member_exn "params" json |> to_list in
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
