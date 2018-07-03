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
open EvalUtil.MessagePayload

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
    | IntLit (wl, l) ->
      l, Str.regexp "-?[0-9]+$", 0
    | UintLit (wl, l) ->
      l, Str.regexp "[0-9]+$", 0
    | BNum l ->
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
      (match n with
      | IntLit (wl, l) | UintLit (wl, l) ->
        (* detailed validation for integer literals *)
        if validate_int_literal n
        then
          n
        else
          raise (Invalid_json ("Invalid integer literal " ^ literal_tag n ^ " : " ^ s ^ " in json"))
      | _ ->
        n
      )
    )
  else
    raise (Invalid_json ("Invalid " ^ literal_tag n ^ " : " ^ s ^ " in json"))

  let build_int_exn t v =
    let r = build_int t v in
    match r with
    | Some rs ->
      rs
    | None ->
      raise (Invalid_json("Invalid integer type/value " ^ t ^ " " ^ v ^ " in json"))

let build_lit_exn t v =
    match t with
    | "String" -> Some (lit_exn (StringLit v))
    | "BNum" -> Some (lit_exn(BNum v))
    | "Address" -> Some (lit_exn(Address v))
    | "Hash" -> Some (lit_exn(Sha256 v))
    | _ ->
    (* See if it is an Int/Uint type. *)
    if is_int_type t || is_uint_type t
    then
      Some (build_int_exn t v)
    else
      None

let rec json_to_adtargs tjs ajs =
  let open Basic.Util in
  match tjs, ajs with
  | (tj :: tr), (aj :: ar) ->
      let tjs = to_string tj in
      let ajs = to_string aj in
      let argS = build_lit_exn tjs ajs in
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
           let parse_exn t = 
              (try EvalUtil.parse_type t
               with _ ->
                 raise (Invalid_json (sprintf "Invalid type in json:\n%s" t)))
           in
           let kt = parse_exn ktype in
           let vt = parse_exn vtype in
           let kvallist = mapvalues_from_json kt vt remaining in           
           Some (Map ((kt, vt), kvallist))
       | _ -> None
      )
  | _ -> None

and mapvalues_from_json kt vt l = 
  let open Basic.Util in
  match l with
  | first :: remaining ->
      let kjson = member_exn "key" first in
      let keylit = 
        (match kt with
         | PrimType "String" -> Some (lit_exn(StringLit (kjson |> to_string)))
         | PrimType "BNum" -> Some (lit_exn(BNum (kjson |> to_string)))
         | PrimType "Address" -> Some (lit_exn(Address (kjson |> to_string)))
         | PrimType "Hash" -> Some (lit_exn(Sha256 (kjson |> to_string)))
         | PrimType i -> (* Try for Int/Uint types *)
            Some (build_int_exn i (to_string kjson))
         | _ -> None
         ) in
      let vjson = member_exn "val" first in
      let vallit =
        (match vt with
         | PrimType "String" -> Some (lit_exn(StringLit (vjson |> to_string)))
         | PrimType "BNum" -> Some (lit_exn(BNum (vjson |> to_string)))
         | PrimType "Address" -> Some (lit_exn(Address (vjson |> to_string)))
         | PrimType "Hash" -> Some (lit_exn(Sha256 (vjson |> to_string)))
         | MapType _ ->
            read_map_json vjson
         | ADT _ ->
            read_adt_json vjson
         | PrimType i -> (* Try for Int/Uint types *)
            Some (build_int_exn i (to_string vjson))
         | _ -> None
          
          ) in
        let vlist = mapvalues_from_json kt vt remaining in
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
    let tv = build_lit_exn t v in
      Option.map ~f:(fun x -> (n, x)) tv

let rec typ_to_string t = 
  match t with
  | PrimType t -> t
  | MapType (kt, vt) ->
      sprintf "Map (%s) (%s)" (typ_to_string kt) (typ_to_string vt )
  | ADT (name, targs) ->
      let tns = List.map targs
          ~f:(fun t -> sprintf "(%s)" (typ_to_string t)) in
      sprintf "ADT %s (%s)" name (String.concat ~sep:", " tns)

  (* TODO: Support other types *)
  | _ -> "Unsupported"

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
  | StringLit (x) | BNum (x) | Address (x) | Sha256 (x) -> `String (x)
  | IntLit (wx, x) | UintLit (wx, x) -> `String (x)
  | Map ((kt, vt), kvs) ->
    let kjson = "keyType", `String (typ_to_string kt) in
    let vjson =  "valType", `String (typ_to_string vt) in
    let mtype_json = `Assoc (kjson :: vjson :: []) in
    let kv_json = mapvalues_to_json kvs in
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

let get_uint_literal l =
  match l with
  | UintLit (wil, il) -> Some il
  | _ -> None

let get_address_literal l =
  match l with
  | Address al -> Some al
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
  let tags = member_exn tag_label json |> to_string in
  let amounts = member_exn amount_label json |> to_string in
  let senders = member_exn sender_label json |> to_string in
  (* Make tag, amount and sender into a literal *)
  let tag = (tag_label, lit_exn(StringLit(tags))) in
  let amount = (amount_label, build_int_exn "Uint128" amounts) in
  let sender = (sender_label, lit_exn(Address(senders))) in
  let pjlist = member_exn "params" json |> to_list in
  let plist = List.map pjlist ~f:jobj_to_statevar in
  let params = List.fold_right plist ~init:[]
    ~f:(fun o z -> match o with Some x -> x :: z | None -> z) in
    tag :: amount :: sender :: params

(* Same as message_to_jstring, but instead gives out raw json, not it's string *)
let message_to_json message =
  (* extract out "_tag", "_amount", "_accepted" and "_recipient" parts of the message *)
  let (_, taglit) = List.find_exn message ~f:(fun (x, _) -> x = tag_label) in
  let (_, amountlit) = List.find_exn message ~f:(fun (x, _) -> x = amount_label) in
  let acceptedlit = List.find message ~f:(fun (x, _) -> x = accepted_label) in
  (* message_to_json may be used to print both output and input message. Choose label accordingly. *)
  let (toORfrom, tofromlit) = List.find_exn message ~f:(fun (x, _) -> x = recipient_label || x = sender_label) in
  let tofrom_label = if toORfrom = recipient_label then recipient_label else sender_label in
  let tags = get_string_literal taglit in
  let amounts = get_uint_literal amountlit in
  (* In case we're trying to print an input message, there won't be an "_accepted" field *)
  let accepteds = 
    if is_some acceptedlit
    then get_string_literal (snd (BatOption.get acceptedlit))
    else Some "false" in
  let tofroms = get_address_literal tofromlit in
  (* Get a list without any of these components *)
  let filtered_list = List.filter message 
      ~f:(fun (x, _) -> not ((x = tag_label) || (x = amount_label) || (x = recipient_label) || x = accepted_label)) in
    `Assoc [(tag_label, `String (BatOption.get tags)); 
                 (amount_label, `String (BatOption.get amounts));
                 (accepted_label, `String (BatOption.get accepteds));
                 (tofrom_label, `String (BatOption.get tofroms));
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

module ContractInfo = struct

let get_string (contr : 'rep contract) =
  (* 1. contract name *)
  let namej = ("name", `String (get_id contr.cname)) in
  (* 2. parameters *)
  let paraml = contr.cparams in
  let paramlj = List.map paraml ~f: (fun (i, t) ->
    `Assoc [("name", `String (get_id i)); ("type", `String (typ_to_string t))]) in
  let paramj = ("params", `List paramlj) in
  (* 3. fields *)
  let fieldsl = contr.cfields in
  let fieldslj = List.map fieldsl ~f: (fun (i, t, _) ->
    `Assoc [("name", `String (get_id i)); ("type", `String (typ_to_string t))]) in
  let fieldsj = ("fields", `List fieldslj) in
  (* 4. transitions *)
  let transl = contr.ctrans in
  let translj = List.map transl ~f: (fun t ->
    (* 4a. transition name *)
    let namej = ("name", `String (get_id t.tname)) in
    (* 4b. transition parameters *)
    let paraml = t.tparams in
    let paramlj = List.map paraml ~f: (fun (i, t) ->
      `Assoc[("name", `String (get_id i)); ("type", `String (typ_to_string t))]) in
    let paramj = ("params", `List paramlj) in
      `Assoc (namej :: paramj :: [] )) in

  let transj = ("transitions", `List translj) in
  let finalj = `Assoc (namej :: paramj :: fieldsj :: transj :: []) in
  pretty_to_string finalj

end
