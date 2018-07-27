(*
 * Copyright (c) 2018 - present. 
 * Zilliqa, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open Syntax
open Core
open Yojson
open ContractUtil.MessagePayload
open Datatypes
open TypeUtil
open BuiltIns
    
exception Invalid_json of string
let addr_len = 40
let hash_len = 64

let parse_typ_exn t = 
  (try FrontEndParser.parse_type t
    with _ ->
      raise (Invalid_json (sprintf "Invalid type in json:\n%s" t)))

let member_exn m j =
  let open Basic.Util in
  let v = member m j in
  match v with
  | `Null -> raise (Invalid_json ("Member '" ^ m ^ "' not found in json"))
  | j -> j

(* Given a literal, return its full type name *)
let lit_typ_string l = match l with
  | Map ((kt, vt), _) -> pp_typ (MapType (kt, vt))
  | ADTValue (name, tl, _) -> 
      let r = DataTypeDictionary.lookup_constructor name in
      (match r with
       | Error emsg ->
           raise (Invalid_json (emsg))
       | Ok (t, _)->
           pp_typ (ADT (t.tname, tl)))
  | StringLit _ -> "String"
  | IntLit (w, _) -> "Int" ^ (Int.to_string w)
  | UintLit (w,_) -> "Uint" ^ (Int.to_string w)
  | BNum _ -> "BNum"
  | Address _ -> "Address"
  | Sha256 _ -> "Hash"
  | Msg _ -> "Message"

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
      raise (Invalid_json ("Invalid " ^ lit_typ_string n ^ " : " ^ s ^ " in json"))
     else
      (match n with
      | IntLit (wl, l) | UintLit (wl, l) ->
        (* detailed validation for integer literals *)
        if validate_int_literal n
        then
          n
        else
          raise (Invalid_json ("Invalid integer literal " ^ lit_typ_string n ^ " : " ^ s ^ " in json"))
      | _ ->
        n
      )
    )
  else
    raise (Invalid_json ("Invalid " ^ lit_typ_string n ^ " : " ^ s ^ " in json"))

let build_int_exn t v =
  let r = build_int t v in
  match r with
  | Some rs ->
    rs
  | None ->
    raise (Invalid_json("Invalid integer type/value " ^ (pp_typ t) ^ " " ^ v ^ " in json"))

let build_prim_lit_exn t v =
  let open PrimTypes in
  match t with
  | x when x = string_typ -> lit_exn (StringLit v)
  | x when x = bnum_typ -> lit_exn(BNum v)
  | x when x = address_typ -> lit_exn(Address v)
  | x when x = hash_typ -> lit_exn(Sha256 v)
  | x when is_int_type x || is_uint_type x -> build_int_exn x v
  | _ ->
      raise (Invalid_json ("JSON parsing: Invalid type for PrimType literal construction"))

let verify_adt_typs_exn name tlist1 adt =
  match adt with
  | ADTValue (_, tlist2, _) ->
    if tlist1 = tlist2 then ()
    else raise (Invalid_json ("Type mismatch in parsing ADT " ^ name))
  | _ -> raise (Invalid_json ("Type mismatch in parsing ADT " ^ name))

let rec json_to_adttyps tjs =
  let open Basic.Util in
  match tjs with
  | (tj :: tr) ->
      let tjs = to_string tj in
      let t = parse_typ_exn tjs in
      let trem = json_to_adttyps tr in
      t :: trem
  | _ -> []

let rec json_to_adtargs cname tlist ajs =
  let open Basic.Util in
  let dt =
  (match DataTypeDictionary.lookup_constructor cname with
  | Error emsg ->
    raise (Invalid_json(emsg))
  | Ok (r, _) ->
    r
  ) in
  match cname with
  | "Some" -> 
    let j = List.nth_exn ajs 0 in
    let t = List.nth_exn tlist 0 in
    let lit = json_to_lit t j in
      ADTValue (cname, tlist, (lit::[]))
  | "None" -> ADTValue (cname, tlist, [])
  | "True" | "False" -> ADTValue (cname, [], []) 
  | "Pair" ->
    let j1 = List.nth_exn ajs 0 in
    let t1 = List.nth_exn tlist 0 in
    let lit1 = json_to_lit t1 j1 in
    let j2 = List.nth_exn ajs 1 in
    let t2 = List.nth_exn tlist 1 in
    let lit2 = json_to_lit t2 j2 in
      ADTValue (cname, tlist, (lit1::lit2::[]))
  | "Nil" ->
    ADTValue (cname, tlist, [])
  | "Cons" ->
    let j1 = List.nth_exn ajs 0 in (* first element in the list *)
    let j2 = List.nth_exn ajs 1 in (* rest of the list *)
    let t = List.nth_exn tlist 0 in (* type of element of list *)
    let lit1 = json_to_lit t j1 in
    (* We know that the "rest of the list" is an ADT. *)
    let lit2 = read_adt_json dt.tname j2 in
      ADTValue (cname, tlist, (lit1::lit2::[]))
  | "Zero" ->
    ADTValue (cname, [], [])
  | "Succ" ->
    let j = List.nth_exn ajs 0 in (* successor of *)
    let lit = read_adt_json dt.tname j in
      ADTValue (cname, [], lit::[])
  | _ ->
    raise (Invalid_json ("JSON parsing: Unsupported ADT type"))

and read_adt_json name j =
  let open Basic.Util in
  let dt =
  (match DataTypeDictionary.lookup_name name with
    | Error emsg ->
      raise (Invalid_json(emsg))
    | Ok r ->
      r
    ) in
  match j with
  | `Assoc adt ->
      let constr = member_exn "constructor" j |> to_string in
      let dt' =
      (match DataTypeDictionary.lookup_constructor constr with
      | Error emsg ->
        raise (Invalid_json(emsg))
      | Ok (r, _) ->
        r
      ) in
      if (dt <> dt') then
        raise (Invalid_json ("ADT type " ^ dt.tname ^ " does not match constructor " ^ constr));
      let argtypes = member_exn "argtypes" j |> to_list in
      let arguments = member_exn "arguments" j |> to_list in
      let tlist = json_to_adttyps argtypes in
        json_to_adtargs constr tlist arguments
  | _ -> raise (Invalid_json ("JSON parsing: error parsing ADT " ^ name))

(* Map is a `List of `Assoc jsons, with
 * the first `Assoc specifying the map's from/to types.*)
and read_map_json kt vt j =
  let open Basic.Util in
  match j with
  | `List vli ->
     let kvallist = mapvalues_from_json kt vt vli in
     Map ((kt, vt), kvallist)
  | `Null -> Map ((kt, vt), [])
  | _ -> raise (Invalid_json ("JSON parsing: error parsing Map"))
 
and mapvalues_from_json kt vt l = 
  let open Basic.Util in
  match l with
  | first :: remaining ->
      let kjson = member_exn "key" first in
      let keylit = 
        (match kt with
         | PrimType t ->
            build_prim_lit_exn kt (to_string kjson)
         | _ -> raise (Invalid_json ("Key in Map JSON is not a PrimType"))
         ) in
      let vjson = member_exn "val" first in
      let vallit =
        (match vt with
         | MapType (kt', vt') ->
            read_map_json kt' vt' vjson
         | ADT (name, tlist) ->
            let vl = read_adt_json name vjson in
            verify_adt_typs_exn name tlist vl;
              vl
         | PrimType t ->
            build_prim_lit_exn vt (to_string vjson)
         | _ -> raise (Invalid_json ("Unknown type in Map value in JSON"))
        ) in
        let vlist = mapvalues_from_json kt vt remaining in
          (keylit, vallit) :: vlist
  | [] -> []

and json_to_lit t v =
  let open Basic.Util in
  match t with
  | MapType (kt, vt) ->
    let vl = read_map_json kt vt v in
      vl
  | ADT (name, tlist) ->
    let vl = read_adt_json name v in
    verify_adt_typs_exn name tlist vl;
      vl
  | _ ->  
    let tv = build_prim_lit_exn t (to_string v) in
      tv

let jobj_to_statevar json =
  let open Basic.Util in
  let n = member_exn "vname" json |> to_string in
  let tstring = member_exn "type" json |> to_string in
  let t = parse_typ_exn tstring in
  let v = member_exn "value" json in
    (n, json_to_lit t v)

let rec mapvalues_to_json ms = 
  match ms with
  | kv :: remaining ->
    let (k, v) = kv in
    let kjson = "key", (literal_to_json k) in
    let vjson = "val", (literal_to_json v) in
    let kv_json = `Assoc (kjson :: vjson :: []) in
      kv_json :: (mapvalues_to_json remaining)
  | [] -> []

and adtargs_to_json vlist =
  match vlist with
  | v1 :: vn ->
    let j2 = literal_to_json v1 in
    let jvn= adtargs_to_json vn in
      (j2 :: jvn)
  | _ -> []

and adttyps_to_json tlist =
  match tlist with
  | t1 :: tn ->
    let j1 = `String (pp_typ t1) in
    let jtn = adttyps_to_json tn in
      (j1 :: jtn)
  | _ -> []

and literal_to_json lit = 
  match lit with
  | StringLit (x) | BNum (x) | Address (x) | Sha256 (x) -> `String (x)
  | IntLit (wx, x) | UintLit (wx, x) -> `String (x)
  | Map ((kt, vt), kvs) ->
      `List (mapvalues_to_json kvs)
  | ADTValue (n, t, v) ->
      let argtl = adttyps_to_json t in
      let argl = adtargs_to_json v in
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
    ("type", `String (lit_typ_string lit));
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
    List.map jlist ~f:jobj_to_statevar

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
  let amount = (amount_label, build_int_exn PrimTypes.uint128_typ amounts) in
  let sender = (sender_label, lit_exn(Address(senders))) in
  let pjlist = member_exn "params" json |> to_list in
  let params = List.map pjlist ~f:jobj_to_statevar in
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
   **  from the json in the input filename. **)
let get_json_data filename  =
  let json = Basic.from_file filename in
  (* input json is a list of key/value pairs *)
  let jlist = json |> Basic.Util.to_list in
    List.map jlist ~f:jobj_to_statevar
  (* Validation against pre-defined block state variables
     is done in `Eval.check_blockchain_entries`  *)

end

module ContractInfo = struct

let get_string (contr : 'rep contract) =
  (* 1. contract name *)
  let namej = ("name", `String (get_id contr.cname)) in
  (* 2. parameters *)
  let paraml = contr.cparams in
  let paramlj = List.map paraml ~f: (fun (i, t) ->
    `Assoc [("name", `String (get_id i)); ("type", `String (pp_typ t))]) in
  let paramj = ("params", `List paramlj) in
  (* 3. fields *)
  let fieldsl = contr.cfields in
  let fieldslj = List.map fieldsl ~f: (fun (i, t, _) ->
    `Assoc [("name", `String (get_id i)); ("type", `String (pp_typ t))]) in
  let fieldsj = ("fields", `List fieldslj) in
  (* 4. transitions *)
  let transl = contr.ctrans in
  let translj = List.map transl ~f: (fun t ->
    (* 4a. transition name *)
    let namej = ("name", `String (get_id t.tname)) in
    (* 4b. transition parameters *)
    let paraml = t.tparams in
    let paramlj = List.map paraml ~f: (fun (i, t) ->
      `Assoc[("name", `String (get_id i)); ("type", `String (pp_typ t))]) in
    let paramj = ("params", `List paramlj) in
      `Assoc (namej :: paramj :: [] )) in

  let transj = ("transitions", `List translj) in
  let finalj = `Assoc (namej :: paramj :: fieldsj :: transj :: []) in
  pretty_to_string finalj

end
