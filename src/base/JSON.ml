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

open Core_kernel
open Literal
open Syntax
open ErrorUtils
open Yojson
open ContractUtil.MessagePayload
open Datatypes
open TypeUtil
open BuiltIns
open PrettyPrinters
module JSONTypeUtilities = TypeUtilities
module JSONLiteral = GlobalLiteral
module JSONType = JSONLiteral.LType
module JSONIdentifier = JSONType.TIdentifier
module JSONName = JSONIdentifier.Name
module JSONBuiltIns =
  ScillaBuiltIns (ParserUtil.ParserRep) (ParserUtil.ParserRep)
module JSONFrontEndParser = FrontEndParser.ScillaFrontEndParser (JSONLiteral)
open JSONTypeUtilities
open JSONIdentifier
open JSONType
open JSONLiteral

type json_parsed_field =
  (* A field belonging to this contract. *)
  | ThisContr of string * JSONType.t * JSONLiteral.t
  (* External contracts and their fields. *)
  | ExtrContrs of (Bystrx.t * (string * JSONType.t * JSONLiteral.t) list) list

(****************************************************************)
(*                    Exception wrappers                        *)
(****************************************************************)

let json_exn_wrapper = JSONParser.json_exn_wrapper

let member_exn = JSONParser.member_exn

let to_string_exn = JSONParser.to_string_exn

let constr_pattern_arg_types_exn = JSONParser.constr_pattern_arg_types_exn

let from_file f =
  let thunk () = Basic.from_file f in
  json_exn_wrapper thunk ~filename:f

let parse_as_name n =
  match String.split_on_chars ~on:[ '.' ] n with
  | [ t1; t2 ] -> JSONName.parse_qualified_name t1 t2
  | [ t1 ] -> JSONName.parse_simple_name t1
  | _ -> raise (mk_invalid_json ~kind:"Invalid name in json" ~inst:n)

let parse_typ_exn t =
  match JSONFrontEndParser.parse_type t with
  | Error _ -> raise (mk_invalid_json ~kind:"Invalid type in json" ~inst:t)
  | Ok s -> s

let to_list_exn j =
  let thunk () = Basic.Util.to_list j in
  json_exn_wrapper thunk

let to_assoc_exn j =
  let thunk () = Basic.Util.to_assoc j in
  json_exn_wrapper thunk

let build_prim_lit_exn t v =
  let exn () =
    mk_invalid_json ~kind:"Invalid value in JSON"
      ~inst:(v ^ " of type " ^ pp_typ t)
  in
  let build_prim_literal_of_type t v =
    match build_prim_literal t v with Some v' -> v' | None -> raise (exn ())
  in
  match t with
  | PrimType pt -> build_prim_literal_of_type pt v
  | Address _ -> build_prim_literal_of_type (Bystrx_typ Type.address_length) v
  | MapType _ | FunType _ | ADT _ | TypeVar _ | PolyFun _ | Unit ->
      raise (exn ())

(****************************************************************)
(*                    JSON parsing                              *)
(****************************************************************)

let rec json_to_adttyps tjs =
  match tjs with
  | tj :: tr ->
      let tjs = to_string_exn tj in
      let t = parse_typ_exn tjs in
      let trem = json_to_adttyps tr in
      t :: trem
  | _ -> []

let rec json_to_adtargs cname tlist ajs =
  let verify_args_exn cname provided expected =
    if provided <> expected then
      let p = Int.to_string provided in
      let e = Int.to_string expected in
      raise
        (mk_invalid_json ~kind:"Malformed ADT constructor"
           ~inst:
             (JSONName.as_error_string cname
             ^ ": expected " ^ e ^ " args, but provided " ^ p ^ "."))
  in
  let dt =
    match DataTypeDictionary.lookup_constructor cname with
    | Error emsg -> raise (Invalid_json emsg)
    | Ok (r, _) -> r
  in
  (* For each component literal of our ADT, calculate its type.
   * This is essentially using DataTypes.constr_tmap and substituting safely. *)
  let tmap =
    constr_pattern_arg_types_exn (ADT (mk_loc_id dt.tname, tlist)) cname
  in
  verify_args_exn cname (List.length ajs) (List.length tmap);
  let llist = List.map2_exn tmap ajs ~f:(fun t j -> json_to_lit_exn t j) in
  ADTValue (cname, tlist, llist)

and read_adt_json name j tlist_verify =
  let dt =
    match DataTypeDictionary.lookup_name name with
    | Error emsg -> raise (Invalid_json emsg)
    | Ok r -> r
  in
  let res =
    match j with
    | `List vli ->
        (* We make an exception for Lists, allowing them to be stored flatly. *)
        if not (is_list_adt_name dt.tname) then
          raise
            (Invalid_json
               (mk_error0
                  ~kind:"ADT value is a JSON array, but type is not List"
                  ?inst:None))
        else
          let etyp = List.nth_exn tlist_verify 0 in
          List.fold_right vli
            ~f:(fun vl acc -> build_cons_lit (json_to_lit_exn etyp vl) etyp acc)
            ~init:(build_nil_lit etyp)
    | `Assoc _ ->
        let constr_str = member_exn "constructor" j |> to_string_exn in
        let constr = parse_as_name constr_str in
        let dt' =
          match DataTypeDictionary.lookup_constructor constr with
          | Error emsg -> raise (Invalid_json emsg)
          | Ok (r, _) -> r
        in
        if not @@ [%equal: Datatypes.adt] dt dt' then
          raise
            (mk_invalid_json ~kind:"ADT type does not match constructor"
               ~inst:
                 (JSONName.as_error_string dt.tname
                 ^ " does not match constructor "
                 ^ JSONName.as_error_string constr));
        let argtypes = member_exn "argtypes" j |> to_list_exn in
        let arguments = member_exn "arguments" j |> to_list_exn in
        let tlist = json_to_adttyps argtypes in
        json_to_adtargs constr tlist arguments
    | _ ->
        raise
          (mk_invalid_json ~kind:"JSON parsing: error parsing ADT"
             ~inst:(JSONName.as_error_string name))
  in
  (* match tlist1 with adt's tlist. *)
  let verify_exn name tlist1 adt =
    match adt with
    | ADTValue (_, tlist2, _) ->
        (* Type arguments must be identical, not assignable *)
        if type_equiv_list ~to_list:tlist1 ~from_list:tlist2 then ()
        else
          let expected = pp_typ_list_error tlist1 in
          let observed = pp_typ_list_error tlist2 in
          raise
            (mk_invalid_json ~kind:"Type mismatch in parsing ADT"
               ~inst:
                 (JSONName.as_error_string name
                 ^ ". Expected: " ^ expected ^ " vs Observed: " ^ observed))
    | _ ->
        raise
          (mk_invalid_json ~kind:"Type mismatch in parsing ADT"
             ~inst:(JSONName.as_error_string name))
  in
  (* verify built ADT *)
  verify_exn name tlist_verify res;
  (* return built ADT *)
  res

(* Map is a `List of `Assoc jsons, with
 * the first `Assoc specifying the map's from/to types.*)
and read_map_json kt vt j =
  match j with
  | `List vli ->
      let m = Caml.Hashtbl.create (List.length vli) in
      let _ = mapvalues_from_json m kt vt vli in
      Map ((kt, vt), m)
  | `Null -> Map ((kt, vt), Caml.Hashtbl.create 0)
  | _ ->
      raise (mk_invalid_json ~kind:"JSON parsing: error parsing Map" ?inst:None)

and mapvalues_from_json m kt vt l =
  List.iter l ~f:(fun first ->
      let kjson = member_exn "key" first in
      let keylit =
        match kt with
        | PrimType _ | Address _ ->
            (* Addresses are handled as ByStr20 *)
            build_prim_lit_exn kt (to_string_exn kjson)
        | _ ->
            raise
              (mk_invalid_json ~kind:"Key in Map JSON is not a PrimType"
                 ?inst:None)
      in
      let vjson = member_exn "val" first in
      let vallit = json_to_lit_exn vt vjson in
      Caml.Hashtbl.replace m keylit vallit)

and json_to_lit_exn t v =
  match t with
  | MapType (kt, vt) ->
      let vl = read_map_json kt vt v in
      vl
  | ADT (name, tlist) ->
      let vl = read_adt_json (get_id name) v tlist in
      vl
  | PrimType _ | Address _ ->
      let tv = build_prim_lit_exn t (to_string_exn v) in
      tv
  | FunType _ | TypeVar _ | PolyFun _ | Unit ->
      let exn () =
        mk_invalid_json ~kind:"Invalid type in JSON" ~inst:(pp_typ t)
      in
      raise (exn ())

let rec jobj_to_statevar json =
  let n = member_exn "vname" json |> to_string_exn in
  let v = member_exn "value" json in
  if String.equal n "_external" then
    (* We have a list of external addresses, each with their own fields. *)
    let exts = v |> to_list_exn in
    let exts' =
      List.fold exts ~init:[] ~f:(fun acc ext ->
          let addr =
            member_exn "address" ext |> to_string_exn |> Bystrx.parse_hex
          in
          let state = member_exn "state" ext |> to_list_exn in
          let state' =
            List.map state ~f:(fun s ->
                match jobj_to_statevar s with
                | ThisContr (n, t, l) -> (n, t, l)
                | _ ->
                    raise
                    @@ mk_invalid_json
                         ~kind:
                           "External contract fields cannot contain other \
                            external fields"
                         ?inst:None)
          in
          (addr, state') :: acc)
    in
    ExtrContrs exts'
  else
    let tstring = member_exn "type" json |> to_string_exn in
    let t = parse_typ_exn tstring in
    if GlobalConfig.validate_json () then ThisContr (n, t, json_to_lit_exn t v)
    else ThisContr (n, t, JSONParser.parse_json t v)

(****************************************************************)
(*                    JSON printing                             *)
(****************************************************************)

let state_to_json state =
  let vname, typ, lit = state in
  `Assoc
    [
      ("vname", `String vname);
      ("type", `String (pp_typ typ));
      ("value", literal_to_json lit);
    ]

let rec slist_to_json l =
  match l with
  | [] -> []
  | s :: remaining ->
      let sj = state_to_json s in
      let remj = slist_to_json remaining in
      sj :: remj

let get_string_literal l = match l with StringLit sl -> Some sl | _ -> None

let get_uint_literal l =
  match l with UintLit il -> Some (string_of_uint_lit il) | _ -> None

let get_address_literal l =
  match l with
  | ByStrX bs when Bystrx.width bs = Type.address_length ->
      Some (Bystrx.hex_encoding bs)
  | _ -> None

(****************************************************************)
(*               JSON Utilities Entry Points                    *)
(****************************************************************)

module ContractState = struct
  (** Returns a list of (vname:string,value:literal) items
      Invalid inputs in the json are ignored **)
  let get_json_data filename =
    let json = from_file filename in
    (* input json is a list of key/value pairs *)
    let jlist = json |> to_list_exn in
    let curstates, extstates =
      List.partition_map jlist ~f:(fun j ->
          match jobj_to_statevar j with
          | ThisContr (n, t, v) -> First (n, t, v)
          | ExtrContrs extlist -> Second extlist)
    in
    (curstates, List.concat extstates)

  (* Get a json object from given states *)
  let state_to_json states =
    let states_str =
      List.map states ~f:(fun (x, t, v) -> (JSONName.as_string x, t, v))
    in
    let jsonl = slist_to_json states_str in
    `List jsonl

  (** 
   ** Prints a list of state variables (string, literal)
   ** as a json and returns it as a string.
   ** pp enables pretty printing.
   **)
  let state_to_string ?(pp = false) states =
    let json = state_to_json states in
    if pp then pretty_to_string json else to_string json

  (* Given the json data from an init file, return extlib fields *)
  let get_init_extlibs init_data =
    let extlibs =
      List.filter init_data ~f:(fun (name, _t, _v) ->
          String.(name = JSONName.as_string ContractUtil.extlibs_label))
    in
    match extlibs with
    | [] -> []
    | [ (_name, _typ, lit) ] -> (
        match Datatypes.scilla_list_to_ocaml lit with
        | Error _ ->
            raise
              (mk_invalid_json ~kind:"Invalid entry in init json"
                 ~inst:(JSONName.as_error_string ContractUtil.extlibs_label))
        | Ok lit' ->
            (* lit' is a list of `Pair` literals. convert them to OCaml pairs. *)
            List.map lit' ~f:(fun sp ->
                match sp with
                | ADTValue (c, [ t1; t2 ], [ StringLit name; ByStrX bs ])
                  when is_pair_ctr_name c
                       && [%equal: JSONType.t] t1 JSONType.string_typ
                       && [%equal: JSONType.t] t2
                            (JSONType.bystrx_typ Type.address_length)
                       && Bystrx.width bs = Type.address_length ->
                    (name, Bystrx.hex_encoding bs)
                | _ ->
                    raise
                      (mk_invalid_json ~kind:"Invalid entry in init json"
                         ~inst:
                           (JSONName.as_error_string ContractUtil.extlibs_label)))
        )
    | _ ->
        raise
          (mk_invalid_json ~kind:"Multiple entries in init json"
             ~inst:(JSONName.as_error_string ContractUtil.extlibs_label))

  (* Accessor for _this_address and _extlibs entries in init.json.
     Combined into one function to avoid reading init.json from disk multiple times. *)
  (* NOTE: The types in init files must be ignored due to backward compatibility - only the names and literals can be relied upon *)
  let get_init_this_address_and_extlibs filename =
    (* We filter out type information from init files for the time being *)
    let init_data, _ = get_json_data filename in
    let extlibs = get_init_extlibs init_data in
    let this_address_init_opt =
      match
        List.filter init_data ~f:(fun (name, _, _) ->
            String.(name = JSONName.as_string ContractUtil.this_address_label))
      with
      | [ (_, _, adr) ] -> Some adr
      | [] ->
          None
          (* We allow init files without a _this_address entry in scilla-checker *)
      | _ ->
          raise
            (mk_invalid_json ~kind:"Multiple entries specified in init json"
               ~inst:(JSONName.as_string ContractUtil.this_address_label))
    in
    match this_address_init_opt with
    | None -> (None, extlibs)
    | Some adr -> (
        match get_address_literal adr with
        | Some adr -> (Some adr, extlibs)
        | None ->
            raise
              (mk_invalid_json
                 ~kind:"Illegal type for field specified in init json"
                 ~inst:(JSONName.as_string ContractUtil.this_address_label)))

  (* Convert a single JSON serialized literal back to its Scilla value. *)
  let jstring_to_literal jstring tp =
    let thunk () = Yojson.Basic.from_string jstring in
    let jobj = json_exn_wrapper ~filename:"ipc_fetch" thunk in
    json_to_lit_exn tp jobj
end

module Message = struct
  (** Parses and returns a list of (pname,pval), with
      "_tag" and "_amount" at the beginning of this list.
      Invalid inputs in the json are ignored **)
  let get_json_data filename =
    let json = from_file filename in
    let tags = member_exn tag_label json |> to_string_exn in
    let amounts = member_exn amount_label json |> to_string_exn in
    let senders = member_exn sender_label json |> to_string_exn in
    let origins = member_exn origin_label json |> to_string_exn in
    (* Make tag, amount and sender into a literal *)
    let tag =
      (tag_label, tag_type, build_prim_lit_exn JSONType.string_typ tags)
    in
    let amount =
      (amount_label, amount_type, build_prim_lit_exn amount_type amounts)
    in
    let sender =
      (sender_label, sender_type, build_prim_lit_exn sender_type senders)
    in
    let origin =
      (origin_label, origin_type, build_prim_lit_exn origin_type origins)
    in
    let pjlist = member_exn "params" json |> to_list_exn in
    let params =
      List.map pjlist ~f:(fun f ->
          let name, t, v =
            match jobj_to_statevar f with
            | ThisContr (name, t, v) -> (name, t, v)
            | ExtrContrs _ ->
                raise
                  (mk_invalid_json
                     ~kind:"_external cannot be present in a message JSON"
                     ?inst:None)
          in
          (name, t, v))
    in
    tag :: amount :: origin :: sender :: params

  (* Same as message_to_jstring, but instead gives out raw json, not it's string *)
  let message_to_json message =
    (* extract out "_tag", "_amount", "_accepted" and "_recipient" parts of the message *)
    let taglit =
      List.find_map_exn message ~f:(fun (x, _, l) ->
          if String.(x = tag_label) then Some l else None)
    in
    let amountlit =
      List.find_map_exn message ~f:(fun (x, _, l) ->
          if String.(x = amount_label) then Some l else None)
    in
    (* message_to_json may be used to print both output and input message. Choose label accordingly. *)
    let toORfrom, tofromlit =
      List.find_map_exn message ~f:(fun (x, _, l) ->
          if String.(x = recipient_label || x = sender_label) then Some (x, l)
          else None)
    in
    let tofrom_label =
      if String.(toORfrom = recipient_label) then recipient_label
      else sender_label
    in
    let tags = get_string_literal taglit in
    let amounts = get_uint_literal amountlit in
    let tofroms = get_address_literal tofromlit in
    (* Get a list without any of these components *)
    let filtered_list =
      List.filter message ~f:(fun (x, _, _) ->
          String.(
            not (x = tag_label || x = amount_label || x = recipient_label)))
    in
    `Assoc
      [
        (tag_label, `String (Option.value_exn tags));
        (amount_label, `String (Option.value_exn amounts));
        (tofrom_label, `String (Option.value_exn tofroms));
        ("params", `List (slist_to_json filtered_list));
      ]

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
    if pp then Basic.pretty_to_string j else Basic.to_string j

  let replicate_contr_to_json m =
    let m' =
      List.filter_map m ~f:(fun (f, t, l) ->
          if String.equal f ContractUtil.MessagePayload.replicate_contr_label
          then None
          else Some (f, t, l))
    in
    `List (slist_to_json m')
end

module BlockChainState = struct
  (**  Returns bcinfo_state *)
  let get_json_data filename =
    let json = from_file filename in
    (* input json is a list of key/value pairs *)
    let jlist = json |> to_list_exn in
    let state = Caml.Hashtbl.create (List.length jlist) in
    List.iter jlist ~f:(fun j ->
        let vname = member_exn "vname" j |> to_string_exn in
        let value = member_exn "value" j in
        match vname with
        | "BLOCKNUMBER" ->
            Caml.Hashtbl.replace state ContractUtil.blocknum_name
              (let subm = Caml.Hashtbl.create 1 in
               Caml.Hashtbl.add subm "" (to_string_exn value);
               subm)
        | "CHAINID" ->
            Caml.Hashtbl.replace state ContractUtil.chainid_name
              (let subm = Caml.Hashtbl.create 1 in
               Caml.Hashtbl.add subm "" (to_string_exn value);
               subm)
        | "TIMESTAMP" ->
            let ts = value |> to_assoc_exn in
            Caml.Hashtbl.replace state ContractUtil.timestamp_name
              (let subm = Caml.Hashtbl.create (List.length ts) in
               List.iter ts ~f:(fun (bnum, timestamp) ->
                   Caml.Hashtbl.add subm bnum (to_string_exn timestamp));
               subm)
        | "REPLICATE_CONTRACT" ->
            let ts = value |> to_assoc_exn in
            Caml.Hashtbl.replace state ContractUtil.replicate_contract_name
              (let subm = Caml.Hashtbl.create (List.length ts) in
               List.iter ts ~f:(fun (addr, new_addr) ->
                   Caml.Hashtbl.add subm addr (to_string_exn new_addr));
               subm)
        | _ ->
            raise
              (mk_invalid_json
                 ~kind:("Unknown field " ^ vname ^ " in blockchain JSON")
                 ?inst:None));
    state
end

module ContractInfo = struct
  module JSONParserSyntax = ParserUtil.ParserSyntax (JSONLiteral)
  open JSONParserSyntax

  let get_json cmver (contr : contract)
      (event_info : (string * (string * JSONType.t) list) list) =
    (* 0. contract version *)
    let verj = ("scilla_major_version", `String (Int.to_string cmver)) in
    (* 1. contract name *)
    let namej = ("vname", `String (as_string contr.cname)) in
    (* 2. parameters *)
    let paraml = contr.cparams in
    let paramlj =
      List.map paraml ~f:(fun (i, t) ->
          `Assoc
            [ ("vname", `String (as_string i)); ("type", `String (pp_typ t)) ])
    in
    let paramj = ("params", `List paramlj) in
    (* 3. fields *)
    let fieldsl = contr.cfields in
    let fieldslj =
      List.map fieldsl ~f:(fun (i, t, _) ->
          `Assoc
            [
              ("vname", `String (as_string i));
              ("type", `String (pp_typ t));
              ("depth", `Int (JSONTypeUtilities.map_depth t));
            ])
    in
    let fieldsj = ("fields", `List fieldslj) in
    (* 4. transitions and procedures *)
    let transl, procedl =
      List.partition_tf contr.ccomps ~f:(fun c ->
          match c.comp_type with CompTrans -> true | CompProc -> false)
    in
    let map_comp_list_to_json cl =
      List.map cl ~f:(fun t ->
          (* 4a. component name *)
          let namej = ("vname", `String (as_string t.comp_name)) in
          (* 4b. component parameters *)
          let paraml = t.comp_params in
          let paramlj =
            List.map paraml ~f:(fun (i, t) ->
                `Assoc
                  [
                    ("vname", `String (as_string i));
                    ("type", `String (pp_typ t));
                  ])
          in
          let paramj = ("params", `List paramlj) in
          `Assoc [ namej; paramj ])
    in
    let translj, procedlj =
      (map_comp_list_to_json transl, map_comp_list_to_json procedl)
    in

    let transj, procj =
      (("transitions", `List translj), ("procedures", `List procedlj))
    in
    (* 5. event info *)
    let eventslj =
      List.map event_info ~f:(fun (eventname, plist) ->
          let namej = ("vname", `String eventname) in
          let paramlj =
            List.map plist ~f:(fun (pname, ptype) ->
                `Assoc
                  [ ("vname", `String pname); ("type", `String (pp_typ ptype)) ])
          in
          let paramj = ("params", `List paramlj) in
          `Assoc [ namej; paramj ])
    in
    let eventsj = ("events", `List eventslj) in

    (* 6. ADTs information. *)
    let adts_to_json (alist : adt list) =
      let jlist =
        List.map alist ~f:(fun a ->
            let tname = `String (JSONName.as_string a.tname) in
            let tparams = `List (List.map a.tparams ~f:(fun t -> `String t)) in
            let tmap =
              `List
                (List.map a.tconstr ~f:(fun ctr ->
                     let tsj =
                       match DataTypeDictionary.constr_tmap a ctr.cname with
                       | Some ts ->
                           `List (List.map ts ~f:(fun t -> `String (pp_typ t)))
                       | None -> `List []
                     in
                     `Assoc
                       [
                         ("cname", `String (JSONName.as_string ctr.cname));
                         ("argtypes", tsj);
                       ]))
            in
            `Assoc [ ("tname", tname); ("tparams", tparams); ("tmap", tmap) ])
      in
      `List jlist
    in
    let adtsj = ("ADTs", adts_to_json (DataTypeDictionary.get_all_adts ())) in

    let finalj =
      `Assoc [ verj; namej; paramj; fieldsj; transj; procj; eventsj; adtsj ]
    in
    finalj

  let get_string cver (contr : contract)
      (event_info : (string * (string * JSONType.t) list) list) =
    pretty_to_string (get_json cver contr event_info)
end

module Event = struct
  (* Same as Event_to_jstring, but instead gives out raw json, not it's string *)
  let event_to_json e =
    (* extract out "_eventname" from the message *)
    let eventnamelit =
      List.find_map_exn e ~f:(fun (x, _, l) ->
          if String.(x = eventname_label) then Some l else None)
    in
    let eventnames = get_string_literal eventnamelit in
    (* Get a list without the extracted components *)
    let filtered_list =
      List.filter e ~f:(fun (x, _, _) -> String.(not (x = eventname_label)))
    in
    `Assoc
      [
        (eventname_label, `String (Option.value_exn eventnames));
        ("params", `List (slist_to_json filtered_list));
      ]

  (** 
   ** Prints a Event (string, (string, literal) list) as a json to the 
   ** and returns the string. pp enables pretty printing.
   **)
  let event_to_jstring ?(pp = false) event =
    let j = event_to_json event in
    if pp then Basic.pretty_to_string j else Basic.to_string j
end

module TypeInfo = struct
  let type_info_to_json til =
    `List
      (List.map til ~f:(fun (name, t, sloc, eloc) ->
           `Assoc
             [
               ("vname", `String name);
               ("type", `String (pp_typ t));
               ("start_location", loc_to_json sloc);
               ("end_location", loc_to_json eloc);
             ]))

  let type_info_to_jstring ?(pp = false) til =
    let j = type_info_to_json til in
    if pp then Basic.pretty_to_string j else Basic.to_string j
end

module CashflowInfo = struct
  let get_json (param_field_tags, ctr_tags) =
    `Assoc
      [
        ( "State variables",
          `List
            (List.map param_field_tags ~f:(fun (i, t) ->
                 `Assoc [ ("field", `String i); ("tag", `String t) ])) );
        ( "ADT constructors",
          `List
            (List.map ctr_tags ~f:(fun (adt, ctrs) ->
                 `Assoc
                   [
                     ( adt,
                       `List
                         (List.map ctrs ~f:(fun (i, ts) ->
                              `Assoc
                                [
                                  ("constructor", `String i);
                                  ( "tags",
                                    `List (List.map ts ~f:(fun t -> `String t))
                                  );
                                ])) );
                   ])) );
      ]
end
