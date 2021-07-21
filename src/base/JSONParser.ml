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

open Core_kernel
open Yojson
open ErrorUtils
open TypeUtil
open Datatypes
module JSONTypeUtilities = TypeUtilities
module JSONIdentifier = TypeUtil.TUIdentifier
module JSONName = JSONIdentifier.Name
module JSONType = TypeUtil.TUType
module JSONSanitisedLiteral = TypeUtil.TULiteral
open JSONTypeUtilities

(* Specialised literal type for JSON parsing. Needed in order to parse unrecognised ADT literals *)
type json_literal =
  | StringLit of string
  (* Cannot have different integer literals here directly as Stdint does not derive sexp. *)
  | IntLit of JSONSanitisedLiteral.int_lit
  | UintLit of JSONSanitisedLiteral.uint_lit
  | BNum of string
  (* Byte string with a statically known length. *)
  | ByStrX of JSONSanitisedLiteral.Bystrx.t
  (* Byte string without a statically known length. *)
  | ByStr of JSONSanitisedLiteral.Bystr.t
  (* Message: an associative array *)
  | Msg of (string * JSONType.t * json_literal) list
  (* A dynamic map of literals *)
  | Map of JSONSanitisedLiteral.mtype * (json_literal, json_literal) Sexplib.Std.Hashtbl.t
  (* A constructor in HNF *)
  | ADTValue of JSONIdentifier.Name.t * JSONType.t list * json_literal list
  | Unrecognised of string

let build_nil_lit t =
  ADTValue (JSONName.parse_simple_name "Nil", [ t ], [])

let build_cons_lit hd t tl =
  ADTValue (JSONName.parse_simple_name "Cons", [ t ], [ hd; tl ])

let rec sanitise_literal = function
  | StringLit s -> JSONSanitisedLiteral.StringLit s
  | IntLit i -> JSONSanitisedLiteral.IntLit i
  | UintLit i -> JSONSanitisedLiteral.UintLit i
  | BNum s -> JSONSanitisedLiteral.BNum s
  | ByStrX s -> JSONSanitisedLiteral.ByStrX s
  | ByStr s -> JSONSanitisedLiteral.ByStr s
  | Msg msg -> JSONSanitisedLiteral.Msg (List.map msg ~f:(fun (tag, typ, l) -> (tag, typ, sanitise_literal l)))
  | Map (mtyp, tbl) ->
      let new_tbl = Sexplib.Std.Hashtbl.create (Sexplib.Std.Hashtbl.length tbl) in
      let () = Sexplib.Std.Hashtbl.iter (fun k v -> Sexplib.Std.Hashtbl.replace new_tbl (sanitise_literal k) (sanitise_literal v)) tbl in
      JSONSanitisedLiteral.Map (mtyp, new_tbl)
  | ADTValue (c, ts, ls) -> JSONSanitisedLiteral.ADTValue (c, ts, List.map ls ~f:sanitise_literal)
  | Unrecognised s ->
      printf "%s\n" s;
      raise (mk_invalid_json (Printf.sprintf "Unrecognised ADT constructor in JSON: %s" s))

(*************************************)
(***** Exception and wrappers ********)
(*************************************)

let json_exn_wrapper ?filename thunk =
  try thunk () with
  | Yojson.Json_error s
  | Yojson.Basic.Util.Undefined (s, _)
  | Yojson.Basic.Util.Type_error (s, _) ->
      raise (mk_invalid_json s)
  | _ -> (
      match filename with
      | Some f ->
          raise
            (mk_invalid_json (Printf.sprintf "Unknown error parsing JSON %s" f))
      | None ->
          raise (mk_invalid_json (Printf.sprintf "Unknown error parsing JSON")))

let member_exn m j =
  let thunk () = Basic.Util.member m j in
  let v = json_exn_wrapper thunk in
  match v with
  | `Null -> raise (mk_invalid_json ("Member '" ^ m ^ "' not found in json"))
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

(*************************************)
(*********** ADT parsers *************)
(*************************************)

type adt_parser_entry =
  | Incomplete (* Parser not completely constructed. *)
  | Parser of (Basic.t -> json_literal)

let adt_parsers =
  let open Caml in
  let ht : (string, adt_parser_entry) Hashtbl.t = Hashtbl.create 10 in
  ht

let add_adt_parser adt_name parser =
  let open Caml in
  let _ = Hashtbl.replace adt_parsers adt_name parser in
  ()

let lookup_adt_parser_opt adt_name =
  let open Caml in
  Hashtbl.find_opt adt_parsers adt_name

let lookup_adt_parser adt_name =
  let open Caml in
  match Hashtbl.find_opt adt_parsers adt_name with
  | None -> raise (mk_invalid_json (sprintf "ADT %s not found" adt_name))
  | Some p -> p

(*************************************)
(******** Parser Generator ***********)
(*************************************)

(* Generate a parser. *)
let gen_parser (t' : JSONType.t) : Basic.t -> json_literal =
  let open Basic in
  let open TUType in
  let rec recurser t =
    match t with
    | PrimType pt -> (
        match pt with
        | String_typ -> fun j -> StringLit (to_string_exn j)
        | Bnum_typ -> fun j -> BNum (to_string_exn j)
        | Bystr_typ -> fun j -> ByStr (TULiteral.Bystr.parse_hex (to_string_exn j))
        | Bystrx_typ _ -> fun j -> ByStrX (TULiteral.Bystrx.parse_hex (to_string_exn j))
        | Int_typ Bits32 ->
            fun j -> IntLit (Int32L (Int32.of_string (to_string_exn j)))
        | Int_typ Bits64 ->
            fun j -> IntLit (Int64L (Int64.of_string (to_string_exn j)))
        | Int_typ Bits128 ->
            fun j ->
              IntLit (Int128L (Stdint.Int128.of_string (to_string_exn j)))
        | Int_typ Bits256 ->
            fun j ->
              IntLit (Int256L (Integer256.Int256.of_string (to_string_exn j)))
        | Uint_typ Bits32 ->
            fun j ->
              UintLit (Uint32L (Stdint.Uint32.of_string (to_string_exn j)))
        | Uint_typ Bits64 ->
            fun j ->
              UintLit (Uint64L (Stdint.Uint64.of_string (to_string_exn j)))
        | Uint_typ Bits128 ->
            fun j ->
              UintLit (Uint128L (Stdint.Uint128.of_string (to_string_exn j)))
        | Uint_typ Bits256 ->
            fun j ->
              UintLit
                (Uint256L (Integer256.Uint256.of_string (to_string_exn j)))
        | _ -> raise (mk_invalid_json "Invalid primitive type"))
    | MapType (kt, vt) -> (
        let kp = recurser kt in
        let vp = recurser vt in
        fun j ->
          match j with
          | `List jlist ->
              let m = Caml.Hashtbl.create (List.length jlist) in
              List.iter jlist ~f:(fun first ->
                  let kjson = member_exn "key" first in
                  let keylit = kp kjson in
                  let vjson = member_exn "val" first in
                  let vallit = vp vjson in
                  Caml.Hashtbl.replace m keylit vallit);
              Map ((kt, vt), m)
          | _ -> raise (mk_invalid_json "Invalid map in JSON"))
    | ADT (name, tlist) ->
        (* Add a dummy entry for "t" in our table, to prevent recursive calls. *)
        let _ = add_adt_parser (pp_typ t) Incomplete in

        let a = lookup_adt_name_exn name in
        (* Build a parser for each constructor of this ADT. *)
        let cn_parsers =
          List.fold a.tconstr ~init:(AssocDictionary.make_dict ())
            ~f:(fun maps cn ->
              let tmap = constr_pattern_arg_types_exn t cn.cname in
              let arg_parsers =
                List.map tmap ~f:(fun t ->
                    match lookup_adt_parser_opt (pp_typ t) with
                    | Some _ ->
                        (* Lazy lookup, to avoid using dummy parsers set above. *)
                        fun () -> lookup_adt_parser (pp_typ t)
                    | None ->
                        let p = recurser t in
                        fun () -> Parser p)
              in
              let parser j =
                match j with
                | `Assoc _ ->
                    let arguments = member_exn "arguments" j |> Util.to_list in
                    if List.length tmap <> List.length arguments then
                      raise (mk_invalid_json "Invalid arguments to ADT in JSON")
                    else
                      let arg_lits =
                        List.map2_exn arg_parsers arguments ~f:(fun p a ->
                            (* Apply thunk, and then apply to argument *)
                            match p () with
                            | Incomplete ->
                                raise
                                  (mk_invalid_json
                                     "Attempt to call an incomplete JSON parser")
                            | Parser p' -> p' a)
                      in
                      ADTValue (cn.cname, tlist, arg_lits)
                | `List vli ->
                    (* We make an exception for Lists, allowing them to be stored flatly. *)
                    if
                      not
                        (Datatypes.is_list_adt_name
                           (JSONIdentifier.get_id name))
                    then
                      raise
                        (mk_invalid_json
                           "ADT value is a JSON array, but type is not List")
                    else
                      let eparser = List.nth_exn arg_parsers 0 in
                      let eparser' =
                        match eparser () with
                        | Incomplete ->
                            raise
                              (mk_invalid_json
                                 "Attempt to call an incomplete JSON parser")
                        | Parser p' -> p'
                      in
                      let etyp = List.nth_exn tmap 0 in
                      List.fold_right vli
                        ~f:(fun vl acc ->
                          (* Apply eparser thunk, and then apply to argument *)
                          build_cons_lit (eparser' vl) etyp acc)
                        ~init:(build_nil_lit etyp)
                | _ -> raise (mk_invalid_json "Invalid ADT in JSON")
              in
              AssocDictionary.insert (JSONName.as_string cn.cname) parser maps)
        in
        let adt_parser cn_parsers j =
          let cn =
            match j with
            | `Assoc _ -> member_exn "constructor" j |> to_string_exn
            | `List _ ->
                "Cons" (* for efficiency, Lists can be stored flatly. *)
            | _ -> raise (mk_invalid_json "Invalid construct in ADT JSON")
          in
          match AssocDictionary.lookup cn cn_parsers with
          | Some parser -> parser j
          | None ->
              raise
                (mk_invalid_json ("Unknown constructor " ^ cn ^ " in ADT JSON"))
        in
        (* Create parser *)
        let p = adt_parser cn_parsers in
        (* Add parser to hashtable *)
        let _ = add_adt_parser (pp_typ t) (Parser p) in
        (* Return parser *)
        p
    | _ -> raise (mk_invalid_json "Invalid type")
  in
  recurser t'

let parse_json t j = (gen_parser t) j
