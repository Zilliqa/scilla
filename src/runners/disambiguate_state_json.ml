(*
  This file is part of scilla.

  Copyright (c) 2020 - present Zilliqa Research Pvt. Ltd.

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

open Core
open Scilla_base
open ErrorUtils
open DebugMessage
open PrettyPrinters
open Literal
open Yojson
module InputFEParser = FrontEndParser.ScillaFrontEndParser (LocalLiteral)
module InputSyntax = InputFEParser.FESyntax
module InputLiteral = InputSyntax.SLiteral
module InputType = InputSyntax.SType
module InputIdentifier = InputSyntax.SIdentifier
module InputName = InputIdentifier.Name
module OutputLiteral = GlobalLiteral
module OutputType = OutputLiteral.LType
module OutputIdentifier = OutputType.TIdentifier
module OutputName = OutputIdentifier.Name

module OutputStateService = Scilla_eval.StateService.MakeStateService ()

(* ************************************************************************ * 
 * This executable parses a contract and reads its associated init and      *
 * state JSONs using local names, and outputs its init and state JSONs      *
 * using global names. The state itself does not change, only the type and  * 
 * constructor names that appear in the state.                              *
 *                                                                          *
 * This transformation is necessary in order to enable external libraries   *
 * and remote state reads.                                                  *
 * ************************************************************************ *)

(* RunnerCLI.ml *)

type args = {
  input_init : string;
  input_state : string;
  output_init : string;
  output_state : string;
  input : string;
  ipc_address : string;
}

let f_input_init = ref ""

let f_input_state = ref ""

let f_output_init = ref ""

let f_output_state = ref ""

let f_input = ref ""

let i_ipc_address = ref ""

let reset () =
  f_input_init := "";
  f_input_state := "";
  f_output_init := "";
  f_output_state := "";
  f_input := "";
  i_ipc_address := ""

let validate_main usage =
  let open Core_kernel in
  let msg = "" in
  let msg =
    (* input_init.json is mandatory *)
    if not @@ Sys.file_exists !f_input_init then
      "Invalid input initialization file\n"
    else msg
  in
  let msg =
    (* input file is mandatory *)
    if not @@ Sys.file_exists !f_input then
      msg ^ "Invalid input contract file\n"
    else msg
  in
  let msg =
    (* output_init file is mandatory *)
    if String.is_empty !f_output_init then
      msg ^ "Output initialization file must be specified\n"
    else msg
  in
  let msg =
    (* Either an input state or an ipc address must be provided *)
    if (not @@ Sys.file_exists !f_input_state) && String.is_empty !i_ipc_address
    then
      msg ^ "Either an input state file or an ipc address must be specified\n"
    else msg
  in
  let msg =
    (* If ostate is given, then input_state.json is mandatory, because the balance cannot be read using ipc *)
    if
      (not @@ String.is_empty !f_output_state)
      && (not @@ Sys.file_exists !f_input_state)
    then
      msg ^ "Input state file is mandatory if output state file is expected\n"
    else msg
  in
  if not @@ String.is_empty msg then
    PrettyPrinters.fatal_error_noformat (usage ^ Printf.sprintf "%s\n" msg)

let parse ~exe_name =
  reset ();
  let speclist =
    [
      ( "-iinit",
        Arg.String (fun x -> f_input_init := x),
        "Path to initialization json" );
      ( "-istate",
        Arg.String (fun x -> f_input_state := x),
        "Path to state input json" );
      ("-i", Arg.String (fun x -> f_input := x), "Path to scilla contract");
      ( "-oinit",
        Arg.String (fun x -> f_output_init := x),
        "Path to output init json" );
      ( "-ostate",
        Arg.String (fun x -> f_output_state := x),
        "Path to output state json" );
      ( "-ipcaddress",
        Arg.String (fun x -> i_ipc_address := x),
        "IPC socket address" );
    ]
  in

  let mandatory_usage =
    "Usage:\n" ^ exe_name ^ " -iinit input_init.json "
    ^ " [ -istate input_state.json ] [ -ipcaddress ipcaddress ]"
    ^ " -oinit output_init.json [-ostate output_state.json] -i input.scilla"
    ^ "\n"
  in
  let ignore_anon _ = () in
  let usage = mandatory_usage ^ "\n " in
  let () = Arg.parse speclist ignore_anon mandatory_usage in
  let () = validate_main usage in
  {
    input_init = !f_input_init;
    input_state = !f_input_state;
    output_init = !f_output_init;
    output_state = !f_output_state;
    input = !f_input;
    ipc_address = !i_ipc_address;
  }

(* TypeUtil.ml *)

(* Copied/reimplemented from Disambiguate.ml *)

(* Qualify a local simple name with this_address *)
let disambiguate_name name this_address =
  let open Identifier.LocalName in
  match name with
  | SimpleLocal nm -> Identifier.GlobalName.parse_qualified_name this_address nm
  | QualifiedLocal _ ->
      let msg =
        sprintf "Unexpected qualified local name %s\n" (as_error_string name)
      in
      plog msg;
      fatal_error (mk_error0 msg)

let convert_simple_name_to_simple_name name =
  let open Identifier.LocalName in
  match name with
  | SimpleLocal nm -> Identifier.GlobalName.parse_simple_name nm
  | QualifiedLocal _ ->
      let msg =
        sprintf "Unexpected qualified local name %s\n" (as_error_string name)
      in
      plog msg;
      fatal_error (mk_error0 msg)

let disambiguate_dt_dictionary_name name this_address lookup =
  let open Identifier.LocalName in
  match name with
  | QualifiedLocal _ ->
      raise
        (mk_invalid_json
           (sprintf "Found qualified type name %s in file"
              (InputName.as_error_string name)))
  | SimpleLocal nm -> (
      (* Try nm as a simple global name *)
      let tmp_nm = OutputName.parse_simple_name nm in
      match lookup tmp_nm with
      | Ok _ ->
          (* Name exists => Name is predefined *)
          tmp_nm
      | Error _ ->
          (* Name does not exist => Name is user-defined *)
          OutputName.parse_qualified_name this_address nm )

let disambiguate_adt_name name this_address =
  disambiguate_dt_dictionary_name name this_address
    Datatypes.DataTypeDictionary.lookup_name

let disambiguate_ctr_name name this_address =
  disambiguate_dt_dictionary_name name this_address
    Datatypes.DataTypeDictionary.lookup_constructor

let disambiguate_type t this_address =
  let open InputType in
  let rec recurse t =
    match t with
    | PrimType pt -> OutputType.PrimType pt
    | MapType (kt, vt) ->
        let dis_kt = recurse kt in
        let dis_vt = recurse vt in
        OutputType.MapType (dis_kt, dis_vt)
    | FunType (arg_t, res_t) ->
        let dis_arg_t = recurse arg_t in
        let dis_res_t = recurse res_t in
        OutputType.FunType (dis_arg_t, dis_res_t)
    | ADT (t_name, targs) ->
        let dis_nm =
          disambiguate_adt_name (InputIdentifier.get_id t_name) this_address
        in
        let dis_t_name =
          OutputIdentifier.mk_id dis_nm (InputIdentifier.get_rep t_name)
        in
        let dis_targs = List.map targs ~f:recurse in
        OutputType.ADT (dis_t_name, dis_targs)
    | TypeVar tvar -> OutputType.TypeVar tvar
    | PolyFun (tvar, t) ->
        let dis_t = recurse t in
        OutputType.PolyFun (tvar, dis_t)
    | Unit -> OutputType.Unit
    | Address _ ->
        fatal_error
          (mk_error0 "Address type found in state to be disambiguated\n")
  in
  recurse t

(* JSONParser.ml *)

let json_exn_wrapper = JSONParser.json_exn_wrapper

let member_exn = JSONParser.member_exn

let to_string_exn = JSONParser.to_string_exn

let constr_pattern_arg_types_exn = JSONParser.constr_pattern_arg_types_exn

let lookup_adt_name_exn = JSONParser.lookup_adt_name_exn

let add_adt_parser = JSONParser.add_adt_parser

let lookup_adt_parser_opt = JSONParser.lookup_adt_parser_opt

let lookup_adt_parser = JSONParser.lookup_adt_parser

(* Generate a parser. Parse directly into OutputLiteral *)
let gen_parser (t' : OutputType.t) (this_address : string) :
    Basic.t -> OutputLiteral.t =
  let open Basic in
  let open OutputType in
  let open OutputLiteral in
  let rec recurser t =
    match t with
    | PrimType pt -> (
        match pt with
        | String_typ -> fun j -> StringLit (to_string_exn j)
        | Bnum_typ -> fun j -> BNum (to_string_exn j)
        | Bystr_typ -> fun j -> ByStr (Bystr.parse_hex (to_string_exn j))
        | Bystrx_typ _ -> fun j -> ByStrX (Bystrx.parse_hex (to_string_exn j))
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
        | _ -> raise (mk_invalid_json "Invalid primitive type") )
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
          | _ -> raise (mk_invalid_json "Invalid map in JSON") )
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
                           (OutputIdentifier.get_id name))
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
              AssocDictionary.insert (OutputName.as_string cn.cname) parser maps)
        in
        let adt_parser cn_parsers j =
          let cn =
            match j with
            | `Assoc _ -> member_exn "constructor" j |> to_string_exn
            | `List _ ->
                "Cons" (* for efficiency, Lists can be stored flatly. *)
            | _ -> raise (mk_invalid_json "Invalid construct in ADT JSON")
          in
          (* cn is a local name. Disambiguate before lookup *)
          let dis_cn =
            disambiguate_ctr_name (InputName.parse_simple_name cn) this_address
          in
          match
            AssocDictionary.lookup (OutputName.as_string dis_cn) cn_parsers
          with
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

(* JSON.ml *)

(* Changed to read local type names *)
let parse_typ_exn t =
  match InputFEParser.parse_type t with
  | Error _ -> raise (mk_invalid_json (sprintf "Invalid type in json: %s\n" t))
  | Ok s -> s

let build_prim_lit_exn t v =
  let exn () =
    mk_invalid_json
      ("Invalid " ^ OutputType.pp_typ t ^ " value " ^ v ^ " in JSON")
  in
  match t with
  | OutputType.PrimType pt -> (
      match OutputLiteral.build_prim_literal pt v with
      | Some v' -> v'
      | None -> raise (exn ()) )
  | _ -> raise (exn ())

let rec json_to_adttyps tjs =
  match tjs with
  | tj :: tr ->
      let tjs = to_string_exn tj in
      let t = parse_typ_exn tjs in
      let trem = json_to_adttyps tr in
      t :: trem
  | _ -> []

let rec json_to_adtargs cname tlist this_address ajs =
  let dt =
    match Datatypes.DataTypeDictionary.lookup_constructor cname with
    | Error emsg -> raise (Invalid_json emsg)
    | Ok (r, _) -> r
  in
  (* For each component literal of our ADT, calculate its type.
   * This is essentially using DataTypes.constr_tmap and substituting safely. *)
  let tmap =
    constr_pattern_arg_types_exn
      (ADT (OutputIdentifier.mk_loc_id dt.tname, tlist))
      cname
  in
  let llist =
    List.map2_exn tmap ajs ~f:(fun t j -> json_to_lit t this_address j)
  in
  OutputLiteral.ADTValue (cname, tlist, llist)

and read_adt_json name this_address j tlist_verify =
  let open OutputLiteral in
  let res =
    match j with
    | `List vli ->
        let etyp = List.nth_exn tlist_verify 0 in
        List.fold_right vli
          ~f:(fun vl acc ->
            build_cons_lit (json_to_lit etyp this_address vl) etyp acc)
          ~init:(build_nil_lit etyp)
    | `Assoc _ ->
        let constr_str = member_exn "constructor" j |> to_string_exn in
        (* Trust that the constructor belongs to the datatype, but disambiguate *)
        let dis_constr =
          disambiguate_ctr_name
            (InputName.parse_simple_name constr_str)
            this_address
        in
        let argtypes = member_exn "argtypes" j |> JSON.to_list_exn in
        let arguments = member_exn "arguments" j |> JSON.to_list_exn in
        let tlist = json_to_adttyps argtypes in
        let dis_tlist =
          List.map tlist ~f:(fun t -> disambiguate_type t this_address)
        in
        json_to_adtargs dis_constr dis_tlist this_address arguments
    | _ ->
        raise
          (mk_invalid_json
             ( "JSON parsing: error parsing ADT "
             ^ OutputName.as_error_string name ))
  in
  (* return built ADT *)
  res

(* Map is a `List of `Assoc jsons, with
 * the first `Assoc specifying the map's from/to types.*)
and read_map_json kt this_address vt j =
  let open OutputLiteral in
  match j with
  | `List vli ->
      let m = Caml.Hashtbl.create (List.length vli) in
      let _ = mapvalues_from_json m kt vt this_address vli in
      Map ((kt, vt), m)
  | `Null -> Map ((kt, vt), Caml.Hashtbl.create 0)
  | _ -> raise (mk_invalid_json "JSON parsing: error parsing Map")

and mapvalues_from_json m kt vt this_address l =
  List.iter l ~f:(fun first ->
      let kjson = member_exn "key" first in
      let keylit =
        match kt with
        | PrimType _ -> build_prim_lit_exn kt (to_string_exn kjson)
        | _ -> raise (mk_invalid_json "Key in Map JSON is not a PrimType")
      in
      let vjson = member_exn "val" first in
      let vallit = json_to_lit vt this_address vjson in
      Caml.Hashtbl.replace m keylit vallit)

and json_to_lit (t : OutputType.t) (this_address : string) (v : Basic.t) :
    OutputLiteral.t =
  let open OutputType in
  match t with
  | MapType (kt, vt) ->
      let vl = read_map_json kt this_address vt v in
      vl
  | ADT (name, tlist) ->
      let vl =
        read_adt_json (OutputIdentifier.get_id name) this_address v tlist
      in
      vl
  | _ ->
      let tv = build_prim_lit_exn t (to_string_exn v) in
      tv

(* Use parser for local type and constructor names *)
let jobj_to_statevar this_address json =
  let n = member_exn "vname" json |> to_string_exn in
  let tstring = member_exn "type" json |> to_string_exn in
  let t = parse_typ_exn tstring in
  let dis_t = disambiguate_type t this_address in
  let v = member_exn "value" json in
  (n, dis_t, parse_json dis_t this_address v)

(* Inserted from Runner.ml *)
let map_json_input_strings_to_names map =
  List.map map ~f:(fun (x, t, l) ->
      match String.split x ~on:'.' with
      | [ simple_name ] -> (OutputName.parse_simple_name simple_name, t, l)
      | _ -> raise (mk_invalid_json (sprintf "invalid name %s in json input" x)))

let get_address_literal = JSON.get_address_literal

let extract_this_address_from_init_json_data jlist =
  let this_name = OutputName.as_string ContractUtil.this_address_label in
  (* init json contains a _this_address entry, which we need for parsing values *)
  match
    List.find jlist ~f:(fun j ->
        let n = member_exn "vname" j |> to_string_exn in
        String.(n = this_name))
  with
  | Some jv -> (
      let v = member_exn "value" jv |> to_string_exn in
      let lit = OutputLiteral.ByStrX (OutputLiteral.Bystrx.parse_hex v) in
      match get_address_literal lit with
      | None ->
          raise
            (mk_invalid_json
               (sprintf "Unable to extract %s value as string" this_name))
      | Some v -> v )
  | None ->
      raise
        (mk_invalid_json (sprintf "No %s entry found in init file" this_name))

(* Convert a single JSON serialized literal back to its Scilla value. *)
let jstring_to_literal jstring tp this_address =
  let thunk () = Yojson.Basic.from_string jstring in
  let jobj = json_exn_wrapper ~filename:"ipc_fetch" thunk in
  json_to_lit tp this_address jobj

let get_json_data filename =
  let json = JSON.from_file filename in
  (* input json is a list of key/value pairs *)
  json |> JSON.to_list_exn

let parse_json_as_literal jlist this_address =
  List.map jlist ~f:(jobj_to_statevar this_address)

(** Returns a list of (vname:string,value:literal) items
      Invalid inputs in the json are ignored **)
let get_json_data_list filename this_address =
  let jlist = get_json_data filename in
  parse_json_as_literal jlist this_address

let parse_json filename this_address =
  let init_data = get_json_data_list filename this_address in
  map_json_input_strings_to_names init_data

let get_this_address_from_init_file filename =
  let jlist = get_json_data filename in
  (* Extract this_address entry, since this is needed for disambiguation inside jobj_to_statevar *)
  extract_this_address_from_init_json_data jlist

(* Eval.ml *)

let init_lib_entries libs this_address =
  let open InputSyntax in
  let open InputIdentifier in
  List.iter libs ~f:(fun lentry ->
      match lentry with
      | LibTyp (tname, ctr_defs) ->
          let open Datatypes.DataTypeDictionary in
          let ctrs, tmaps =
            List.fold_right ctr_defs ~init:([], [])
              ~f:(fun ctr_def (tmp_ctrs, tmp_tmaps) ->
                let { cname; c_arg_types } = ctr_def in
                (* cname is a user-defined constructor, so qualify with this_address *)
                let dis_cname = disambiguate_name (get_id cname) this_address in
                let dis_c_arg_types =
                  List.fold_right c_arg_types ~init:[] ~f:(fun c_arg_typ acc ->
                      disambiguate_type c_arg_typ this_address :: acc)
                in
                ( {
                    Datatypes.cname = dis_cname;
                    Datatypes.arity = List.length c_arg_types;
                  }
                  :: tmp_ctrs,
                  (dis_cname, dis_c_arg_types) :: tmp_tmaps ))
          in
          let dis_tname = disambiguate_name (get_id tname) this_address in
          let adt =
            {
              Datatypes.tname = dis_tname;
              Datatypes.tparams = [];
              Datatypes.tconstr = ctrs;
              Datatypes.tmap = tmaps;
            }
          in
          let _ = add_adt adt (get_rep tname) in
          ()
      | LibVar _ -> ())

(* StateService.ml *)

module InputStateService = struct
  (* StateIPCClient.ml *)

  module InputStateIPCClient = struct
    open Scilla_eval.StateIPCIdl
    module M = Idl.IdM
    module IDL = Idl.Make (M)

    module IPCClient = IPCIdl (IDL.GenClient ())

    (* Translate JRPC result to our result. *)
    let translate_res res =
      match res |> IDL.T.get |> M.run with
      | Error (e : Scilla_eval.IPCUtil.RPCError.err_t) ->
          fatal_error
            (mk_error0
               (Printf.sprintf
                  "StateIPCClient: Error in IPC access: (code:%d, message:%s)."
                  e.code e.message))
      | Ok res' -> res'

    let ipcclient_exn_wrapper thunk =
      try thunk () with
      | Unix.Unix_error (_, s1, s2) ->
          fatal_error (mk_error0 ("StateIPCClient: Unix error: " ^ s1 ^ s2))
      | _ ->
          fatal_error
            (mk_error0 "StateIPCClient: Unexpected error making JSON-RPC call")

    let binary_rpc ~socket_addr (call : Rpc.call) : Rpc.response M.t =
      let socket =
        Unix.socket ~domain:Unix.PF_UNIX ~kind:Unix.SOCK_STREAM ~protocol:0 ()
      in
      Unix.connect socket ~addr:(Unix.ADDR_UNIX socket_addr);
      let ic, oc =
        (Unix.in_channel_of_descr socket, Unix.out_channel_of_descr socket)
      in
      let msg_buf = Jsonrpc.string_of_call ~version:Jsonrpc.V2 call in
      DebugMessage.plog (Printf.sprintf "Sending: %s\n" msg_buf);
      (* Send data to the socket. *)
      let _ = Scilla_eval.IPCUtil.send_delimited oc msg_buf in
      (* Get response. *)
      let response = Caml.input_line ic in
      Unix.close socket;
      DebugMessage.plog (Printf.sprintf "Response: %s\n" response);
      M.return @@ Jsonrpc.response_of_string response

    let deserialize_literal s tp this_address =
      try jstring_to_literal s tp this_address
      with Invalid_json s ->
        fatal_error
          ( s
          @ mk_error0
              "InputStateIPCClient: Error deserializing literal fetched from \
               IPC call" )

    (* Deserialize proto_scilla_val, given its type. *)
    let rec deserialize_value value tp this_address =
      match value with
      | Scilla_eval.Ipcmessage_types.Bval s ->
          deserialize_literal (Bytes.to_string s) tp this_address
      | Scilla_eval.Ipcmessage_types.Mval m -> (
          match tp with
          | MapType (kt, vt) ->
              let mlit = Caml.Hashtbl.create (List.length m.m) in
              let () =
                let m =
                  List.sort m.m ~compare:(fun (k1, _) (k2, _) ->
                      String.compare k1 k2)
                in
                List.iter m ~f:(fun (k, v) ->
                    let k' = deserialize_literal k kt this_address in
                    let v' = deserialize_value v vt this_address in
                    Caml.Hashtbl.add mlit k' v')
              in
              GlobalLiteral.Map ((kt, vt), mlit)
          | _ ->
              fatal_error
                (mk_error0
                   "StateIPCClient: Type mismatch deserializing value. \
                    Unexpected protobuf map.") )

    let encode_serialized_query query =
      try
        let encoder = Pbrt.Encoder.create () in
        Scilla_eval.Ipcmessage_pb.encode_proto_scilla_query query encoder;
        Bytes.to_string @@ Pbrt.Encoder.to_bytes encoder
      with e -> fatal_error (mk_error0 (Exn.to_string e))

    let decode_serialized_value value =
      try
        let decoder = Pbrt.Decoder.of_bytes value in
        Scilla_eval.Ipcmessage_pb.decode_proto_scilla_val decoder
      with e -> fatal_error (mk_error0 (Exn.to_string e))

    let fetch ~socket_addr ~fname ~tp ~this_address =
      let open Scilla_eval.Ipcmessage_types in
      let q =
        {
          name = InputIdentifier.as_string fname;
          mapdepth = TypeUtil.TypeUtilities.map_depth tp;
          indices = [];
          (* indices are not needed, as we are only fetching entire states *)
          ignoreval = false;
        }
      in
      let q' = encode_serialized_query q in
      let res =
        let thunk () =
          translate_res
          @@ IPCClient.fetch_state_value (binary_rpc ~socket_addr) q'
        in
        ipcclient_exn_wrapper thunk
      in
      match res with
      | true, res' ->
          let decoded_pb = decode_serialized_value (Bytes.of_string res') in
          let res'' = deserialize_value decoded_pb tp this_address in
          Some res''
      | false, _ -> None
  end

  type ss_field = {
    fname : InputName.t;
    (* Easier to disambiguate the type before fetching *)
    ftyp : OutputType.t;
    fval : InputLiteral.t option;
        (* We may or may not have the value in memory. *)
  }

  let fetch ~fname ~tp ~socket_addr ~this_address =
    let res = InputStateIPCClient.fetch ~socket_addr ~fname ~tp ~this_address in
    match res with
    | None ->
        fatal_error
          (mk_error0
             (sprintf "StateService: Field %s not found on IPC server."
                (InputIdentifier.as_error_string fname)))
    | Some _ -> res

  let get_full_state fl ~socket_addr ~this_address =
    List.map fl ~f:(fun f ->
        let v =
          fetch
            ~fname:(InputIdentifier.mk_loc_id f.fname)
            ~tp:f.ftyp ~socket_addr ~this_address
        in
        (f.fname, f.ftyp, v))
end

(* Runner.ml *)

let run_with_args args =
  match InputFEParser.parse_cmodule args.input with
  | Error e ->
      (* Error is printed by the parser. *)
      plog (sprintf "%s\n" "Failed to parse input file.");
      fatal_error e
  | Ok cmod ->
      plog
        (sprintf "\n[Parsing]:\nContract module [%s] is successfully parsed.\n"
           args.input);
      let init, state =
        try
          (* Read _this_address from init file. This is needed for all disambiguation *)
          let this_address = get_this_address_from_init_file args.input_init in
          (* Contract library. *)
          let clib_entries =
            Option.value_map cmod.libs ~default:[] ~f:(fun { lentries; _ } ->
                lentries)
          in
          (* Initialise datatype dictionary with user-defined types *)
          let () = init_lib_entries clib_entries this_address in

          (* parse_json reads, parses and disambiguates the json file *)
          let init =
            let untyped_state = parse_json args.input_init this_address in
            List.map untyped_state ~f:(fun x ->
                match TypeUtil.TypeUtilities.literal_type (trd3 x) with
                | Ok t -> (fst3 x, t, trd3 x)
                | Error _ ->
                    fatal_error
                      (mk_error0
                         (sprintf "Unable to determine type of literal %s"
                            (pp_literal (trd3 x)))))
          in
          let state =
            if String.is_empty args.ipc_address then
              (* Use the provided state json. *)
              let untyped_state = parse_json args.input_state this_address in
              List.map untyped_state ~f:(fun x ->
                  match TypeUtil.TypeUtilities.literal_type (trd3 x) with
                  | Ok t -> (fst3 x, t, trd3 x)
                  | Error _ ->
                      fatal_error
                        (mk_error0
                           (sprintf "Unable to determine type of literal %s"
                              (pp_literal (trd3 x)))))
            else
              (* Use IPC *)
              (* Fetch state from IPC server *)
              let inputfields =
                List.map cmod.contr.cfields ~f:(fun (fname, ftyp, _) ->
                    let open InputStateService in
                    (* Disambiguate type before fetching - it's easier to parse the json that way *)
                    {
                      fname = InputIdentifier.get_id fname;
                      ftyp = disambiguate_type ftyp this_address;
                      fval = None;
                    })
              in
              (* Fetch state. Parsing the fetched jsons disambiguates *)
              let state =
                InputStateService.get_full_state inputfields
                  ~socket_addr:args.ipc_address ~this_address
              in

              (* Update using StateService.ml *)
              let sm = Scilla_eval.StateService.IPC args.ipc_address in
              let outputfields =
                List.map state ~f:(fun (n, tp, v) ->
                    let open Scilla_eval.StateService in
                    {
                      fname = convert_simple_name_to_simple_name n;
                      ftyp = tp;
                      fval = v;
                    })
              in
              (* Initialise with the final values, then update, then finalise. *)
              (* ~ext_states not initialised, since they are not supported anyway *)
              let () =
                OutputStateService.initialize ~sm ~fields:outputfields
                  ~ext_states:[]
              in
              let () =
                List.iter outputfields ~f:(fun ssf ->
                    let open Scilla_eval.StateService in
                    let { fname; fval; _ } = ssf in
                    match fval with
                    | None ->
                        fatal_error
                          (mk_error0
                             (sprintf "Missing value for field %s\n"
                                (SSName.as_string fname)))
                    | Some v -> (
                        match
                          OutputStateService.update
                            ~fname:(SSIdentifier.mk_loc_id fname)
                            ~keys:[] ~value:v
                        with
                        | Ok () -> ()
                        | Error e -> fatal_error e ))
              in
              let _ = OutputStateService.finalize () in

              (* TODO: Make sure the ipc-generated state have the correct form for state output *)
              match OutputStateService.get_full_state () with
              | Ok state ->
                  (* _balance is not availabe from IPC server, so use the one from the state file *)
                  let state_from_file =
                    parse_json args.input_state this_address
                  in
                  let balance =
                    List.find_exn state_from_file ~f:(fun (fname, _, _) ->
                        OutputName.equal fname ContractUtil.balance_label)
                  in
                  balance :: state
              | Error e -> fatal_error e
          in
          (init, state)
        with Invalid_json s ->
          fatal_error (s @ mk_error0 "Failed to parse json\n")
      in
      (* state_to_json maps name * literal to a vname * type * value json, which is
         the format for both init and state jsons *)
      ( JSON.ContractState.state_to_json init,
        JSON.ContractState.state_to_json state )

let run ~exe_name =
  ErrorUtils.reset_warnings ();
  Datatypes.DataTypeDictionary.reinit ();
  let args = parse ~exe_name in
  let result_init, result_state = run_with_args args in
  (result_init, result_state, args)

(* scilla_runner.ml *)

let output_to_string = Yojson.Basic.pretty_to_string

let () =
  try
    let output_init, output_state, args = run ~exe_name:(Sys.get_argv ()).(0) in
    let init_str = output_to_string output_init in
    let state_str = output_to_string output_state in
    Out_channel.with_file args.output_init ~f:(fun ch ->
        Out_channel.output_string ch init_str);
    Out_channel.with_file args.output_state ~f:(fun ch ->
        Out_channel.output_string ch state_str)
  with FatalError msg -> exit_with_error msg
