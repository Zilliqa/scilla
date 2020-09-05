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
}

let f_input_init = ref ""

let f_input_state = ref ""

let f_output_init = ref ""

let f_output_state = ref ""

let f_input = ref ""

let reset () =
  f_input_init := "";
  f_input_state := "";
  f_output_init := "";
  f_output_state := "";
  f_input := ""

let validate_main usage =
  let open Core_kernel in
  let msg = "" in
  let msg =
    (* input_init.json is mandatory *)
    if not @@ Sys.file_exists !f_input_init then "Invalid input initialization file\n"
    else msg
  in
  let msg =
    (* input_state.json is mandatory *)
    if not @@ Sys.file_exists !f_input_state then
      msg ^ "Invalid input state file\n"
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
    if String.is_empty !f_input then
      msg ^ "Output initialization file must be specified\n"
    else msg
  in
  let msg =
    (* output_init file is mandatory *)
    if String.is_empty !f_input then
      msg ^ "Output initialization file must be specified\n"
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
      ("-oinit", Arg.String (fun x -> f_output_init := x), "Path to output init json");
      ("-ostate", Arg.String (fun x -> f_output_state := x), "Path to output state json");
    ]
  in

  let mandatory_usage =
    "Usage:\n" ^ exe_name ^ " -iinit input_init.json -istate input_state.json"
    ^ " -oinit output_init.json -ostate output_state.json] -i input.scilla"
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
  }


(* Copied/reimplemented from Disambiguate.ml *)

(* Qualify a local simple name with this_address *)
let disambiguate_name name this_address =
  let open Identifier.LocalName in
  match name with
  | SimpleLocal nm -> Identifier.GlobalName.parse_qualified_name this_address nm
  | QualifiedLocal _ -> 
      let msg = sprintf "Unexpected qualified local name %s\n" (as_error_string name) in
      plog msg;
      fatal_error (mk_error0 msg)

let disambiguate_dt_dictionary_name name this_address lookup =
  let open Identifier.LocalName in
  match name with
  | QualifiedLocal _ ->
      raise (
        mk_invalid_json (
          sprintf "Found qualified type name %s in file" 
            (InputName.as_error_string name)))
  | SimpleLocal nm ->
      (* Try nm as a simple global name *)
      let tmp_nm = OutputName.parse_simple_name nm in
      match lookup tmp_nm with
      | Ok _ -> (* Name exists => Name is predefined *)
          tmp_nm
      | Error _ -> (* Name does not exist => Name is user-defined *)
          OutputName.parse_qualified_name this_address nm
      
let disambiguate_adt_name name this_address =
  disambiguate_dt_dictionary_name name this_address Datatypes.DataTypeDictionary.lookup_name

let disambiguate_ctr_name name this_address =
  disambiguate_dt_dictionary_name name this_address Datatypes.DataTypeDictionary.lookup_constructor

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
    | ADT (t_name, targs) -> (
        let dis_nm = disambiguate_adt_name (InputIdentifier.get_id t_name) this_address in
        let dis_t_name = OutputIdentifier.mk_id dis_nm (InputIdentifier.get_rep t_name) in
        let dis_targs = List.map targs ~f:recurse in
        OutputType.ADT (dis_t_name, dis_targs))
    | TypeVar tvar -> OutputType.TypeVar tvar
    | PolyFun (tvar, t) ->
      let dis_t = recurse t in
      OutputType.PolyFun (tvar, dis_t)
    | Unit -> OutputType.Unit
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
let gen_parser (t' : OutputType.t) (this_address : string) : Basic.t -> OutputLiteral.t =
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
                    if not (Datatypes.is_list_adt_name (OutputIdentifier.get_id name)) then
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
          let dis_cn = disambiguate_ctr_name (InputName.parse_simple_name cn) this_address in
          match AssocDictionary.lookup (OutputName.as_string dis_cn) cn_parsers with
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

(* Use parser for local type and constructor names *)
(* TODO: this_address is not available at this point, so disambiguating the type needs to be done differently *)
let jobj_to_statevar this_address json =
  let n = member_exn "vname" json |> to_string_exn in
  let tstring = member_exn "type" json |> to_string_exn in
  let t = parse_typ_exn tstring in
  let dis_t = disambiguate_type t this_address in
  let v = member_exn "value" json in
  (n, parse_json dis_t this_address v) 

(* Inserted from Runner.ml *)
let map_json_input_strings_to_names map =
  List.map map ~f:(fun (x, l) ->
      match String.split x ~on:'.' with
      | [simple_name] -> (OutputName.parse_simple_name simple_name, l)
      | _ -> raise (mk_invalid_json (sprintf "invalid name %s in json input" x)))

let get_address_literal = JSON.get_address_literal

let extract_this_address_from_init_json_data jlist =
  let this_name = OutputName.as_string ContractUtil.this_address_label in
  (* init json contains a _this_address entry, which we need for parsing values *)
  match List.find jlist ~f:(fun j ->
      let n = member_exn "vname" j |> to_string_exn in
      String.(n = this_name)) with
  | Some jv ->
      let v = member_exn "value" jv |> to_string_exn in
      let lit = OutputLiteral.ByStrX (OutputLiteral.Bystrx.parse_hex v) in
      Option.value (get_address_literal lit)
        ~default:(raise (mk_invalid_json (sprintf "Unable to extract %s value as string" this_name)))
  | None ->
      raise (mk_invalid_json (sprintf "No %s entry found in init file" this_name))

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
                  let dis_c_arg_types = List.fold_right c_arg_types ~init:[]
                      ~f:(fun c_arg_typ acc ->
                          disambiguate_type c_arg_typ this_address :: acc) in
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

(* Runner.ml *)

let run_with_args args =
  match InputFEParser.parse_cmodule args.input with
  | Error e ->
      (* Error is printed by the parser. *)
      plog (sprintf "%s\n" "Failed to parse input file.");
      fatal_error e
  | Ok cmod ->
      plog
        (sprintf
           "\n[Parsing]:\nContract module [%s] is successfully parsed.\n"
           args.input);
      let init, state = 
        try 
          let this_address = get_this_address_from_init_file args.input_init in
          (* Contract library. *)
          let clib_entries = Option.value_map cmod.libs ~default:[] ~f:(fun { lentries ; _ } -> lentries) in
          (* Initialise datatype dictionary with user-defined types *)
          let () = init_lib_entries clib_entries this_address in
          
          (* parse_json reads, parses and disambiguates the json file *)
          let init = parse_json args.input_init this_address in
          let state = parse_json args.input_state this_address in
          (init, state)
        with Invalid_json s ->
          fatal_error
            ( s @ mk_error0 "Failed to parse json\n" )
      in
      (* state_to_json maps name * literal to a vname * type * value json, which is
         the format for both init and state jsons *)
      (JSON.ContractState.state_to_json init,
       JSON.ContractState.state_to_json state)

let run ~exe_name =
  ErrorUtils.reset_warnings ();
  Datatypes.DataTypeDictionary.reinit ();
  let args = parse ~exe_name in
  let result_init, result_state = run_with_args args in
  (result_init, result_state, args)

(* scilla_runner.ml *)
  
let output_to_string = Yojson.Basic.to_string

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
