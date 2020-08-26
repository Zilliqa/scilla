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
open Scilla_eval
open ErrorUtils
open DebugMessage
open PrettyPrinters
open GlobalConfig
open Syntax
open Literal
open Yojson
open ParserUtil

module InputLiteral = LocalLiteral
module InputType = InputLiteral.LType
module InputIdentifier = InputType.TIdentifier
module InputName = InputIdentifier.Name
module InputFEParser = FrontEndParser.ScillaFrontEndParser (InputLiteral)

module OutputLiteral = GlobalLiteral
module OutputType = OutputLiteral.LType
module OutputIdentifier = OutputType.TIdentifier
module OutputName = OutputIdentifier.Name

(* ************************************************************************ * 
 * This executable parses a contract and reads its associated state using   *
 * local names, and outputs its state using global names. The state itself  *
 * does not change, only the type and constructor names that appear in the  *
 * state.                                                                   *
 *                                                                          *
 * This transformation is necessary in order to enable external libraries   *
 * and remote state reads.                                                  *
 * ************************************************************************ *)

(* Copied/reimplemented from Disambiguate.ml *)

(* Assumptions:
   - DataTypeDictionary only contains predefined names
   - The module has already passed scilla-checker

   It follows from this that types and constructors in state and init jsons 
   fall into two distinct categories:
   - Names that exist in DataTypeDictionary: These disambiguate to simple names
   - Names that do not exist in DataTypeDictionary: These disambiguate to 
     _this_address.name *)

let disambiguate_type this_address t =
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
        match TIdentifier.get_id t_name with
        | QualifiedLocal _ ->
            raise (
              mk_invalid_json (
                sprintf "Found qualified type name %s in file"
                  (TIdentifier.as_error_string t_name)))
        | SimpleLocal nm ->
            let dis_nm = 
              (* Try nm as a simple global name *)
              let tmp_nm = OutputName.parse_simple_name nm in
              match Datatypes.DataTypeDictionary.lookup_name tmp_nm with
              | Ok _ -> (* Name exists => Name is predefined *)
                  tmp_nm
              | Error _ -> (* Name does not exist => Name is user-defined *)
                  OutputName.parse_qualified_name this_address nm
            in
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

(* Copied from RunnerCLI.ml *)

type args = {
  input_init : string;
  input_state : string;
  output_init : string;
  output_state : string;
}

let f_input_init = ref ""

let f_input_state = ref ""

let f_output_init = ref ""

let f_output_state = ref ""

let reset () =
  f_input_init := "";
  f_input_state := "";
  f_output_init := "";
  f_output_state := ""

let validate_main usage =
  let open Core_kernel in
  let msg = "" in
  let msg =
    (* input_init.json is mandatory *)
    if not @@ Sys.file_exists !f_input_init then "Invalid input init file\n"
    else msg
  in
  let msg =
    (* input_state.json is mandatory *)
    if not @@ Sys.file_exists !f_input_state then "Invalid input state file\n"
    else msg
  in
  let msg =
    (* output_init.json is mandatory *)
    if not @@ Sys.file_exists !f_output_init then "Invalid output init file\n"
    else msg
  in
  let msg =
    (* output_state.json is mandatory *)
    if not @@ Sys.file_exists !f_output_state then "Invalid output state file\n"
    else msg
  in
    PrettyPrinters.fatal_error_noformat (usage ^ Printf.sprintf "%s\n" msg)

let parse ~exe_name =
  reset ();
  let speclist =
    [
      ( "-iinit",
        Arg.String (fun x -> f_input_init := x),
        "Path to input init json" );
      ( "-istate",
        Arg.String (fun x -> f_input_state := x),
        "Path to input state json" );
      ("-oinit", Arg.String (fun x -> f_output_init := x), "Path to output init json");
      ("-ostate", Arg.String (fun x -> f_output_init := x), "Path to output state json");
    ]
  in
  let mandatory_usage =
    "Usage:\n" ^ exe_name ^ " -iinit input_init.json -istate input_state.json"
    ^ "-oinit output_init.json -ostate output_state.scilla" ^ "\n"
  in
  let usage = mandatory_usage ^ "\n  " in
  let ignore_anon _ = () in
  let () = Arg.parse speclist ignore_anon mandatory_usage in
  let () = validate_main usage in
  {
    input_init = !f_input_init;
    input_state = !f_input_state;
    output_init = !f_output_init;
    output_state = !f_output_state;
  }

(* Copied from JSONParser.ml *)

let constr_pattern_arg_types_exn dt cname =
  (* TODO: Figure out whether it's possible to reuse TypeUtil code *)
  match constr_pattern_arg_types dt cname with
  | Error emsg -> raise (Invalid_json emsg)
  | Ok s -> s

let lookup_adt_name_exn name =
  (* TODO: Name is an InputName - find out how to translate to something that works for datatype dictionary *)
  match Datatypes.DataTypeDictionary.lookup_name (InputIdentifier.get_id name) with
  | Error emsg -> raise (Invalid_json emsg)
  | Ok s -> s

type adt_parser_entry =
  | Incomplete (* Parser not completely constructed. *)
  | Parser of (Basic.t -> InputLiteral.t)

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

(* Copied from JSON.ml - inserted here for convenience *)

let member_exn = JSONParser.member_exn

let to_string_exn = JSONParser.to_string_exn

(* Copied from JSONParser.ml - continued *)

(*************************************)
(******** Parser Generator ***********)
(*************************************)

(* Generate a parser. *)
let gen_parser (t' : InputType.t) : Yojson.Basic.t -> InputLiteral.t =
  let open Basic in
  let open InputType in
  let open InputLiteral in
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
                    if not (Datatypes.is_list_adt_name (InputIdentifier.get_id name)) then
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
              AssocDictionary.insert (InputName.as_string cn.cname) parser maps)
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


(* Copied from JSON.ml *)

let json_exn_wrapper = JSONParser.json_exn_wrapper

let to_string_exn = JSONParser.to_string_exn

let from_file f =
  let thunk () = Basic.from_file f in
  json_exn_wrapper thunk ~filename:f

let parse_as_name n =
  match String.split_on_chars ~on:['.'] n with
  | [ t1 ; t2 ] -> InputName.parse_qualified_name t1 t2
  | [ t1 ] -> InputName.parse_simple_name t1
  | _ -> raise (mk_invalid_json (sprintf "Invalid name in json: %s\n" n))

let parse_typ_exn t =
  match InputFEParser.parse_type t with
  | Error _ -> raise (mk_invalid_json (sprintf "Invalid type in json: %s\n" t))
  | Ok s -> s

let to_list_exn j =
  let thunk () = Basic.Util.to_list j in
  json_exn_wrapper thunk

let build_prim_lit_exn t v =
  let open InputType in
  let open InputLiteral in
  let exn () =
    mk_invalid_json ("Invalid " ^ pp_typ t ^ " value " ^ v ^ " in JSON")
  in
  match t with
  | PrimType pt -> (
      match build_prim_literal pt v with
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

let rec json_to_adtargs cname tlist ajs =
  let open InputType in
  let open InputLiteral in
  let verify_args_exn cname provided expected =
    if provided <> expected then
      let p = Int.to_string provided in
      let e = Int.to_string expected in
      raise
        (mk_invalid_json
           ( "Malformed ADT constructor " ^ (OutputName.as_error_string cname) ^ ": expected " ^ e
             ^ " args, but provided " ^ p ^ "." ))
  in
  let dt =
    (* TODO: this needs to be translated to an input constructor *)
    match Datatypes.DataTypeDictionary.lookup_constructor cname with
    | Error emsg -> raise (Invalid_json emsg)
    | Ok (r, _) -> r
  in
  (* For each component literal of our ADT, calculate its type.
   * This is essentially using DataTypes.constr_tmap and substituting safely. *)
  let tmap =
    constr_pattern_arg_types_exn (ADT (InputIdentifier.mk_loc_id dt.tname, tlist)) cname
  in
  verify_args_exn cname (List.length ajs) (List.length tmap);
  let llist = List.map2_exn tmap ajs ~f:(fun t j -> json_to_lit t j) in
  ADTValue (cname, tlist, llist)

and read_adt_json name j tlist_verify =
  (* TODO: We rely heavily on DataTypeDictionary, but we need to keep it as input types *)
  let open InputLiteral in
  let open Datatypes in
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
               (mk_error0 "ADT value is a JSON array, but type is not List"))
        else
          let etyp = List.nth_exn tlist_verify 0 in
          List.fold_right vli
            ~f:(fun vl acc -> build_cons_lit (json_to_lit etyp vl) etyp acc)
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
            (mk_invalid_json
               ("ADT type " ^ (OutputName.as_error_string dt.tname)
                ^ " does not match constructor " ^ (OutputName.as_error_string constr)));
        let argtypes = member_exn "argtypes" j |> to_list_exn in
        let arguments = member_exn "arguments" j |> to_list_exn in
        let tlist = json_to_adttyps argtypes in
        json_to_adtargs constr tlist arguments
    | _ -> raise (mk_invalid_json ("JSON parsing: error parsing ADT "
                                   ^ (OutputName.as_error_string name)))
  in
  (* return built ADT *)
  res

(* Map is a `List of `Assoc jsons, with
 * the first `Assoc specifying the map's from/to types.*)
and read_map_json kt vt j =
  let open InputLiteral in
  match j with
  | `List vli ->
      let m = Caml.Hashtbl.create (List.length vli) in
      let _ = mapvalues_from_json m kt vt vli in
      Map ((kt, vt), m)
  | `Null -> Map ((kt, vt), Caml.Hashtbl.create 0)
  | _ -> raise (mk_invalid_json "JSON parsing: error parsing Map")

and mapvalues_from_json m kt vt l =
  List.iter l ~f:(fun first ->
      let kjson = member_exn "key" first in
      let keylit =
        match kt with
        | PrimType _ -> build_prim_lit_exn kt (to_string_exn kjson)
        | _ -> raise (mk_invalid_json "Key in Map JSON is not a PrimType")
      in
      let vjson = member_exn "val" first in
      let vallit = json_to_lit vt vjson in
      Caml.Hashtbl.replace m keylit vallit)

and json_to_lit t v =
  let open InputType in
  let open InputIdentifier in
  match t with
  | MapType (kt, vt) ->
      let vl = read_map_json kt vt v in
      vl
  | ADT (name, tlist) ->
      let vl = read_adt_json (get_id name) v tlist in
      vl
  | _ ->
      let tv = build_prim_lit_exn t (to_string_exn v) in
      tv

let jobj_to_statevar json =
  let n = member_exn "vname" json |> to_string_exn in
  let tstring = member_exn "type" json |> to_string_exn in
  let t = parse_typ_exn tstring in
  let v = member_exn "value" json in
  (n, parse_json t v)

module ContractState = struct
  (** Returns a list of (vname:string,value:literal) items
      Invalid inputs in the json are ignored **)
  let get_json_data filename =
    let json = from_file filename in
    (* input json is a list of key/value pairs *)
    let jlist = json |> to_list_exn in
    List.map jlist ~f:jobj_to_statevar
end

(* Copied from Eval.ml *)

(* Initialize a module with given arguments and initial balance *)
let init_module initargs curargs init_bal =
  let%bind initcstate, field_vals =
    init_contract libs elibs cconstraint cparams cfields initargs init_bal
  in
  let%bind curfield_vals = create_cur_state_fields field_vals curargs in
  (* blockchain input provided is only validated and not used here. *)
  let%bind () = EvalMonad.ignore_m @@ check_blockchain_entries bstate in
  let cstate = { initcstate with fields = initcstate.fields } in
  pure (contr, cstate, curfield_vals)

(* Copied from Runner.ml *)

(* validate_get_init_json combined with get_init_this_address_and_extlibs from JSON.ml *)
let validate_get_init_json init_file gas_remaining source_ver =
  (* TODO: Combine these two reads of init_file into one, 
     and make sure everything is returned *)
  let this_address, name_addr_pairs =
    if not (Caml.Sys.file_exists init_file) then (
      plog (sprintf "Invalid init json %s file" init_file);
      (None, []) )
    else
      try
        let this_address, name_addr_pairs =
          JSON.ContractState.get_init_this_address_and_extlibs init_file in
        (this_address, name_addr_pairs)
      with Invalid_json s ->
        fatal_error
          (s @ mk_error0 (sprintf "Unable to parse JSON file %s. " init_file))
  in
(* Retrieve initial parameters *)
  let initargs_str =
    try JSON.ContractState.get_json_data init_file
    with Invalid_json s ->
      fatal_error_gas
        (s @ mk_error0 (sprintf "Failed to parse json %s:\n" init_file))
        gas_remaining
  in
  let initargs = map_json_input_strings_to_names initargs_str in
  (* Check for version mismatch. Subtract penalty for mismatch. *)
  let init_json_scilla_version =
    List.Assoc.find initargs ~equal:[%equal : RunnerName.t]
      ContractUtil.scilla_version_label
  in
  initargs

let run_with_args args =
  let gas_remaining = Stdint.Uint64.max_int in
  let cmod = 1 in
  (* TODO: get_init_this_address_and_extlibs and validate_get_init_json are to be combined *)
  let this_address_opt, _init_address_map = get_init_this_address_and_extlibs args.input_init in
  match this_address_opt with
  | None -> 
      let msg = sprintf "No %s entry found in init file %s\n" (OutputName.as_string ContractUtil.this_address_label) args.input_init in
      plog msg;
      fatal_error_gas 
        (mk_error0
           (sprintf "Ran out of gas when parsing contract/init files.\n"))
        gas_remaining
  | Some this_address ->
      (* Checking initialized libraries! *)
      let initargs =
        validate_get_init_json args.input_init gas_remaining cmod.smver
      in
      
      let output_state_json =
        (* Not initialization, execute transition specified in the message *)
        let cstate, gas_remaining' =
          (* Retrieve state variables *)
          let curargs, cur_bal =
            try input_state_json args.input_state
            with Invalid_json s ->
              fatal_error_gas
                ( s
                  @ mk_error0
                    (sprintf "Failed to parse json %s:\n"
                       args.input_state) )
                gas_remaining
          in
          
          (* Initializing the contract's state *)
          let init_res = init_module initargs curargs cur_bal 
          in
          (* Prints stats after the initialization and returns the initial state *)
          (* Will throw an exception if unsuccessful. *)
          let cstate, gas_remaining', field_vals =
            check_extract_cstate args.input init_res gas_remaining
          in
          
          (* Initialize the state server. *)
          let fields =
            List.map field_vals ~f:(fun (s, l) ->
                let open StateService in
                let t =
                  List.Assoc.find_exn cstate.fields s ~equal:[%equal : RunnerName.t]
                in
                { fname = s; ftyp = t; fval = Some l })
          in
          let () = StateService.initialize ~sm:Local ~fields in
          (cstate, gas_remaining')
        in
        
        (* If we're using a local state (JSON file) then need to fetch and dump it. *)
        let field_vals =
          match
            (StateService.get_full_state (), StateService.finalize ())
          with
          | Ok fv, Ok () -> fv
          | _ ->
              fatal_error_gas
                (mk_error0 "Error finalizing state from StateService")
                gas
        in
        
        let osj = output_state_json cstate'.balance field_vals in
        osj
      in
      `Assoc
        [
          ("scilla_major_version", `String (Int.to_string cmod.smver));
          ("gas_remaining", `String (Uint64.to_string gas));
          (RunnerName.as_string ContractUtil.accepted_label, `String (Bool.to_string accepted_b));
          ("messages", output_msg_json);
          ("states", output_state_json);
          ("events", output_events_json);
        ]
            
let run ~exe_name =
  GlobalConfig.reset ();
  ErrorUtils.reset_warnings ();
  Datatypes.DataTypeDictionary.reinit ();
  let args = parse ~exe_name in
  let result = run_with_args args in
  (result, args)

(* Copied from scilla_runner *)

let output_to_string output =
  Yojson.Basic.to_string output

let () =
  try
    let output, args = run ~exe_name:(Sys.get_argv ()).(0) in
    let str = output_to_string output in
    if String.is_empty args.output_init ||
       String.is_empty args.output_state
    then DebugMessage.pout str
    else
      Out_channel.with_file args.output_init ~f:(fun ch ->
          Out_channel.output_string ch str);
      Out_channel.with_file args.output_state ~f:(fun ch ->
          Out_channel.output_string ch str);
  with FatalError msg -> exit_with_error msg
