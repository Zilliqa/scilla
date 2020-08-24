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

(* Copied from JSON.ml *)

(* JSONParser is a wrapper for Yojson, so don't worry about the meaning of names there *)
let json_exn_wrapper = JSONParser.json_exn_wrapper

let member_exn = JSONParser.member_exn

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
  let open InputLiteral in
  let open InputType in
  let exn () =
    mk_invalid_json ("Invalid " ^ pp_typ t ^ " value " ^ v ^ " in JSON")
  in
  match t with
  | PrimType pt -> (
      match build_prim_literal pt v with
      | Some v' -> v'
      | None -> raise (exn ()) )
  | _ -> raise (exn ())

and read_adt_json name j tlist_verify =
  let open Datatypes in
  let open InputLiteral in
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
               ("ADT type " ^ (JSONName.as_error_string dt.tname)
                ^ " does not match constructor " ^ (JSONName.as_error_string constr)));
        let argtypes = member_exn "argtypes" j |> to_list_exn in
        let arguments = member_exn "arguments" j |> to_list_exn in
        let tlist = json_to_adttyps argtypes in
        json_to_adtargs constr tlist arguments
    | _ -> raise (mk_invalid_json ("JSON parsing: error parsing ADT "
                                   ^ (JSONName.as_error_string name)))
  in
  (* match tlist1 with adt's tlist. *)
  let verify_exn name tlist1 adt =
    match adt with
    | ADTValue (_, tlist2, _) ->
        if type_equiv_list tlist1 tlist2 then ()
        else
          let expected = pp_typ_list_error tlist1 in
          let observed = pp_typ_list_error tlist2 in
          raise
            (mk_invalid_json
               ( "Type mismatch in parsing ADT " ^ JSONName.as_error_string name
                 ^ ". Expected: " ^ expected ^ " vs Observed: " ^ observed ))
    | _ -> raise (mk_invalid_json ("Type mismatch in parsing ADT "
                                   ^ JSONName.as_error_string name))
  in
  (* verify built ADT *)
  verify_exn name tlist_verify res;
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
  if GlobalConfig.validate_json () then (n, json_to_lit t v)
  else (n, JSONParser.parse_json t v)

module ContractState = struct
  (** Returns a list of (vname:string,value:literal) items
      Invalid inputs in the json are ignored **)
  let get_json_data filename =
    let json = from_file filename in
    (* input json is a list of key/value pairs *)
    let jlist = json |> to_list_exn in
    List.map jlist ~f:jobj_to_statevar

  (* Accessor for _this_address and _extlibs entries in init.json. 
     Combined into one function to avoid reading init.json from disk multiple times. *)
  let get_init_this_address_and_extlibs filename =
    let init_data = get_json_data filename in
    let extlibs = get_init_extlibs init_data in
    let this_address_init_opt =
      match List.filter init_data ~f:(fun (name, _) ->
          String.(name = JSONName.as_string ContractUtil.this_address_label)) with
      | [ ( _, adr) ] -> Some adr
      | [] -> None (* We allow init files without a _this_address entry in scilla-checker *)
      | _ -> raise (mk_invalid_json
                       ( "Multiple " ^ JSONName.as_string ContractUtil.this_address_label
                         ^ " entries specified in init json")) in
    match this_address_init_opt with
    | None -> (None, extlibs)
    | Some adr ->
        match get_address_literal adr with
        | Some adr -> (Some adr, extlibs)
        | None -> raise (mk_invalid_json
                           ( "Illegal type for field " ^ JSONName.as_string ContractUtil.this_address_label
                             ^ " specified in init json"))
end

end



(* Copied from Runner.ml *)

let run_with_args args =
  (* No gas is charged *)
  let gas_remaining = Stdint.Uint64.max_int in
  let this_address_opt, _ = get_init_this_address_and_extlibs args.input_init in


  match this_address_opt with
  | None -> 
      let msg = sprintf "No %s entry found in init file %s\n" (CUName.as_string ContractUtil.this_address_label) args.input_init in
            plog msg;
            fatal_error_gas 
              (mk_error0
                 (sprintf "Ran out of gas when parsing contract/init files.\n"))
              gas_remaining
        | Some this_address ->
            let elibs = import_libs cmod.elibs init_address_map in
            let dis_cmod = match Dis.disambiguate_cmodule cmod elibs init_address_map this_address with
              | Error e ->
                  plog (sprintf "%s\n" "Failed to disambiguate contract file.");
                  fatal_error_gas e gas_remaining
              | Ok res ->
                  plog (sprintf "\n[Disambiguation]:\nContract module [%s] is successfully disambiguated.\n" args.input);
                  res
            in
            (* Contract library. *)
            let clibs = dis_cmod.libs in
            
            (* Checking initialized libraries! *)
            let gas_remaining = check_libs clibs elibs args.input gas_remaining in
            let initargs =
              validate_get_init_json args.input_init gas_remaining cmod.smver
            in

            (* Retrieve block chain state  *)
            let bstate =
              try JSON.BlockChainState.get_json_data args.input_blockchain
              with Invalid_json s ->
                fatal_error_gas
                  ( s
                    @ mk_error0
                      (sprintf "Failed to parse json %s:\n" args.input_blockchain)
                  )
                  gas_remaining
            in
            let ( ( output_msg_json,
                    output_state_json,
                    output_events_json,
                    accepted_b ),
                  gas ) =
                (* Not initialization, execute transition specified in the message *)
                let mmsg =
                  try JSON.Message.get_json_data args.input_message
                  with Invalid_json s ->
                    fatal_error_gas
                      ( s
                        @ mk_error0
                          (sprintf "Failed to parse json %s:\n" args.input_message)
                      )
                      gas_remaining
                in
                let m = JSON.JSONLiteral.Msg mmsg in

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
                    let init_res =
                      init_module dis_cmod initargs curargs cur_bal bstate elibs
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

                (* Contract code *)
                let ctr = dis_cmod.contr in

                plog
                  (sprintf "Executing message:\n%s\n"
                     (JSON.Message.message_to_jstring mmsg));
                plog
                  (sprintf "In a Blockchain State:\n%s\n" (pp_literal_map bstate));
                let step_result = handle_message ctr cstate bstate m in
                let (cstate', mlist, elist, accepted_b), gas =
                  check_after_step step_result gas_remaining'
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
                let omj = output_message_json gas mlist in
                let oej = `List (output_event_json elist) in
                ((omj, osj, oej, accepted_b), gas)
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



(* TODO: 
   - Parse and disambiguate external libraries (needed for our own testcases)
   - Parse and disambiguate contract (needed for our own testcases)
   - Parse and disambiguate types and literals in init.json
   - Parse and disambiguate literals in state.json
   - Output disambiguated init.json and state.json *)
            


































(* Copied from JSON.ml *)

let constr_pattern_arg_types_exn = JSONParser.constr_pattern_arg_types_exn

let parse_as_name n =
  let open ParserLiteral.LType.TIdentifier.Name in
  match String.split_on_chars ~on:['.'] n with
  | [ t1 ; t2 ] -> parse_qualified_name t1 t2
  | [ t1 ] -> parse_simple_name t1
  | _ -> raise (mk_invalid_json (sprintf "Invalid name in json: %s\n" n))

let parse_typ_exn t =
  match JSONFrontEndParser.parse_type t with
  | Error _ -> raise (mk_invalid_json (sprintf "Invalid type in json: %s\n" t))
  | Ok s -> s

let build_prim_lit_exn t v =
  let open ParserLiteral in
  let open ParserLiteral.LType in
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
  let open Datatypes in
  let open ParserLiteral.LType.TIdentifier in
  let verify_args_exn cname provided expected =
    if provided <> expected then
      let p = Int.to_string provided in
      let e = Int.to_string expected in
      raise
        (mk_invalid_json
           ( "Malformed ADT constructor " ^ (Name.as_error_string cname) ^ ": expected " ^ e
             ^ " args, but provided " ^ p ^ "." ))
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
  let llist = List.map2_exn tmap ajs ~f:(fun t j -> json_to_lit t j) in
  ADTValue (cname, tlist, llist)



(* Copied from RunnerUtil.ml *)

let get_init_this_address_and_extlibs filename =
  if not (Caml.Sys.file_exists filename) then (
    plog (sprintf "Invalid init json %s file" filename);
    (None, []) )
  else
    try
      let this_address, name_addr_pairs =
        JSON.ContractState.get_init_this_address_and_extlibs filename in
      if
        List.contains_dup
          ~compare:(fun a b -> String.compare (fst a) (fst b))
          name_addr_pairs
      then
        fatal_error
        @@ mk_error0
          (sprintf "Duplicate extlib map entries in init JSON file %s."
             filename)
      else (this_address, name_addr_pairs)
    with Invalid_json s ->
      fatal_error
        (s @ mk_error0 (sprintf "Unable to parse JSON file %s. " filename))

(* Copied from Runner.ml *)

let map_json_input_strings_to_names map =
  (* Field names are disambiguated here for convenience (no actual disambiguation needed). 
     Field values are disambiguated later *)
  let open OutputName in
  List.map map ~f:(fun (x, l) ->
      match String.split x ~on:'.' with
      | [simple_name] -> (parse_simple_name simple_name, l)
      | _ -> raise (mk_invalid_json (sprintf "invalid name %s in json input" x)))

(* Parse the input state json and extract out _balance separately *)
let input_state_json filename =
  let open ContractState in
  let states_str = get_json_data filename in
  let states = map_json_input_strings_to_names states_str in
  let bal_lit =
    match List.Assoc.find states ContractUtil.balance_label ~equal:[%equal : RunnerName.t] with
    | Some v -> v
    | None -> raise @@ mk_invalid_json (sprintf "%s field missing" (RunnerName.as_string balance_label))
  in
  let bal_int =
    match bal_lit with
    | UintLit (Uint128L x) -> x
    | _ -> raise (mk_invalid_json (RunnerName.as_string balance_label ^ " invalid"))
  in
  let no_bal_states =
    List.Assoc.remove states balance_label ~equal:[%equal : RunnerName.t]
  in
  (no_bal_states, bal_int)

let validate_get_init_json init_file =
  (* Retrieve initial parameters *)
  let initargs_str =
    try ContractState.get_json_data init_file
    with Invalid_json s ->
      fatal_error_gas
        (s @ mk_error0 (sprintf "Failed to parse json %s:\n" init_file))
        Stdint.Uint64.zero
  in
  let initargs = map_json_input_strings_to_names initargs_str in
  (* Check for version mismatch. Subtract penalty for mismatch. *)
  let emsg = mk_error0 "Scilla version mismatch\n" in
  let rgas =
    Uint64.sub gas_remaining (Uint64.of_int Gas.version_mismatch_penalty)
  in
  let init_json_scilla_version =
    List.Assoc.find initargs ~equal:[%equal : RunnerName.t]
      ContractUtil.scilla_version_label
  in
  let () =
    match init_json_scilla_version with
    | Some (UintLit (Uint32L v)) ->
        let mver, _, _ = scilla_version in
        let v' = Uint32.to_int v in
        if v' <> mver || mver <> source_ver then fatal_error_gas emsg rgas
    | _ -> fatal_error_gas emsg rgas
  in
  initargs
