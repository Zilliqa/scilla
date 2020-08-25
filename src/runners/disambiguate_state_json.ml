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
