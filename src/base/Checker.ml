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
open ParserUtil
open Syntax
open ErrorUtils
open PrettyPrinters
open DebugMessage
open MonadUtil
open Result.Let_syntax
open RunnerUtil
open PatternChecker
open SanityChecker
open GasUseAnalysis
open RecursionPrinciples
open EventInfo
open TypeInfo
open Cashflow
open Accept
open Stdint
open Literal

(* Modules use local names, which are then disambiguated *)
module FEParser = FrontEndParser.ScillaFrontEndParser (LocalLiteral)
module Parser = FEParser.Parser
module ParserSyntax = FEParser.FESyntax
module PSRep = ParserRep
module PERep = ParserRep
module Dis = Disambiguate.ScillaDisambiguation (PSRep) (PERep)
module Rec = Recursion.ScillaRecursion (PSRep) (PERep)
module RecSRep = Rec.OutputSRep
module RecERep = Rec.OutputERep
module TC = TypeChecker.ScillaTypechecker (RecSRep) (RecERep)
module TCSRep = TC.OutputSRep
module TCERep = TC.OutputERep
module PMC = ScillaPatternchecker (TCSRep) (TCERep)
module PMCSRep = PMC.SPR
module PMCERep = PMC.EPR
module SC = ScillaSanityChecker (TCSRep) (TCERep)
module EI = ScillaEventInfo (PMCSRep) (PMCERep)
module GUA = ScillaGUA (TCSRep) (TCERep)
module CF = ScillaCashflowChecker (TCSRep) (TCERep)
module AC = ScillaAcceptChecker (TCSRep) (TCERep)
module TI = ScillaTypeInfo (TCSRep) (TCERep)

(* Check that the module parses *)
let check_parsing ctr syn =
  let cmod = FEParser.parse_file syn ctr in
  if Result.is_ok cmod then
    plog @@ sprintf "\n[Parsing]:\n module [%s] is successfully parsed.\n" ctr;
  cmod

(* Change local names to global names *)
let disambiguate_lmod lmod elibs names_and_addresses this_address =
  let open Dis in
  let res = disambiguate_lmodule lmod elibs names_and_addresses this_address in
  if Result.is_ok res then
    plog
    @@ sprintf "\n[Disambiguation]:\n lmodule [%s] is successfully checked.\n"
         (PreDisIdentifier.as_error_string lmod.libs.lname);
  res

(* Change local names to global names *)
let disambiguate_cmod cmod elibs names_and_addresses this_address =
  let open Dis in
  let res = disambiguate_cmodule cmod elibs names_and_addresses this_address in
  if Result.is_ok res then
    plog
    @@ sprintf "\n[Disambiguation]:\n cmodule [%s] is successfully checked.\n"
         (PreDisIdentifier.as_error_string cmod.contr.cname);
  res

(* Check restrictions on inductive datatypes, and on associated recursion principles *)
let check_recursion cmod elibs =
  let open Rec in
  let res = recursion_module cmod recursion_principles elibs in
  if Result.is_ok res then
    plog
    @@ sprintf "\n[Recursion Check]:\n module [%s] is successfully checked.\n"
         (RecIdentifier.as_error_string cmod.contr.cname);
  res

let check_recursion_lmod lmod elibs =
  let open Rec in
  let res = recursion_lmodule lmod recursion_principles elibs in
  if Result.is_ok res then
    plog
    @@ sprintf "\n[Recursion Check]:\n lmodule [%s] is successfully checked.\n"
         (RecIdentifier.as_error_string lmod.libs.lname);
  res

(* Type check the contract with external libraries *)
let check_typing cmod rprin elibs gas =
  let open TC in
  let res = type_module cmod rprin elibs gas in
  let _ =
    match res with
    | Ok (_, remaining_gas) ->
        plog
        @@ sprintf "\n[Type Check]:\n module [%s] is successfully checked.\n"
             (TCIdentifier.as_error_string cmod.contr.cname);
        let open Stdint.Uint64 in
        plog
        @@ sprintf "Gas remaining after typechecking: %s units.\n"
             (to_string remaining_gas)
    | _ -> ()
  in
  res

(* Type check the contract with external libraries *)
let check_typing_lmod lmod rprin elibs gas =
  let open TC in
  strip_error_type
  @@
  let res = type_lmodule lmod rprin elibs gas in
  let _ =
    match res with
    | Ok (_, remaining_gas) ->
        plog
        @@ sprintf "\n[Type Check]:\n lmodule [%s] is successfully checked.\n"
             (TCIdentifier.as_error_string lmod.libs.lname);
        let open Stdint.Uint64 in
        plog
        @@ sprintf "Gas remaining after typechecking: %s units.\n"
             (to_string remaining_gas)
    | _ -> ()
  in
  res

let check_patterns e rlibs elibs =
  let res = PMC.pm_check_module e rlibs elibs in
  if Result.is_ok res then
    plog
    @@ sprintf "\n[Pattern Check]:\n module [%s] is successfully checked.\n"
         (PMC.PCIdentifier.as_error_string e.contr.cname);
  res

let check_patterns_lmodule e rlibs elibs =
  let res = PMC.pm_check_lmodule e rlibs elibs in
  if Result.is_ok res then
    plog
    @@ sprintf "\n[Pattern Check]:\n library module is successfully checked.\n";
  res

let check_sanity m rlibs elibs =
  let res = SC.contr_sanity m rlibs elibs in
  if Result.is_ok res then
    plog
    @@ sprintf "\n[Sanity Check]:\n module [%s] is successfully checked.\n"
         (SC.SCIdentifier.as_error_string m.contr.cname);
  res

let check_sanity_lmod m rlibs elibs =
  let res = SC.lmod_sanity m rlibs elibs in
  if Result.is_ok res then
    plog
    @@ sprintf "\n[Sanity Check]:\n module [%s] is successfully checked.\n"
         (SC.SCIdentifier.as_error_string m.libs.lname);
  res

let check_accepts m = AC.contr_sanity m

let analyze_print_gas cmod typed_elibs =
  let res = GUA.gua_module cmod typed_elibs in
  match res with
  | Error msg ->
      pout @@ scilla_error_to_string msg;
      res
  | Ok cpol ->
      plog
      @@ sprintf
           "\n[Gas Use Analysis]:\n module [%s] is successfully analyzed.\n"
           (GUA.GUAIdentifier.as_error_string cmod.contr.cname);
      let _ =
        List.iter
          ~f:(fun (i, pol) ->
            pout
            @@ sprintf "Gas use polynomial for transition %s:\n%s\n\n"
                 (GUA.GUAIdentifier.as_error_string i)
                 (GUA.sprint_gup pol))
          cpol
      in
      res

let check_cashflow typed_cmod token_fields =
  let param_field_tags, ctr_tags = CF.main typed_cmod token_fields in
  let param_field_tags_to_string =
    List.map param_field_tags ~f:(fun (i, t) ->
        (i, CF.ECFR.money_tag_to_string t))
  in
  let ctr_tags_to_string =
    let open Datatypes in
    (* Using as_error_string to ensure that localised names are output *)
    List.map ctr_tags ~f:(fun (adt, ctrs) ->
        ( DTName.as_string adt,
          List.map ctrs ~f:(fun (i, ts) ->
              ( DTName.as_string i,
                List.map ts ~f:(fun t_opt ->
                    Option.value_map t_opt ~default:"_"
                      ~f:CF.ECFR.money_tag_to_string) )) ))
  in
  (param_field_tags_to_string, ctr_tags_to_string)

let check_version vernum =
  let mver, _, _ = scilla_version in
  if vernum <> mver then
    let emsg =
      sprintf "Scilla version mismatch. Expected %d vs Contract %d\n" mver
        vernum
    in
    fatal_error (mk_error0 emsg)

let wrap_error_with_gas gas res =
  match res with Ok r -> Ok r | Error e -> Error (e, gas)

let check_lmodule cli =
  let r =
    let initial_gas = Uint64.mul Gas.scale_factor cli.gas_limit in
    let%bind (lmod : ParserSyntax.lmodule) =
      wrap_error_with_gas initial_gas
      @@ check_parsing cli.input_file Parser.Incremental.lmodule
    in
    let this_address_opt, init_address_map =
      Option.value_map cli.init_file ~f:get_init_this_address_and_extlibs
        ~default:(None, [])
    in
    let this_address =
      Option.value this_address_opt
        ~default:(FilePath.chop_extension (FilePath.basename cli.input_file))
    in
    let elibs = import_libs lmod.elibs init_address_map in
    let%bind dis_lmod =
      wrap_error_with_gas initial_gas
      @@ disambiguate_lmod lmod elibs init_address_map this_address
    in
    let%bind recursion_lmod, recursion_rec_principles, recursion_elibs =
      wrap_error_with_gas initial_gas @@ check_recursion_lmod dis_lmod elibs
    in
    let%bind (typed_lmod, typed_rlibs, typed_elibs), remaining_gas =
      check_typing_lmod recursion_lmod recursion_rec_principles recursion_elibs
        initial_gas
    in
    let%bind () =
      Result.ignore_m
      @@ wrap_error_with_gas remaining_gas
      @@ check_patterns_lmodule typed_lmod typed_rlibs typed_elibs
    in
    let%bind () =
      Result.ignore_m
      @@ wrap_error_with_gas remaining_gas
      @@ check_sanity_lmod typed_lmod typed_rlibs typed_elibs
    in
    let type_info =
      if cli.p_type_info then TI.type_info_lmod typed_lmod else []
    in
    let remaining_gas' =
      Gas.finalize_remaining_gas cli.gas_limit remaining_gas
    in
    pure ((typed_lmod, typed_rlibs, typed_elibs), type_info, remaining_gas')
  in
  match r with
  | Error (s, g) -> fatal_error_gas_scale Gas.scale_factor s g
  | Ok (_, type_info, g) ->
      let json_output =
        if cli.p_type_info then
          [ ("type_info", JSON.TypeInfo.type_info_to_json type_info) ]
        else []
      in
      if GlobalConfig.use_json_errors () || not (List.is_empty json_output) then
        let warnings_and_gas_output =
          [
            ("warnings", scilla_warning_to_json (get_warnings ()));
            ("gas_remaining", `String (Stdint.Uint64.to_string g));
          ]
          @ json_output
        in
        let j = `Assoc warnings_and_gas_output in
        sprintf "%s\n" (Yojson.Basic.pretty_to_string j)
      else
        scilla_warning_to_sstring (get_warnings ())
        ^ "\ngas_remaining: " ^ Stdint.Uint64.to_string g ^ "\n"

let rec has_user_def_adt t =
  let open TC.TypedSyntax.SType in
  let open Datatypes in
  match t with
  | PrimType _ | TypeVar _ | Unit | Address _ -> false
  | PolyFun (_, t) -> has_user_def_adt t
  | MapType (t1, t2)
  | FunType (t1, t2) -> has_user_def_adt t1 || has_user_def_adt t2
  | ADT (n, targs)
    when is_bool_adt_name   (TIdentifier.get_id n) ||
         is_list_adt_name   (TIdentifier.get_id n) ||
         is_pair_adt_name   (TIdentifier.get_id n) ||
         is_nat_adt_name    (TIdentifier.get_id n) ||
         is_option_adt_name (TIdentifier.get_id n)
    -> List.exists targs ~f:has_user_def_adt
  | ADT _ -> true
    
let check_tparams_for_userdef_adts comps =
  let open TC.TypedSyntax in
  let open Datatypes in
  List.fold_left comps ~init:[] ~f:(fun acc comp ->
      let { comp_type; comp_name; comp_params; _ } = comp in
      match comp_type with
      | CompTrans ->
          if List.exists comp_params ~f:(fun (_, typ) -> has_user_def_adt typ) then
            (DTIdentifier.as_string comp_name) :: acc
          else acc
      | CompProc -> acc)

let check_msgs_for_userdef_adts (cmod : TC.TypedSyntax.cmodule) =
  let open TC.TypedSyntax in
  let open TC.TU in
  let rec check_exp acc e =
    match fst e with
    | Literal _ | Var _ | App _ | Constr _ | Builtin _ | TApp _ ->
        acc
    | Fun (_, _, body) | TFun (_, body) | Fixpoint (_, _, body) | GasExpr (_, body) ->
        check_exp acc body
    | Let (_, _, lhs, rhs) ->
        check_exp (check_exp acc lhs) rhs
    | MatchExpr (_, pes) ->
        List.fold_left pes ~init:acc ~f:(fun acc (_, e) ->
            check_exp acc e)
    | Message msg ->
        if List.exists msg ~f:(fun (_, pl) ->
            match pl with
            | MLit l -> (
                match literal_type l with
                | Ok t -> has_user_def_adt t
                | Error _ -> false)
            | MVar v ->
                has_user_def_adt ((TCERep.get_type (SIdentifier.get_rep v)).tp))
        then
          match List.find msg ~f:(fun (f, _) -> String.(f = ContractUtil.MessagePayload.tag_label)) with
          | Some (_, MLit l) -> (pp_literal l) :: acc
          | Some (_, MVar v) -> (SIdentifier.as_string v) :: acc
          | None -> acc
        else acc
  in
  let rec check_stmt acc s =
    match fst s with
    | Load _ | RemoteLoad _ | Store _ | MapUpdate _ | MapGet _ | RemoteMapGet _ | ReadFromBC _
    | AcceptPayment | Iterate _ | SendMsgs _ | CreateEvnt _ | CallProc _ | Throw _ | GasStmt _ ->
        acc
    | Bind (_, e) ->
        check_exp acc e
    | MatchStmt (_, pss) ->
        List.fold_left pss ~init:acc ~f:(fun acc (_, ss) ->
            List.fold_left ss ~init:acc ~f:(fun acc s -> check_stmt acc s))
  in
  let check_lib_entry acc le =
    let open TC.TypedSyntax in
    match le with
    | LibVar (_, _, body) -> check_exp acc body
    | LibTyp _ -> acc
  in
  let lib_msgs = Option.value_map cmod.libs ~default:[] ~f:(fun lib -> List.fold_left lib.lentries ~init:[] ~f:check_lib_entry) in
  let constraint_msgs = check_exp lib_msgs cmod.contr.cconstraint in
  let field_init_msgs = List.fold_left cmod.contr.cfields ~init:constraint_msgs ~f:(fun acc (_, _, i) -> check_exp acc i) in
  List.fold_left cmod.contr.ccomps ~init:field_init_msgs ~f:(fun acc c -> List.fold_left c.comp_body ~init:acc ~f:check_stmt)
  
let check_for_hash_usage (cmod : TC.TypedSyntax.cmodule) =
  let open TC.TypedSyntax in
  let rec check_exp acc e =
    match fst e with
    | Builtin ((b, _), _, ids)
      when [%equal : builtin ] b Builtin_sha256hash ||
           [%equal : builtin ] b Builtin_keccak256hash ||
           [%equal : builtin ] b Builtin_ripemd160hash ->
        if List.exists ids ~f:(fun id -> has_user_def_adt ((TCERep.get_type (SIdentifier.get_rep id)).tp))
        then
          (pp_builtin b) :: acc
        else
          acc
    | Builtin _ | Literal _ | Var _ | App _ | Constr _  | TApp _ | Message _ ->
        acc
    | Fun (_, _, body) | TFun (_, body) | Fixpoint (_, _, body) | GasExpr (_, body) ->
        check_exp acc body
    | Let (_, _, lhs, rhs) ->
        check_exp (check_exp acc lhs) rhs
    | MatchExpr (_, pes) ->
        List.fold_left pes ~init:acc ~f:(fun acc (_, e) ->
            check_exp acc e)
  in
  let rec check_stmt acc s =
    match fst s with
    | Load _ | RemoteLoad _ | Store _ | MapUpdate _ | MapGet _ | RemoteMapGet _ | ReadFromBC _
    | AcceptPayment | Iterate _ | SendMsgs _ | CreateEvnt _ | CallProc _ | Throw _ | GasStmt _ ->
        acc
    | Bind (_, e) ->
        check_exp acc e
    | MatchStmt (_, pss) ->
        List.fold_left pss ~init:acc ~f:(fun acc (_, ss) ->
            List.fold_left ss ~init:acc ~f:(fun acc s -> check_stmt acc s))
  in
  let check_lib_entry acc le =
    let open TC.TypedSyntax in
    match le with
    | LibVar (_, _, body) -> check_exp acc body
    | LibTyp _ -> acc
  in
  let lib_msgs = Option.value_map cmod.libs ~default:[] ~f:(fun lib -> List.fold_left lib.lentries ~init:[] ~f:check_lib_entry) in
  let constraint_msgs = check_exp lib_msgs cmod.contr.cconstraint in
  let field_init_msgs = List.fold_left cmod.contr.cfields ~init:constraint_msgs ~f:(fun acc (_, _, i) -> check_exp acc i) in
  List.fold_left cmod.contr.ccomps ~init:field_init_msgs ~f:(fun acc c -> List.fold_left c.comp_body ~init:acc ~f:check_stmt)

(* Check a contract module. *)
let check_cmodule cli =
  let r =
    let initial_gas = Uint64.mul Gas.scale_factor cli.gas_limit in
    let%bind (cmod : ParserSyntax.cmodule) =
      wrap_error_with_gas initial_gas
      @@ check_parsing cli.input_file Parser.Incremental.cmodule
    in
    (* Import whatever libs we want. *)
    let this_address_opt, init_address_map =
      Option.value_map cli.init_file ~f:get_init_this_address_and_extlibs
        ~default:(None, [])
    in
    let this_address =
      Option.value this_address_opt
        ~default:(FilePath.chop_extension (FilePath.basename cli.input_file))
    in
    let elibs = import_libs cmod.elibs init_address_map in
    let%bind dis_cmod =
      wrap_error_with_gas initial_gas
      @@ disambiguate_cmod cmod elibs init_address_map this_address
    in
    let%bind recursion_cmod, recursion_rec_principles, recursion_elibs =
      wrap_error_with_gas initial_gas @@ check_recursion dis_cmod elibs
    in
    let%bind (typed_cmod, _tenv, _typed_elibs, _typed_rlibs), _remaining_gas =
      check_typing recursion_cmod recursion_rec_principles recursion_elibs
        initial_gas
    in
    let comps_with_userdef_adt_as_tparam = check_tparams_for_userdef_adts typed_cmod.contr.ccomps in
    let msgs_tags_when_userdef_adt_as_param = check_msgs_for_userdef_adts typed_cmod in
    let hash_usage = check_for_hash_usage typed_cmod in
(*
    let%bind pm_checked_cmod, _pm_checked_rlibs, _pm_checked_elibs =
      wrap_error_with_gas remaining_gas
      @@ check_patterns typed_cmod typed_rlibs typed_elibs
    in
    let _ = if cli.cf_flag then check_accepts typed_cmod else () in
    let _type_info =
      if cli.p_type_info then TI.type_info_cmod typed_cmod else []
    in
    let%bind () =
      wrap_error_with_gas remaining_gas
      @@ check_sanity typed_cmod typed_rlibs typed_elibs
    in
    let%bind _event_info =
      wrap_error_with_gas remaining_gas @@ EI.event_info pm_checked_cmod
    in
    let%bind () =
      Result.ignore_m
      @@
      if cli.gua_flag then
        wrap_error_with_gas remaining_gas
        @@ analyze_print_gas typed_cmod typed_elibs
      else pure []
    in
    let _cf_info_opt =
      if cli.cf_flag then Some (check_cashflow typed_cmod cli.cf_token_fields)
      else None
    in
    let _remaining_gas' =
      Gas.finalize_remaining_gas cli.gas_limit remaining_gas
    in
        pure @@ (dis_cmod, tenv, event_info, type_info, cf_info_opt, remaining_gas')
*)
    pure @@ (this_address, comps_with_userdef_adt_as_tparam, msgs_tags_when_userdef_adt_as_param, hash_usage)
  in
  match r with
  | Error (s, g) -> fatal_error_gas_scale Gas.scale_factor s g
                      (*   | Ok (cmod, _, event_info, type_info, cf_info_opt, g) -> *)
  | Ok (this_address, comps_with_userdef_adt_as_tparam, msgs_tags_when_userdef_adt_as_param, hash_usage) ->
(*      check_version cmod.smver;
      let output =
        if cli.p_contract_info then
          [
            ( "contract_info",
              JSON.ContractInfo.get_json cmod.smver cmod.contr event_info );
          ]
        else []
      in
      let output =
        if cli.p_type_info then
          ("type_info", JSON.TypeInfo.type_info_to_json type_info) :: output
        else output
      in
      let output =
        match cf_info_opt with
        | None -> output
        | Some cf_info ->
            ("cashflow_tags", JSON.CashflowInfo.get_json cf_info) :: output
      in
      let output =
        (* This part only has warnings and gas_remaining, which we output as JSON
         * if either `-jsonerrors` OR if there is other JSON output. *)
        if GlobalConfig.use_json_errors () || not (List.is_empty output) then
          output
          @ [
              ("warnings", scilla_warning_to_json (get_warnings ()));
              ("gas_remaining", `String (Stdint.Uint64.to_string g));
            ]
        else output
      in
      (* We print as a JSON if `-jsonerrors` OR if there is some JSON data to display. *)
      if GlobalConfig.use_json_errors () || not (List.is_empty output) then
        let j = `Assoc output in
        sprintf "%s\n" (Yojson.Basic.pretty_to_string j)
      else
        scilla_warning_to_sstring (get_warnings ())
        ^ "\ngas_remaining: " ^ Stdint.Uint64.to_string g ^ "\n"
*)
      sprintf "{ \"address\" : \"%s\"%s%s%s }"
        this_address
        (if not @@ List.is_empty comps_with_userdef_adt_as_tparam then 
           sprintf ", \"ADT transitions\" : [ \"%s\" ]"
             (String.concat ~sep:"\", \"" comps_with_userdef_adt_as_tparam)
         else "")
        (if not @@ List.is_empty msgs_tags_when_userdef_adt_as_param then 
           sprintf ", \"ADT messages\" : [ \"%s\" ]"
             (String.concat ~sep:"\", \"" msgs_tags_when_userdef_adt_as_param)
         else "")
        (if not @@ List.is_empty hash_usage then 
           sprintf ", \"ADT hashes\" : [ \"%s\" ]"
             (String.concat ~sep:"\", \"" hash_usage)
         else "")

let run args ~exe_name =
  GlobalConfig.reset ();
  ErrorUtils.reset_warnings ();
  Datatypes.DataTypeDictionary.reinit ();
  let cli = parse_cli args ~exe_name in
  let open GlobalConfig in
  StdlibTracker.add_stdlib_dirs cli.stdlib_dirs;
  (* Get list of stdlib dirs. *)
  let lib_dirs = StdlibTracker.get_stdlib_dirs () in
  if List.is_empty lib_dirs then stdlib_not_found_err ~exe_name ();

  let open FilePath in
  let open StdlibTracker in
  if check_extension cli.input_file file_extn_contract then
    (* Check contract modules. *)
    check_cmodule cli
  else fatal_error (mk_error0 (sprintf "Unknown file extension\n"))
