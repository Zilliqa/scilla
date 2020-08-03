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

module Tests = Scilla_test.Util.DiffBasedTests (struct
  let gold_path dir f = [ dir; "checker"; "bad"; "gold"; f ^ ".gold" ]

  let test_path f = [ "checker"; "bad"; f ]

  let runner = "scilla-checker"

  let ignore_predef_args = false

  let json_errors = true

  let gas_limit = Stdint.Uint64.of_int 8000

  let custom_args = [ "-cf"; "-contractinfo" ]

  let additional_libdirs = []

  let provide_init_arg = false

  let tests =
    [
      "bad_fields1.scilla";
      "bad_fields2.scilla";
      "bad_fields3.scilla";
      "bad_fields4.scilla";
      "bad_comment_1.scilla";
      "bad_comment_2.scilla";
      "bad_map_key_1.scilla";
      "bad_map_key_5.scilla";
      "map_value_function.scilla";
      "bad_message1.scilla";
      "bad_message2.scilla";
      "message_field.scilla";
      "message_field2.scilla";
      "message_field3.scilla";
      "send_event1.scilla";
      "send_event2.scilla";
      "unbound.scilla";
      "event_bad1.scilla";
      "event_bad2.scilla";
      "lib_bad1.scilla";
      "zil_mod.scilla";
      "mappair2.scilla";
      "mappair.scilla";
      "inplace-map.scilla";
      "homonymous_vars.scilla";
      "homonymous_vars2.scilla";
      "homonymous_vars3.scilla";
      "bad_adt_1.scilla";
      "bad_adt_2.scilla";
      "bad_adt_3.scilla";
      "bad_adt_4.scilla";
      "bad_adt_7.scilla";
      "bad_adt_8.scilla";
      "unserializable_param.scilla";
      "unstorable_adt.scilla";
      "bad_version.scilla";
      "balance_field.scilla";
      "bad_param.scilla";
      "bad_transition_param.scilla";
      "recursive_procedure.scilla";
      "mutually_recursive_procedure.scilla";
      "forward_definition_procedure.scilla";
      "procedure_bad_type.scilla";
      "procedure_bad_args.scilla";
      "procedure_map_arg.scilla";
      "name_clashes.scilla";
      "procedure_env.scilla";
      "global_scope_procedures.scilla";
      "bad-exception1.scilla";
      "blowup.scilla";
      "BadPMLib.scillib";
      "constraint_field_not_in_scope.scilla";
      "constraint_locals_not_in_scope.scilla";
      "constraint_type_illegal.scilla";
      "match-in-transition.scilla";
      "listiter-bad.scilla";
    ]

  let exit_code : UnixLabels.process_status = WEXITED 1
end)

module LibTests = Scilla_test.Util.DiffBasedTests (struct
  let gold_path dir f = [ dir; "checker"; "bad"; "gold"; f ^ ".gold" ]

  let test_path f = [ "checker"; "bad"; f ]

  let runner = "scilla-checker"

  let ignore_predef_args = false

  let json_errors = true

  let gas_limit = Stdint.Uint64.of_int 8000

  let custom_args = [ "-cf" ]

  let additional_libdirs = [ [ "checker"; "bad"; "lib" ] ]

  let provide_init_arg = false

  let tests =
    [
      "bad_adt_lib_1.scilla";
      "bad_adt_lib_2.scilla";
      "bad_adt_lib_3.scilla";
      "bad_adt_lib_4.scilla";
      "bad_adt_lib_5.scilla";
      "bad_adt_lib_6.scilla";
      "libchaincycle.scilla";
      "libindirect.scilla";
      "libdiamondcycle.scilla";
      "libdup1.scilla";
      "libdup2.scilla";
      "namespace1.scilla";
      "TestLibNS1.scillib";
      "bad_lib_type.scilla";
      "bad_lib_type2.scilla";
      "bad_lib_type3.scilla";
      "bad_lib_pm_import.scilla";
    ]

  let exit_code : UnixLabels.process_status = WEXITED 1
end)

(* The test here require the `-init` argument. This is required for
 * importing libraries whose addresses are specified in the init JSON *)
module InitArgTests = Scilla_test.Util.DiffBasedTests (struct
  let gold_path dir f = [ dir; "checker"; "bad"; "gold"; f ^ ".gold" ]

  let test_path f = [ "checker"; "bad"; f ]

  let runner = "scilla-checker"

  let ignore_predef_args = false

  let json_errors = true

  let gas_limit = Stdint.Uint64.of_int 8000

  let custom_args = []

  let provide_init_arg = true

  let additional_libdirs = [ [ "checker"; "bad"; "lib" ] ]

  let tests = [ "extlib_dup_entry.scilla"; "bad_init.scilla" ]

  let exit_code : UnixLabels.process_status = WEXITED 1
end)
