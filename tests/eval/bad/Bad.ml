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
open OUnit2

let explist =
  [
    "app_error1.scilexp";
    "app_error2.scilexp";
    "builtin4.scilexp";
    "blowup.scilexp";
    (* Should fail with out-of-gas. *)
    "builtin-divzero.scilexp";
    "builtin-divzero2.scilexp";
    "builtin-divzero3.scilexp";
    "builtin-divzero4.scilexp";
    "builtin-remzero.scilexp";
    "builtin-overflow10.scilexp";
    "builtin-overflow11.scilexp";
    "builtin-overflow12.scilexp";
    "builtin-overflow13.scilexp";
    "builtin-overflow14.scilexp";
    "builtin-overflow1.scilexp";
    "builtin-overflow2.scilexp";
    "builtin-overflow3.scilexp";
    "builtin-overflow5.scilexp";
    "builtin-overflow6.scilexp";
    "builtin-overflow7.scilexp";
    "builtin-overflow8.scilexp";
    "builtin-overflow9.scilexp";
    "builtin-pow.scilexp";
    "builtin-pow2.scilexp";
    "builtin-bech32-1.scilexp";
    "builtin-bech32-2.scilexp";
    "builtin-bech32-3.scilexp";
    "builtin-alt-bn128-add.scilexp";
    "builtin-alt-bn128-mul.scilexp";
    "msg_error2.scilexp";
    "msg_error3.scilexp";
    "msg_error.scilexp";
    "let-error.scilexp";
    "list_to_map.scilexp";
    "string_error1.scilexp";
    "substr_err1.scilexp";
  ]

module Tests = Scilla_test.Util.DiffBasedTests (struct
  let gold_path dir f = [ dir; "eval"; "bad"; "gold"; f ^ ".gold" ]

  let test_path f = [ "eval"; "bad"; f ]

  let runner = "eval-runner"

  let ignore_predef_args = false

  let json_errors = true

  let gas_limit = Stdint.Uint64.of_int 4002000

  let custom_args = []

  let additional_libdirs = []

  let provide_init_arg = false

  let tests = explist

  let exit_code : UnixLabels.process_status = WEXITED 1
end)

let tests env = "bad" >::: [ Tests.tests env ]
