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

(* Add tests in alphabetical order *)

module Tests = TestUtil.DiffBasedTests(
  struct
    let gold_path dir f = [dir; "parser"; "bad"; "gold"; f ^ ".gold" ]
    let test_path f = ["parser"; "bad"; f]
    let runner = "scilla-checker"
    let gas_limit = Stdint.Uint64.of_int 8000
    let custom_args = ["-contractinfo"]
    let additional_libdirs = []
    let tests = [
        "bad_map_key_2.scilla";
        "bad_map_key_3.scilla";
        "bad_map_key_4.scilla";
        "cmodule-contract-cid-lparen-id-colon-comma-with.scilla";
        "cmodule-contract-cid-lparen-id-colon-tid-with.scilla";
        "cmodule-contract-cid-lparen-id-colon-with.scilla";
        "cmodule-contract-cid-lparen-id-with.scilla";
        "cmodule-field-id-colon-cid-eq-hexlit-with.scilla";
        "cmodule-field-id-colon-tid-eq-with.scilla";
        "cmodule-field-id-colon-tid-with.scilla";
        "cmodule-field-id-colon-with.scilla";
        "cmodule-field-id-with.scilla";
        "cmodule-field-with.scilla";
        "cmodule-procedure-id-lparen-rparen-with.scilla";
        "cmodule-procedure-id-with.scilla";
        "cmodule-procedure-with.scilla";
        "cmodule-transition-id-lparen-rparen-end-with.scilla";
        "cmodule-transition-id-lparen-rparen-throw-bar.scilla";
        "cmodule-transition-id-lparen-rparen-with.scilla";
        "cmodule-transition-id-lparen-with.scilla";
        "cmodule-transition-id-with.scilla";
        "cmodule-transition-with.scilla";
        "cmodule-version-number-contract-name-lparen-rparen-with.scilla";
        "cmodule-version-number-contract-name-lparen-with.scilla";
        "cmodule-version-number-contract-name-with.scilla";
        "cmodule-version-number-contract-with.scilla";
        "cmodule-version-number-library-name.scilla";
        "cmodule-version-number-with.scilla";
        "cmodule-version-with.scilla";
        "cmodule-with.scilla";
        "lmodule-import-cid-as-cid-with.scilla";
        "lmodule-import-cid-as-with.scilla";
        "lmodule-import-cid-with.scilla";
        "stmts_t-accept-with.scilla";
        "stmts_t-cid-with.scilla";
        "stmts_t-delete-id-with.scilla";
        "stmts_t-delete-with.scilla";
        "stmts_t-event-with.scilla";
        "stmts_t-id-assign-with.scilla";
        "stmts_t-id-bind-and-with.scilla";
        "stmts_t-id-bind-exists-id-with.scilla";
        "stmts_t-id-bind-exists-with.scilla";
        "stmts_t-id-bind-id-with.scilla";
        "stmts_t-id-bind-with.scilla";
        "stmts_t-id-eq-with.scilla";
        "stmts_t-id-lsqb-spid-rsqb-assign-with.scilla";
        "stmts_t-id-lsqb-spid-rsqb-semicolon.scilla";
        "stmts_t-id-lsqb-spid-rsqb-with.scilla";
        "stmts_t-id-lsqb-spid-with.scilla";
        "stmts_t-id-lsqb-with.scilla";
        "stmts_t-id-with.scilla";
        "stmts_t-match-spid-underscore.scilla";
        "stmts_t-match-spid-with-bar-underscore-arrow-accept-eof.scilla";
        "stmts_t-match-spid-with-bar-underscore-arrow-with.scilla";
        "stmts_t-match-spid-with-bar-underscore-with.scilla";
        "stmts_t-match-spid-with-bar-with.scilla";
        "stmts_t-match-spid-with-with.scilla";
        "stmts_t-match-with.scilla";
        "stmts_t-send-cid-period-with.scilla";
        "stmts_t-send-cid-with.scilla";
        "stmts_t-send-with.scilla";
        "stmts_t-throw-semicolon-with.scilla";
        "stmts_t-throw-with.scilla";
        "type_t-cid-lparen-tid-with.scilla";
        "type_t-cid-lparen-with.scilla";
        "type_t-cid-map-cid-underscore.scilla";
        "type_t-cid-map-with.scilla";
        "type_t-cid-period-with.scilla";
        "type_t-cid-tid-with.scilla";
        "type_t-cid-underscore.scilla";
        "type_t-cid-with.scilla";
        "type_t-forall-tid-period-tid-with.scilla";
        "type_t-forall-tid-period-with.scilla";
        "type_t-forall-tid-with.scilla";
        "type_t-forall-with.scilla";
        "type_t-lparen-tid-with.scilla";
        "type_t-map-cid-cid-cid-underscore.scilla";
        "type_t-map-cid-cid-lparen-cid-underscore.scilla";
        "type_t-map-cid-cid-lparen-with.scilla";
        "type_t-map-cid-cid-map-cid-underscore.scilla";
        "type_t-map-cid-cid-map-with.scilla";
        "type_t-map-cid-cid-underscore.scilla";
        "type_t-map-cid-lparen-cid-cid-type.scilla";
        "type_t-map-cid-lparen-map-cid-cid-underscore.scilla";
        "type_t-map-cid-lparen-map-cid-underscore.scilla";
        "type_t-map-cid-lparen-map-with.scilla";
        "type_t-map-cid-lparen-with.scilla";
        "type_t-map-cid-map-cid-underscore.scilla";
        "type_t-map-cid-map-with.scilla";
        "type_t-map-with.scilla";
        "type_t-tid-arrow-tid-with.scilla";
        "type_t-tid-arrow-with.scilla";
    ]
    let exit_code : Unix.process_status = WEXITED 1
  end)


module LibTests = TestUtil.DiffBasedTests(
  struct
    let gold_path dir f = [dir; "parser"; "bad"; "gold"; f ^ ".gold" ]
    let test_path f = ["parser"; "bad"; "lib"; f]
    let runner = "scilla-checker"
    let gas_limit = Stdint.Uint64.of_int 8000
    let custom_args = []
    let additional_libdirs = [["parser"; "bad"; "lib"]]
    let tests = [
        "lmodule-import-contract.scillib";
        "lmodule-import-with.scillib";
        "lmodule-library-cid-contract.scillib";
        "lmodule-library-cid-let-id-colon-tid-eq-with.scillib";
        "lmodule-library-cid-let-id-eq-hexlit-with.scillib";
        "lmodule-library-cid-let-id-eq-with.scillib";
        "lmodule-library-cid-let-id-with.scillib";
        "lmodule-library-cid-let-with.scillib";
        "lmodule-library-cid-type-cid-eq-bar-cid-of-cid-transition.scillib";
        "lmodule-library-cid-type-cid-eq-bar-cid-of-with.scillib";
        "lmodule-library-cid-type-cid-eq-bar-cid-with.scillib";
        "lmodule-library-cid-type-cid-eq-bar-with.scillib";
        "lmodule-library-cid-type-cid-eq-with.scillib";
        "lmodule-library-cid-type-cid-with.scillib";
        "lmodule-library-cid-type-with.scillib";
        "lmodule-library-cid-with.scillib";
        "lmodule-library-with.scillib";
        "lmodule-version-with.scillib";
        "lmodule-with.scillib";
    ]
    let exit_code : Unix.process_status = WEXITED 1
  end)

module ExpTests = TestUtil.DiffBasedTests(
    struct
      let gold_path dir f = [dir; "parser"; "bad"; "gold"; f ^ ".gold" ]
      let test_path f = ["parser"; "bad"; "exps"; f]
      let runner = "type-checker"
      let gas_limit = Stdint.Uint64.of_int 8000
      let custom_args = []
      let additional_libdirs = []
      let tests = [
          "bad-map-key.scilexp";
          "bad-map-key2.scilexp";
          "builtin-overflow4.scilexp";
          "builtin_error1.scilexp";
          "exp_t-at-spid-tid-with.scilexp";
          "exp_t-at-spid-with.scilexp";
          "exp_t-at-with.scilexp";
          "exp_t-builtin-id-lparen-with.scilexp";
          "exp_t-builtin-id-with.scilexp";
          "exp_t-builtin-with.scilexp";
          "exp_t-cid-lbrace-rbrace-at.scilexp";
          "exp_t-cid-lbrace-tid-eq.scilexp";
          "exp_t-cid-lbrace-with.scilexp";
          "exp_t-cid-period-cid-with.scilexp";
          "exp_t-cid-period-with.scilexp";
          "exp_t-cid-with.scilexp";
          "exp_t-emp-cid-underscore.scilexp";
          "exp_t-emp-with.scilexp";
          "exp_t-fun-lparen-id-colon-tid-rparen-arrow-with.scilexp";
          "exp_t-fun-lparen-id-colon-tid-rparen-with.scilexp";
          "exp_t-fun-lparen-id-colon-tid-with.scilexp";
          "exp_t-fun-lparen-id-colon-with.scilexp";
          "exp_t-fun-lparen-id-with.scilexp";
          "exp_t-fun-lparen-with.scilexp";
          "exp_t-fun-with.scilexp";
          "exp_t-lbrace-spid-colon-cid-with.scilexp";
          "exp_t-lbrace-spid-colon-hexlit-semicolon-with.scilexp";
          "exp_t-lbrace-spid-colon-hexlit-with.scilexp";
          "exp_t-lbrace-spid-colon-with.scilexp";
          "exp_t-lbrace-spid-with.scilexp";
          "exp_t-lbrace-with.scilexp";
          "exp_t-let-id-colon-tid-eq-string-in-with.scilexp";
          "exp_t-let-id-colon-tid-eq-string-with.scilexp";
          "exp_t-let-id-colon-tid-eq-with.scilexp";
          "exp_t-let-id-colon-tid-with.scilexp";
          "exp_t-let-id-colon-with.scilexp";
          "exp_t-let-id-eq-string-in-with.scilexp";
          "exp_t-let-id-eq-with.scilexp";
          "exp_t-let-id-with.scilexp";
          "exp_t-match-spid-underscore.scilexp";
          "exp_t-match-spid-with-bar-cid-lparen-underscore-with.scilexp";
          "exp_t-match-spid-with-bar-cid-lparen-with.scilexp";
          "exp_t-match-spid-with-bar-cid-type.scilexp";
          "exp_t-match-spid-with-bar-cid-underscore-with.scilexp";
          "exp_t-match-spid-with-bar-underscore-arrow-hexlit-with.scilexp";
          "exp_t-match-spid-with-bar-underscore-arrow-with.scilexp";
          "exp_t-match-spid-with-bar-underscore-with.scilexp";
          "exp_t-match-spid-with-with.scilexp";
          "exp_t-match-with.scilexp";
          "exp_t-spid-cid-period-with.scilexp";
          "exp_t-spid-cid-with.scilexp";
          "exp_t-spid-spid-with.scilexp";
          "exp_t-string-with.scilexp";
          "exp_t-tfun-tid-arrow-with.scilexp";
          "exp_t-tfun-tid-with.scilexp";
          "exp_t-tfun-with.scilexp";
          "exp_t-with.scilexp";
      ]
      let exit_code : Unix.process_status = WEXITED 1
    end)

