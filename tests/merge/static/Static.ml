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

open Core

module ContractTests = Scilla_test.Util.DiffBasedMultiTests (struct
  let gold_path dir f = [ dir; "merge"; "static"; "gold"; f ^ ".gold" ]
  let test_path f = [ "merge"; "static"; f ]
  let runner = "scilla-merger"
  let ignore_predef_args = false
  let gas_limit = Stdint.Uint64.of_int 8000
  let custom_args = [ "--config"; "merge/static/merge_config.json" ]
  let additional_libdirs = []
  let provide_init_arg = false
  let diff_filter s = s

  let tests =
    [
      [ "simple11.scilla"; "simple12.scilla" ];
      [ "collisions11.scilla"; "collisions12.scilla" ];
      [ "same_contract_name11.scilla"; "same_contract_name12.scilla" ];
      [ "constraints11.scilla"; "constraints12.scilla"; "constraints13.scilla" ];
      [
        "remote_collisions11.scilla";
        "remote_collisions12.scilla";
        "remote_collisions13.scilla";
      ];
      [
        "remote_collisions21.scilla";
        "remote_collisions22.scilla";
        "remote_collisions23.scilla";
      ];
      [ "type_casts1.scilla" ];
    ]

  let exit_code : UnixLabels.process_status = WEXITED 0
end)

module JSONTests = Scilla_test.Util.DiffBasedMergeJSONTests (struct
  let gold_path dir f = [ dir; "merge"; "static"; "gold"; f ^ ".gold" ]
  let test_path f = [ "merge"; "static"; f ]
  let runner = "scilla-merger"
  let ignore_predef_args = false
  let gas_limit = Stdint.Uint64.of_int 8000
  let custom_args = [ "--config"; "merge/static/merge_config.json" ]
  let additional_libdirs = []
  let diff_filter s = s

  let tests =
    let open Scilla_test.Util in
    [
      {
        json_test_name = "ark_zil_game.json";
        json_tests =
          [ ("ark_init.json", "ark"); ("zil_game_init.json", "zil_game") ];
      };
    ]

  let exit_code : UnixLabels.process_status = WEXITED 0
end)
