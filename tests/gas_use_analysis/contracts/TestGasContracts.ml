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

let explist = [
  "auction.scilla";
  "bookstore.scilla";
  "crowdfunding.scilla";
  "zil-game.scilla";
  "fungible-token.scilla";
  "nonfungible-token.scilla";
  "schnorr.scilla";
  "helloworld.scilla";
  "wallet.scilla";
]

module Tests = TestUtil.DiffBasedTests(
  struct
    let gold_path dir f = [dir; "gas_use_analysis"; "contracts"; "gold"; f ^ ".gold" ]
    let test_path f = ["gas_use_analysis"; "contracts"; f]
    let runner = "scilla-checker"
    let custom_args = ["-gua"; "-contractinfo"]
    let additional_libdirs = []
    let tests = explist
    let exit_code : Unix.process_status = WEXITED 0

  end)

