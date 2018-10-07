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




module Tests = TestUtil.DiffBasedTests(
  struct
    let gold_path dir f = [dir; "checker"; "good"; "gold"; f ^ ".gold" ]
    let test_path f = ["checker"; "good"; f]
    let runner = "scilla-checker"
    let tests = [
      "crowdfunding.scilla"; "zil-game.scilla"; "fungible-token.scilla"; "auction.scilla";
      "empty.scilla"; "schnorr.scilla";
      "one_accept.scilla"
    ]
    let use_stdlib = true
  end)
