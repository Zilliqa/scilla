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
open OUnit2
open Snark

let alt_bn128_G1_add_io =
  [
    ("2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824",
     "486ea46224d1bb4fb680f34f7c9ad96a8f24ec88be73ea8e5a6c65260e9cb8a7",
     "936a185caaa266bb9cbe981e9e05cb78cd732b0b3280eb944412bb6f8f8f07af");
  ]

let alt_bn128_G1_add_tests = 
  "alt_bn128_G1_add_tests" >::: 
  List.map alt_bn128_G1_add_io ~f:(fun (p1, p2, pres) ->
    test_case (fun _ ->
      let pres' = alt_bn128_G1_add p1 p2 in
      assert_bool "alt_bn128_G1_add failed" (pres = pres')
    )
  )

let snark_tests _ = "snark_tests" >::: [alt_bn128_G1_add_tests]