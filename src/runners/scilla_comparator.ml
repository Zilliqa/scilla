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
open! Int.Replace_polymorphic_compare
open Syntax
open Result.Let_syntax

let () =
  let file1 = Sys.argv.(1) in
  let file2 = Sys.argv.(2) in
  let open ParsedSyntax in
  let res =
    let%bind (cmod1 : cmodule) = FrontEndParser.parse_cmodule file1 in
    let%bind (cmod2 : cmodule) = FrontEndParser.parse_cmodule file2 in
    return @@ [%equal: ParsedSyntax.cmodule] cmod1 cmod2
  in
  match res with Error _ -> exit 1 | Ok true -> exit 0 | Ok false -> exit 2
