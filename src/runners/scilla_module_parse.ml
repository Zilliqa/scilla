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



open Printf
open Sexplib.Std
open Syntax
open Core

let () =
  let filename = Sys.argv.(1) in
  match FrontEndParser.parse_file ScillaParser.cmodule filename with
  | Some cs ->
      printf "%s \n" (sexp_of_cmodule sexp_of_loc cs |> Sexplib.Sexp.to_string)
  | None -> ()

