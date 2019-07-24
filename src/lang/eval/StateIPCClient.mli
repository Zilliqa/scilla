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
open Syntax
open ErrorUtils
open ParserUtil.ParsedSyntax

(* Fetch a field value. keys is empty iff the value being fetched is not a whole map itself.
 * If a map key is not found, then None is returned, otherwise (Some value) is returned. *)
val fetch : socket_address:string -> fname:loc ident -> keys:(literal list) -> is_map:bool 
            -> ((literal option * stmt_eval_context), scilla_error list) result

(* Update a field. keys is empty iff the value being updated is not a whole map itself. 
Value can be None, which implies a remove operation *)
val update : socket_address:string -> fname:loc ident -> keys:(literal list) -> value:literal option 
             -> is_map:bool -> (stmt_eval_context, scilla_error list) result

val test_server_rpc : socket_address:string -> query:string -> (string, Idl.DefaultError.t) result