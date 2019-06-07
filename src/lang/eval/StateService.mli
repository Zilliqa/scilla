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

(* This file describes function that communicate with the blockchain to fetch
 * and update state variables on demand. *)

open Syntax
open ErrorUtils
open ParserUtil.ParsedSyntax

type ss_field =
  {
    fname : string;
    ftyp : typ;
    fval : literal option; (* Value may not be available (in IPC mode) *)
  }
type service_mode = 
  | IPC of int (* port number for IPC *)
  | Local

(* Sets up the state service object. Should be called before any queries. *)
val initialize : sm:service_mode -> fields:ss_field list-> unit
(* Expensive operation, use with care. *)
val get_full_state : unit -> ((string * literal) list, scilla_error list) result
(* Finalize: no more queries. *)
val finalize : unit -> (unit, scilla_error list) result

(* Fetch a field value. keys is empty iff the value being fetched is not a whole map itself.
 * If a map key is not found, then None is returned, otherwise (Some value) is returned. *)
val fetch : fname:loc ident -> keys:(literal list) -> ((literal option * stmt_eval_context), scilla_error list) result
(* Update a field. keys is empty iff the value being updated is not a whole map itself. *)
val update : fname:loc ident -> keys:(literal list) -> value:literal -> (stmt_eval_context, scilla_error list) result
(* Is a key in a map. keys must be non-empty. *)
val is_member : fname:loc ident -> keys:(literal list) -> ((bool * stmt_eval_context), scilla_error list) result
(* Remove a key from a map. keys must be non-empty. *)
val remove : fname:loc ident -> keys:(literal list) -> (stmt_eval_context, scilla_error list) result
