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

open Scilla_base
open ErrorUtils
open Literal
module IPCCLiteral = GlobalLiteral
module IPCCType = IPCCLiteral.LType
module IPCCIdentifier = IPCCType.TIdentifier

(* Fetch from a field. "keys" is empty when fetching non-map fields or an entire Map field.
 * If a map key is not found, then None is returned, otherwise (Some value) is returned. *)
val fetch :
  socket_addr:string ->
  fname:loc IPCCIdentifier.t ->
  keys:IPCCLiteral.t list ->
  tp:IPCCType.t ->
  (IPCCLiteral.t option, scilla_error list) result

(* Fetch from another contract's field. "keys" is empty when fetching non-map fields
 * or an entire Map field. If a map key is not found, then None is returned, otherwise
 * (Some value) is returned. *)
val external_fetch :
  socket_addr:string ->
  caddr:string ->
  fname:'a IPCCIdentifier.t ->
  keys:IPCCLiteral.t list ->
  ignoreval:bool ->
  (IPCCLiteral.t option * IPCCType.t option, scilla_error list) result

(* Update a field. "keys" is empty when updating non-map fields or an entire Map field. *)
val update :
  socket_addr:string ->
  fname:loc IPCCIdentifier.t ->
  keys:IPCCLiteral.t list ->
  value:IPCCLiteral.t ->
  tp:IPCCType.t ->
  (unit, scilla_error list) result

(* Is a key in a map. keys must be non-empty. *)
val is_member :
  socket_addr:string ->
  fname:loc IPCCIdentifier.t ->
  keys:IPCCLiteral.t list ->
  tp:IPCCType.t ->
  (bool, scilla_error list) result

(* Remove a key from a map. keys must be non-empty. *)
val remove :
  socket_addr:string ->
  fname:loc IPCCIdentifier.t ->
  keys:IPCCLiteral.t list ->
  tp:IPCCType.t ->
  (unit, scilla_error list) result

(* Fetch blockchain info. The semantics and format of 
 * ~query_args and the result depends on ~query_name 
 * Any error on the blockchain side or IPC is forwarded
 * (via the error monad). *)
val fetch_bcinfo :
  socket_addr:string ->
  query_name:string ->
  query_args:string ->
  (string, scilla_error list) result
