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
(** Ipcmessage.proto Types *)

(** {2 Types} *)

type proto_scilla_val_map = { m : (string * proto_scilla_val) list }
and proto_scilla_val = Bval of bytes | Mval of proto_scilla_val_map

type proto_scilla_query = {
  name : string;
  mapdepth : int;
  indices : bytes list;
  ignoreval : bool;
}

(** {2 Default values} *)

val default_proto_scilla_val_map :
  ?m:(string * proto_scilla_val) list -> unit -> proto_scilla_val_map
(** [default_proto_scilla_val_map ()] is the default value for type [proto_scilla_val_map] *)

val default_proto_scilla_val : unit -> proto_scilla_val
(** [default_proto_scilla_val ()] is the default value for type [proto_scilla_val] *)

val default_proto_scilla_query :
  ?name:string ->
  ?mapdepth:int ->
  ?indices:bytes list ->
  ?ignoreval:bool ->
  unit ->
  proto_scilla_query
(** [default_proto_scilla_query ()] is the default value for type [proto_scilla_query] *)
