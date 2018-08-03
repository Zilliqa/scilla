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

(* Available debug levels for functions in DebugMessage *)
type debug_kind =
  | Debug_None
  | Debug_Normal
  | Debug_Verbose

val get_debug_level : unit -> debug_kind
val set_debug_level : debug_kind -> unit
val get_log_file : unit -> string
val set_log_file : string -> unit

(* Available trace levels *)
type trace_kind =
  | Trace_None
  | Trace_Statement
  | Trace_Expression

val get_trace_level : unit -> trace_kind
val set_trace_level : trace_kind -> unit
val get_trace_file : unit -> string
val set_trace_file : string -> unit

(* Environment variable to look for stdlib *)
val scilla_stdlib_path : string
