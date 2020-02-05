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
open GlobalConfig

(* Prints to log file *)
let plog msg =
  match get_debug_level () with
  | Debug_Normal | Debug_Verbose ->
      let fname = get_log_file () in
      Out_channel.with_file fname ~append:true ~f:(fun h ->
          Out_channel.output_string h msg)
  | Debug_None -> ()

(* Prints to stdout and log file *)
let pout msg =
  Out_channel.output_string Out_channel.stdout msg;
  plog ("stdout: " ^ msg ^ "\n")

(* Prints to stderr and log file *)
let perr msg =
  Out_channel.output_string Out_channel.stderr msg;
  plog ("stderr: " ^ msg ^ "\n")

(* Prints to trace file, if set, else to stdout. *)
let ptrace msg =
  let fname = GlobalConfig.get_trace_file () in
  if String.(fname <> "") then
    Out_channel.with_file fname ~append:true ~f:(fun h ->
        Out_channel.output_string h msg)
  else Out_channel.output_string Out_channel.stdout msg
