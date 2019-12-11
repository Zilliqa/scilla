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

open Ctypes

(* Copy the contents of string s to pointer p.
 * This does the opposite of `string_from_ptr`.
 *)
val copy_to_cptr : (char, [`C]) pointer -> string -> unit

(* Copy the contents of "temporary" string s
 * to a new string. "temporary" strings are those
 * whose underlying memory can be freed "anytime".
 * This arises when the string is created from `string_from_ptr`.
 *)
val copy_from_tstring : string -> string

(*
 *  typedef struct
 *  {
 *    char* data;
 *    int len;
 *  } RawBytes_Z;
 *)
type rawBytes_Z
val rawBytes_Z : rawBytes_Z structure typ
val rawBytes_data : (char Ctypes_static.ptr, rawBytes_Z structure) field
val rawBytes_len : (int, rawBytes_Z structure) field
