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
let copy_to_cptr p s =
  let f i c = p +@ i <-@ c in
  String.iteri f s

(* Copy the contents of "temporary" string s
 * to a new string. "temporary" strings are those
 * whose underlying memory can be freed "anytime".
 * This arises when the string is created from `string_from_ptr`.
 *)
let copy_from_tstring s =
  let f i = s.[i] in
  String.init (String.length s) f

(*
 *  typedef struct
 *  {
 *    char* data;
 *    int len;
 *  } RawBytes_Z;
 *)
type rawBytes_Z

let rawBytes_Z : rawBytes_Z structure typ = structure "rawBytes_Z"

let rawBytes_data = field rawBytes_Z "data" (ptr char)

let rawBytes_len = field rawBytes_Z "len" int

let () = seal rawBytes_Z
