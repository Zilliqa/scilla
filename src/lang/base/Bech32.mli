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

(* The string length of a valid bech32 address.  *)
val bech32_addr_len : string -> int
(* Decodes a bech32 address string to a string of 20 bytes.
 * Signature: prefix -> bech32_addr -> bystr20 address option. *)
val decode_bech32_addr : string -> string -> string option
(* Is a given string a valid bech32 address. *)
val is_valid_bech32 : string -> string -> bool
(* Encodes a 20-byte string into the bech32 address format.
 * Signature: prefix -> bystr20 address -> bech32_addr option. *)
val encode_bech32_addr : string -> string -> string option
