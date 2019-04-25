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

val privkey_len : int
val pubkey_len : int
val signature_len : int

(* Generate a private key / public key pair.
 * The output is a raw string pair representing
 * privkey_len / pubkey_len bytes of the keys
 *)
val genKeyPair : unit -> string * string

(* Given private key, public key and message,
 * sign the message and return the signature.
 *)
val sign : string -> string -> string -> string

(* Given public key, message and a signature, verify
 * that the message was indeed signed by the public key.
 *)
val verify : string -> string -> string -> bool

