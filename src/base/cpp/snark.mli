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

type scalar = string (* TODO: Replace this with something like ByStr32 *)

type g1point = { g1x : scalar; g1y : scalar }

type g2point = { g2x : string; g2y : string }

(* TODO: Replace with (ByStr64,ByStr64) *)

(* lengths of byte encoding of various types. *)
val scalar_len : int

val g1point_len : int (* length of a point in G1 *)

val g2comp_len : int (* length of each component of a point in G2 *)

val g2point_len : int (* length of a point in G2 *)

val g1g2pair_len : int (* length of each pair in alt_bn128_pairing_product *)

val alt_bn128_G1_add : g1point -> g1point -> g1point option

val alt_bn128_G1_mul : g1point -> scalar -> g1point option

val alt_bn128_pairing_product : (g1point * g2point) list -> bool option

val encode_g1point_bytes : g1point -> string

val decode_g1point_bytes : string -> g1point option

val encode_g2point_bytes : g2point -> string

val encode_g1g2pair_bytes : g1point -> g2point -> string

val eq_scalar : scalar -> scalar -> bool

val eq_g1 : g1point -> g1point -> bool

val eq_g2 : g2point -> g2point -> bool
