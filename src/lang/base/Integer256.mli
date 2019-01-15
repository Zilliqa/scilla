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

(* Extends Stdint's uint128 to uint256. *)
module Uint256 : sig
  type t

  val zero : t
  val one : t
  val max_int : t
  val min_int : t

  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div: t -> t -> t
  val rem: t -> t -> t
  val compare : t -> t -> int
  val shift_left : t -> int -> t
  val shift_right : t -> int -> t
  val shift_right_logical : t -> int -> t
  val setbit : t -> int -> t
  val clearbit : t -> int -> t
  val logand : t -> t -> t
  val logor : t -> t -> t
  val logxor : t -> t -> t
  val lognot : t -> t

  val abs: t -> t
  val neg: t -> t

  val of_string : string -> t
  val to_string : t -> string
  val to_bytes_big_endian : t -> Bytes.t -> int -> unit
  val to_bytes_little_endian : t -> Bytes.t -> int -> unit
  val of_bytes_big_endian : Bytes.t -> int -> t
  val of_bytes_little_endian : Bytes.t -> int -> t

end

module Int256 : sig
  type t

  val zero : t
  val one : t
  val max_int : t
  val min_int : t

  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div: t -> t -> t
  val rem: t -> t -> t
  val compare : t -> t -> int
  val shift_left : t -> int -> t
  val shift_right : t -> int -> t
  val shift_right_logical : t -> int -> t
  val setbit : t -> int -> t
  val clearbit : t -> int -> t
  val logand : t -> t -> t
  val logor : t -> t -> t
  val logxor : t -> t -> t
  val lognot : t -> t

  val abs: t -> t
  val neg: t -> t

  val of_string : string -> t
  val to_string : t -> string
  val to_bytes_big_endian : t -> Bytes.t -> int -> unit
  val to_bytes_little_endian : t -> Bytes.t -> int -> unit
  val of_bytes_big_endian : Bytes.t -> int -> t
  val of_bytes_little_endian : Bytes.t -> int -> t

end

type int256 = Int256.t
type uint256 = Uint256.t
