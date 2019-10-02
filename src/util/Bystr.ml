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


open Core
open Sexplib.Std

module type BYSTR = sig
  type t [@@deriving sexp]
  val width : t -> int
  val parse_hex : string -> t
  val hex_encoding : t -> string
  val to_raw_bytes : t -> string
  val of_raw_bytes : int -> string -> t option
  val equal : t -> t -> bool
  val concat : t -> t -> t
end

module Bystr : BYSTR = struct
  type t = string [@@deriving sexp]

  let width = String.length

  let parse_hex s =
    if not (String.equal (String.prefix s 2) "0x") then
      raise @@ Invalid_argument "hex conversion: 0x prefix is missing"
    else
      let s_nopref = String.drop_prefix s 2 in
      if String.length s_nopref = 0 then
        raise @@ Invalid_argument "hex conversion: empty byte sequence"
      else
        Hex.to_string (`Hex s_nopref)

  let hex_encoding bs = "0x" ^ Hex.show @@ Hex.of_string bs

  let to_raw_bytes = Fn.id

  let of_raw_bytes expected_width raw =
    Option.some_if (String.length raw = expected_width) raw

  let equal = String.equal

  let concat = (^)
end

module type BYSTRX = sig
  type t [@@deriving sexp]
  val width : t -> int
  val parse_hex : string -> t
  val hex_encoding : t -> string
  val to_raw_bytes : t -> string
  val of_raw_bytes : int -> string -> t option
  val equal : t -> t -> bool
  val concat : t -> t -> t
  val to_bystr : t -> Bystr.t
end

module Bystrx : BYSTRX = struct
  include Bystr
  let to_bystr = Fn.id
end