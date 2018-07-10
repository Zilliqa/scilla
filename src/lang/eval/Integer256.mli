(*
 * Copyright (c) 2018 - present Zilliqa, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
  val abs: t -> t
  val compare : t -> t -> int
  val shift_left : t -> int -> t
  val shift_right : t -> int -> t
  val shift_right_logical : t -> int -> t
  val logand : t -> t -> t
  val logor : t -> t -> t
  val logxor : t -> t -> t
  val lognot : t -> t

  val of_string : string -> t
  val to_string : t -> string
end
