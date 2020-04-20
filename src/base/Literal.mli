(*
  This file is part of scilla.

  Copyright (c) 2020 - present Zilliqa Research Pvt. Ltd.
  
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

open Stdint
open MonadUtil
open ErrorUtils

type mtype = Type.t * Type.t [@@deriving sexp]

open Integer256

type int_lit =
  | Int32L of int32
  | Int64L of int64
  | Int128L of int128
  | Int256L of int256
[@@deriving equal]

val equal_uint32 : uint32 -> uint32 -> bool

type uint_lit =
  | Uint32L of uint32
  | Uint64L of uint64
  | Uint128L of uint128
  | Uint256L of uint256
[@@deriving equal]

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

module Bystr : BYSTR

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

module Bystrx : BYSTRX

type t =
  | StringLit of string
  (* Cannot have different integer literals here directly as Stdint does not derive sexp. *)
  | IntLit of int_lit
  | UintLit of uint_lit
  | BNum of string
  (* Byte string with a statically known length. *)
  | ByStrX of Bystrx.t
  (* Byte string without a statically known length. *)
  | ByStr of Bystr.t
  (* Message: an associative array *)
  | Msg of (string * t) list
  (* A dynamic map of literals *)
  | Map of mtype * (t, t) Hashtbl.t
  (* A constructor in HNF *)
  | ADTValue of string * Type.t list * t list
  (* An embedded closure *)
  | Clo of
      (t ->
      ( t,
        scilla_error list,
        uint64 ->
        ((t * (string * t) list) * uint64, scilla_error list * uint64) result
      )
      CPSMonad.t)
  (* A type abstraction *)
  | TAbs of
      (Type.t ->
      ( t,
        scilla_error list,
        uint64 ->
        ((t * (string * t) list) * uint64, scilla_error list * uint64) result
      )
      CPSMonad.t)
[@@deriving sexp]

val subst_type_in_literal : 'a Identifier.t -> Type.t -> t -> t

(****************************************************************)
(*            PrimType Literal utilities                        *)
(****************************************************************)

val build_prim_literal : Type.prim_typ -> string -> t option

(* Is string representation of integer valid for integer typ. *)
val validate_int_string : Type.prim_typ -> string -> bool

(* Get bit-width if int_lit. *)
val int_lit_width : int_lit -> int

(* Get bit-width if uint_lit. *)
val uint_lit_width : uint_lit -> int

(* String conversion from int_typ *)
val string_of_int_lit : int_lit -> string

(* String conversion from uint_typ *)
val string_of_uint_lit : uint_lit -> string
