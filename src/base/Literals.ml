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

(* [Specialising the Return Type of Closures]

   The syntax for literals implements a _shallow embedding_ of
   closures and type abstractions (cf. constructors `Clo` and `TAbs`).
   Since our computations are all in CPS (cf. [Evaluation in CPS]), so
   should be the computations, encapsulated by those two forms.
   However, for the time being, we want to keep the type `literal`
   non-parametric. This is at odds with the priniciple of keeping
   computations in CPS parametric in their result type.

   Therefore, for now we have a compromise of fixing the result of
   evaluating expressions to be as below. In order to restore the
   genericity enabled by CPS, we provide an "impedance matcher",
   described in [Continuation for Expression Evaluation]. 

*)

open Core_kernel
open! Int.Replace_polymorphic_compare
open Sexplib.Std
open Stdint
open MonadUtil
open ErrorUtils
open Types

(*******************************************************)
(*                      Literals                       *)
(*******************************************************)

(* The first component is a primitive type *)
type mtype = typ * typ [@@deriving sexp]

open Integer256

let equal_int128 x y = Int128.compare x y = 0

let equal_int256 x y = Int256.compare x y = 0

type int_lit =
  | Int32L of int32
  | Int64L of int64
  | Int128L of int128
  | Int256L of int256
[@@deriving equal]

let sexp_of_int_lit = function
  | Int32L i' -> Sexp.Atom ("Int32 " ^ Int32.to_string i')
  | Int64L i' -> Sexp.Atom ("Int64 " ^ Int64.to_string i')
  | Int128L i' -> Sexp.Atom ("Int128 " ^ Int128.to_string i')
  | Int256L i' -> Sexp.Atom ("Int256 " ^ Int256.to_string i')

let int_lit_of_sexp _ = failwith "int_lit_of_sexp is not implemented"

let equal_uint32 x y = Uint32.compare x y = 0

let equal_uint64 x y = Uint64.compare x y = 0

let equal_uint128 x y = Uint128.compare x y = 0

let equal_uint256 x y = Uint256.compare x y = 0

type uint_lit =
  | Uint32L of uint32
  | Uint64L of uint64
  | Uint128L of uint128
  | Uint256L of uint256
[@@deriving equal]

let sexp_of_uint_lit = function
  | Uint32L i' -> Sexp.Atom ("Uint32 " ^ Uint32.to_string i')
  | Uint64L i' -> Sexp.Atom ("Uint64 " ^ Uint64.to_string i')
  | Uint128L i' -> Sexp.Atom ("Uint128 " ^ Uint128.to_string i')
  | Uint256L i' -> Sexp.Atom ("Uint256 " ^ Integer256.Uint256.to_string i')

let uint_lit_of_sexp _ = failwith "uint_lit_of_sexp is not implemented"

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
      else Hex.to_string (`Hex s_nopref)

  let hex_encoding bs = "0x" ^ Hex.show @@ Hex.of_string bs

  let to_raw_bytes = Fn.id

  let of_raw_bytes expected_width raw =
    Option.some_if (String.length raw = expected_width) raw

  let equal = String.equal

  let concat = ( ^ )
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

type literal =
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
  | Msg of (string * literal) list
  (* A dynamic map of literals *)
  | Map of mtype * (literal, literal) Hashtbl.t
  (* A constructor in HNF *)
  | ADTValue of string * typ list * literal list
  (* An embedded closure *)
  | Clo of
      (literal ->
      ( literal,
        scilla_error list,
        uint64 ->
        ( (literal * (string * literal) list) * uint64,
          scilla_error list * uint64 )
        result )
      CPSMonad.t)
  (* A type abstraction *)
  | TAbs of
      (typ ->
      ( literal,
        scilla_error list,
        uint64 ->
        ( (literal * (string * literal) list) * uint64,
          scilla_error list * uint64 )
        result )
      CPSMonad.t)
[@@deriving sexp]

