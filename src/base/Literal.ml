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
open Identifier
open Type

module type ScillaLiteral = sig
  module LType : ScillaType

  type mtype = LType.t * LType.t [@@deriving sexp]

  open Integer256

  type int_lit =
    | Int32L of int32
    | Int64L of int64
    | Int128L of int128
    | Int256L of int256
  [@@deriving equal, sexp]

  val equal_uint32 : uint32 -> uint32 -> bool

  type uint_lit =
    | Uint32L of uint32
    | Uint64L of uint64
    | Uint128L of uint128
    | Uint256L of uint256
  [@@deriving equal, sexp]

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
    | ADTValue of string * LType.t list * t list
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
        (LType.t ->
        ( t,
          scilla_error list,
          uint64 ->
          ((t * (string * t) list) * uint64, scilla_error list * uint64) result
        )
        CPSMonad.t)
  [@@deriving sexp]

  val subst_type_in_literal : 'a LType.TIdentifier.t -> LType.t -> t -> t

  (****************************************************************)
  (*            PrimType Literal utilities                        *)
  (****************************************************************)

  val build_prim_literal : LType.prim_typ -> string -> t option

  (* Is string representation of integer valid for integer typ. *)
  val validate_int_string : LType.prim_typ -> string -> bool

  (* Get bit-width if int_lit. *)
  val int_lit_width : int_lit -> int

  (* Get bit-width if uint_lit. *)
  val uint_lit_width : uint_lit -> int

  (* String conversion from int_typ *)
  val string_of_int_lit : int_lit -> string

  (* String conversion from uint_typ *)
  val string_of_uint_lit : uint_lit -> string
end

module MkLiteral (T : ScillaType) = struct
  module LType = T
  open LType

  (*******************************************************)
  (*                      Literals                       *)
  (*******************************************************)

  (* The first component is a primitive type *)
  type mtype = LType.t * LType.t [@@deriving sexp]

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
    | ADTValue of string * LType.t list * t list
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
        (LType.t ->
        ( t,
          scilla_error list,
          uint64 ->
          ((t * (string * t) list) * uint64, scilla_error list * uint64) result
        )
        CPSMonad.t)
  [@@deriving sexp]

  (****************************************************************)
  (*                     Type substitutions                       *)
  (****************************************************************)

  let rec subst_type_in_literal tvar tp l =
    match l with
    | Map ((kt, vt), ls) ->
        let kts = subst_type_in_type' tvar tp kt in
        let vts = subst_type_in_type' tvar tp vt in
        let ls' = Hashtbl.create (Hashtbl.length ls) in
        let _ =
          Hashtbl.iter
            (fun k v ->
              let k' = subst_type_in_literal tvar tp k in
              let v' = subst_type_in_literal tvar tp v in
              Hashtbl.add ls' k' v')
            ls
        in
        Map ((kts, vts), ls')
    | ADTValue (n, ts, ls) ->
        let ts' = List.map ts ~f:(subst_type_in_type' tvar tp) in
        let ls' = List.map ls ~f:(subst_type_in_literal tvar tp) in
        ADTValue (n, ts', ls')
    | _ -> l

  (****************************************************************)
  (*                 Primitive Literal Utilities                  *)
  (****************************************************************)

  (* Is string representation of integer valid for integer typ. *)
  let validate_int_string pt x =
    let open String in
    try
      match pt with
      | Int_typ Bits32 -> Int32.to_string (Int32.of_string x) = x
      | Int_typ Bits64 -> Int64.to_string (Int64.of_string x) = x
      | Int_typ Bits128 -> Int128.to_string (Int128.of_string x) = x
      | Int_typ Bits256 -> Int256.to_string (Int256.of_string x) = x
      | Uint_typ Bits32 -> Uint32.to_string (Uint32.of_string x) = x
      | Uint_typ Bits64 -> Uint64.to_string (Uint64.of_string x) = x
      | Uint_typ Bits128 -> Uint128.to_string (Uint128.of_string x) = x
      | Uint_typ Bits256 -> Uint256.to_string (Uint256.of_string x) = x
      | _ -> false
    with _ -> false

  (* Given an integer type and the value (as string),
     build IntLit or UintLit out of it. *)
  let build_int pt v =
    let validator_wrapper l = Option.some_if (validate_int_string pt v) l in
    try
      match pt with
      | Int_typ Bits32 ->
          validator_wrapper (IntLit (Int32L (Int32.of_string v)))
      | Int_typ Bits64 ->
          validator_wrapper (IntLit (Int64L (Int64.of_string v)))
      | Int_typ Bits128 ->
          validator_wrapper (IntLit (Int128L (Stdint.Int128.of_string v)))
      | Int_typ Bits256 ->
          validator_wrapper (IntLit (Int256L (Int256.of_string v)))
      | Uint_typ Bits32 ->
          validator_wrapper (UintLit (Uint32L (Stdint.Uint32.of_string v)))
      | Uint_typ Bits64 ->
          validator_wrapper (UintLit (Uint64L (Stdint.Uint64.of_string v)))
      | Uint_typ Bits128 ->
          validator_wrapper (UintLit (Uint128L (Stdint.Uint128.of_string v)))
      | Uint_typ Bits256 ->
          validator_wrapper (UintLit (Uint256L (Uint256.of_string v)))
      | _ -> None
    with _ -> None

  let int_lit_width = function
    | Int32L _ -> 32
    | Int64L _ -> 64
    | Int128L _ -> 128
    | Int256L _ -> 256

  let string_of_int_lit = function
    | Int32L i' -> Int32.to_string i'
    | Int64L i' -> Int64.to_string i'
    | Int128L i' -> Int128.to_string i'
    | Int256L i' -> Int256.to_string i'

  let uint_lit_width = function
    | Uint32L _ -> 32
    | Uint64L _ -> 64
    | Uint128L _ -> 128
    | Uint256L _ -> 256

  let string_of_uint_lit = function
    | Uint32L i' -> Uint32.to_string i'
    | Uint64L i' -> Uint64.to_string i'
    | Uint128L i' -> Uint128.to_string i'
    | Uint256L i' -> Uint256.to_string i'

  (* Scilla only allows printable ASCII characters from 32 (0x20) up-to 126 (0x7e).
   * The only exceptions allowed are various whitespaces: \b \f \n \r \t. *)
  let validate_string_literal s =
    let specials = [ '\b'; '\012'; '\n'; '\r'; '\t' ] in
    String.fold s ~init:true ~f:(fun safe c ->
        if not safe then false
        else
          let c_int = Char.to_int c in
          if c_int >= 32 && c_int <= 126 then true
          else if List.mem specials c ~equal:Char.( = ) then true
          else false)

  let build_prim_literal pt v =
    match pt with
    | Int_typ _ | Uint_typ _ -> build_int pt v
    | String_typ -> Option.some_if (validate_string_literal v) (StringLit v)
    | Bnum_typ ->
        Option.some_if
          (let re = Str.regexp "[0-9]+$" in
           Str.string_match re v 0)
          (BNum v)
    | Bystr_typ -> Some (ByStr (Bystr.parse_hex v))
    | Bystrx_typ _ -> Some (ByStrX (Bystrx.parse_hex v))
    | _ -> None
end

module FlattenedLiteral = MkLiteral (MkType (MkIdentifier (FlattenedName)))
