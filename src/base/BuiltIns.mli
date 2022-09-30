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

open Syntax
open ErrorUtils
open Core
open Literal
module BILiteral = GlobalLiteral
module BIType = BILiteral.LType
module BIIdentifier = BIType.TIdentifier
module BIName = BIIdentifier.Name

module ScillaBuiltIns (SR : Rep) (ER : Rep) : sig
  val bstring_from_int_lit : BILiteral.int_lit -> string
  val bstring_from_uint_lit : BILiteral.uint_lit -> string

  type elaborator =
    BIType.t ->
    (* builtin type *)
    BIType.t list ->
    (* type arguments *)
    BIType.t list ->
    (* types of value arguments *)
    (BIType.t, scilla_error list) result

  module StringBuiltins : sig
    val eq_arity : int
    val eq_type : BIType.t
    val concat_arity : int
    val concat_type : BIType.t
    val substr_arity : int
    val substr_type : BIType.t
    val strlen_arity : int
    val strlen_type : BIType.t
    val strlen_elab : elaborator
    val to_string_arity : int
    val to_string_type : BIType.t
    val to_string_elab : elaborator
    val to_ascii_arity : int
    val to_ascii_type : BIType.t
    val to_ascii_elab : elaborator
    val strrev_arity : int
    val strrev_type : BIType.t
    val strrev_elab : elaborator
  end

  module BNumBuiltins : sig
    val eq_arity : int
    val eq_type : BIType.t
    val blt_arity : int
    val blt_type : BIType.t
    val badd_arity : int
    val badd_type : BIType.t
    val badd_elab : elaborator
    val bsub_arity : int
    val bsub_type : BIType.t
    val bsub_elab : elaborator
  end

  module CryptoBuiltins : sig
    val eq_arity : int
    val eq_type : BIType.t
    val eq_elab : elaborator
    val hash_arity : int
    val hash_type : BIType.t
    val hash_elab : elaborator
    val ripemd160hash_type : BIType.t
    val to_bystr_arity : int
    val to_bystr_type : BIType.t
    val to_bystr_elab : elaborator
    val to_bystrx_arity : int
    val to_bystrx_type : int -> BIType.t
    val substr_arity : int
    val substr_type : BIType.t
    val to_uint_arity : int
    val to_uint_type : Type.PrimType.int_bit_width -> BIType.t
    val to_uint_elab : Type.PrimType.int_bit_width -> elaborator
    val bech32_to_bystr20_arity : int
    val bech32_to_bystr20_type : BIType.t
    val bystr20_to_bech32_arity : int
    val bystr20_to_bech32_type : BIType.t
    val concat_arity : int
    val concat_type : BIType.t
    val concat_elab : elaborator
    val ec_gen_key_pair_arity : int
    val ec_gen_key_pair_type : BIType.t
    val schnorr_sign_arity : int
    val schnorr_sign_type : BIType.t
    val schnorr_verify_arity : int
    val schnorr_verify_type : BIType.t
    val ecdsa_sign_arity : int
    val ecdsa_sign_type : BIType.t
    val ecdsa_verify_arity : int
    val ecdsa_verify_type : BIType.t
    val ecdsa_recover_pk_arity : int
    val ecdsa_recover_pk_type : BIType.t
    val schnorr_get_address_arity : int
    val schnorr_get_address_type : BIType.t
    val alt_bn128_G1_add_arity : int
    val alt_bn128_G1_add_type : BIType.t
    val alt_bn128_G1_mul_arity : int
    val alt_bn128_G1_mul_type : BIType.t
    val alt_bn128_G1_neg_arity : int
    val alt_bn128_G1_neg_type : BIType.t
    val alt_bn128_pairing_product_arity : int
    val alt_bn128_pairing_product_type : BIType.t
  end

  module IntBuiltins : sig
    val eq_arity : int
    val eq_type : BIType.t
    val eq_elab : elaborator
    val binop_arity : int
    val binop_type : BIType.t
    val binop_elab : elaborator
    val pow_arity : int
    val pow_type : BIType.t
    val pow_elab : elaborator
    val to_int_arity : int
    val to_int_type : BIType.t
    val to_int_elab : Type.PrimType.int_bit_width -> elaborator
  end

  module UintBuiltins : sig
    val eq_arity : int
    val eq_type : BIType.t
    val eq_elab : elaborator
    val binop_arity : int
    val binop_type : BIType.t
    val binop_elab : elaborator
    val pow_arity : int
    val pow_type : BIType.t
    val pow_elab : elaborator
    val isqrt_arity : int
    val isqrt_type : BIType.t
    val isqrt_elab : elaborator
    val to_uint_arity : int
    val to_uint_type : BIType.t
    val to_uint_elab : Type.PrimType.int_bit_width -> elaborator
    val to_nat_arity : int
    val to_nat_type : BIType.t
    val to_bystrx_arity : int
    val to_bystrx_type : BIType.t
    val to_bystrx_elab : int -> elaborator
  end

  module MapBuiltins : sig
    val contains_arity : int
    val contains_type : BIType.t
    val contains_elab : elaborator
    val put_arity : int
    val put_type : BIType.t
    val put_elab : elaborator
    val get_arity : int
    val get_type : BIType.t
    val get_elab : elaborator
    val remove_arity : int
    val remove_type : BIType.t
    val remove_elab : elaborator
    val to_list_arity : int
    val to_list_type : BIType.t
    val to_list_elab : elaborator
    val size_arity : int
    val size_type : BIType.t
    val size_elab : elaborator
  end

  module BuiltInDictionary : sig
    (* Returns the result type for given argument types, e.g., Bool *)
    val find_builtin_op :
      ER.rep builtin_annot ->
      targtypes:BIType.t list ->
      (* type arguments *)
      vargtypes:BIType.t list ->
      (* types of value arguments *)
      (BIType.t, scilla_error list) result
  end

  (* Elaborator for the built-in typ *)
  val elab_id :
    BIType.t ->
    (* builtin type *)
    BIType.t list ->
    (* type arguments *)
    BIType.t list ->
    (* types of value arguments *)
    (BIType.t, scilla_error list) result
end
