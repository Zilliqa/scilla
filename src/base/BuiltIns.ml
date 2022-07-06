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
open Literal
open Syntax
open ErrorUtils
open MonadUtil
open Result.Let_syntax
open Stdint
open TypeUtil
open Integer256
open TypeUtilities
module BILiteral = GlobalLiteral
module BIType = BILiteral.LType
module BIIdentifier = BIType.TIdentifier
module BIName = BIIdentifier.Name
open BIType
open BILiteral

module ScillaBuiltIns (SR : Rep) (ER : Rep) = struct
  let print_literal_list ls = PrettyPrinters.pp_literal_list ls

  let builtin_fail name ls =
    fail0
      ~kind:(sprintf "Cannot apply built-in %s to a list of arguments" name)
      ~inst:(print_literal_list ls)

  (* Convert int_lit to raw byte string. *)
  let bstring_from_int_lit = function
    | Int32L i ->
        let buf = Bytes.create 4 in
        let _ = Int32.to_bytes_big_endian i buf 0 in
        Bytes.to_string buf
    | Int64L i ->
        let buf = Bytes.create 8 in
        let _ = Int64.to_bytes_big_endian i buf 0 in
        Bytes.to_string buf
    | Int128L i ->
        let buf = Bytes.create 16 in
        let _ = Int128.to_bytes_big_endian i buf 0 in
        Bytes.to_string buf
    | Int256L i ->
        let buf = Bytes.create 32 in
        let _ = Int256.to_bytes_big_endian i buf 0 in
        Bytes.to_string buf

  (* Convert uint_lit to raw byte string. *)
  let bstring_from_uint_lit = function
    | Uint32L ui ->
        let buf = Bytes.create 4 in
        let _ = Uint32.to_bytes_big_endian ui buf 0 in
        Bytes.to_string buf
    | Uint64L ui ->
        let buf = Bytes.create 8 in
        let _ = Uint64.to_bytes_big_endian ui buf 0 in
        Bytes.to_string buf
    | Uint128L ui ->
        let buf = Bytes.create 16 in
        let _ = Uint128.to_bytes_big_endian ui buf 0 in
        Bytes.to_string buf
    | Uint256L ui ->
        let buf = Bytes.create 32 in
        let _ = Uint256.to_bytes_big_endian ui buf 0 in
        Bytes.to_string buf

  (* Elaborates the operation type based on the arguments types *)
  type elaborator =
    BIType.t ->
    (* builtin type *)
    BIType.t list ->
    (* type arguments *)
    BIType.t list ->
    (* types of value arguments *)
    (BIType.t, scilla_error list) result

  (*******************************************************)
  (**************** String *******************************)
  (*******************************************************)

  (* String operations *)
  module StringBuiltins = struct
    open Datatypes.DataTypeDictionary

    (* let string_eq_type = FunType (string_typ, FunType (string_typ, )) *)

    let eq_arity = 2

    let eq_type = fun_typ string_typ @@ fun_typ string_typ bool_typ

    let concat_arity = 2

    let concat_type = fun_typ string_typ @@ fun_typ string_typ string_typ

    let substr_arity = 3

    let substr_type =
      fun_typ string_typ @@ fun_typ uint32_typ @@ fun_typ uint32_typ string_typ

    let strlen_arity = 1

    let strlen_type = tfun_typ "'A" (fun_typ (tvar "'A") uint32_typ)

    let strlen_elab _ targs ts =
      match (targs, ts) with
      | [], [ PrimType pt ] -> (
          match pt with
          | String_typ | Bystr_typ -> elab_tfun_with_args_no_gas strlen_type ts
          | _ -> fail0 ~kind:"Failed to elaborate" ?inst:None)
      | _, _ -> fail0 ~kind:"Failed to elaborate" ?inst:None

    let to_string_arity = 1

    let to_string_type = tfun_typ "'A" (fun_typ (tvar "'A") string_typ)

    let to_string_elab _ targs ts =
      match (targs, ts) with
      | [], [ PrimType pt ] -> (
          match pt with
          | Int_typ _ | Uint_typ _ | Bystrx_typ _ | Bystr_typ ->
              elab_tfun_with_args_no_gas to_string_type ts
          | _ -> fail0 ~kind:"Failed to elaborate" ?inst:None)
      | [], [ t ] when is_address_type t ->
          elab_tfun_with_args_no_gas to_string_type
            [ bystrx_typ Type.address_length ]
      | _, _ -> fail0 ~kind:"Failed to elaborate" ?inst:None

    let to_ascii_arity = 1

    let to_ascii_type = tfun_typ "'A" (fun_typ (tvar "'A") string_typ)

    let to_ascii_elab _ targs ts =
      match (targs, ts) with
      | [], [ PrimType pt ] -> (
          match pt with
          | Bystrx_typ _ | Bystr_typ ->
              elab_tfun_with_args_no_gas to_ascii_type ts
          | _ -> fail0 ~kind:"Failed to elaborate" ?inst:None)
      | [], [ t ] when is_address_type t ->
          elab_tfun_with_args_no_gas to_string_type
            [ bystrx_typ Type.address_length ]
      | _, _ -> fail0 ~kind:"Failed to elaborate" ?inst:None

    let strrev_arity = 1

    let strrev_type = tfun_typ "'A" (fun_typ (tvar "'A") (tvar "'A"))

    let strrev_elab _ targs ts =
      match (targs, ts) with
      | [], [ PrimType pt ] -> (
          match pt with
          | String_typ | Bystrx_typ _ | Bystr_typ ->
              elab_tfun_with_args_no_gas strrev_type ts
          | _ -> fail0 ~kind:"Failed to elaborate" ?inst:None)
      | [], [ t ] when is_address_type t ->
          elab_tfun_with_args_no_gas to_string_type
            [ bystrx_typ Type.address_length ]
      | _, _ -> fail0 ~kind:"Failed to elaborate" ?inst:None
  end

  (* Instantiating the functors *)
  open SafeArith
  module Int32_safe = SafeInt (Int32)
  module Int64_safe = SafeInt (Int64)
  module Int128_safe = SafeInt (Int128)
  module Int256_safe = SafeInt (Int256)
  module Uint32_safe = SafeUint (Uint32)
  module Uint64_safe = SafeUint (Uint64)
  module Uint128_safe = SafeUint (Uint128)
  module Uint256_safe = SafeUint (Uint256)

  (*******************************************************)
  (*******************************************************)

  (* Integer operations *)
  module IntBuiltins = struct
    open Datatypes.DataTypeDictionary

    let eq_arity = 2

    let eq_type =
      tfun_typ "'A" (fun_typ (tvar "'A") @@ fun_typ (tvar "'A") bool_typ)

    let eq_elab t targs ts =
      match (targs, ts) with
      | [], [ i1; i2 ] when [%equal: BIType.t] i1 i2 && is_int_type i1 ->
          elab_tfun_with_args_no_gas t [ i1 ]
      | _, _ -> fail0 ~kind:"Failed to elaborate" ?inst:None

    let binop_arity = 2

    let binop_type =
      tfun_typ "'A" (fun_typ (tvar "'A") @@ fun_typ (tvar "'A") (tvar "'A"))

    let binop_elab t targs ts =
      match (targs, ts) with
      | [], [ i1; i2 ] when [%equal: BIType.t] i1 i2 && is_int_type i1 ->
          elab_tfun_with_args_no_gas t [ i1 ]
      | _, _ -> fail0 ~kind:"Failed to elaborate" ?inst:None

    let pow_arity = 2

    let pow_type =
      tfun_typ "'A" (fun_typ (tvar "'A") @@ fun_typ uint32_typ (tvar "'A"))

    let pow_elab t targs ts =
      match (targs, ts) with
      | [], [ i1; i2 ] when is_int_type i1 && [%equal: BIType.t] i2 uint32_typ
        ->
          elab_tfun_with_args_no_gas t [ i1 ]
      | _, _ -> fail0 ~kind:"Failed to elaborate" ?inst:None

    let to_int_arity = 1

    let to_int_type =
      tfun_typ "'A"
      @@ tfun_typ "'B" (fun_typ (tvar "'A") (option_typ (tvar "'B")))

    let to_int_elab w sc targs ts =
      match (targs, ts) with
      | [], [ (PrimType (Int_typ _) as t) ]
      | [], [ (PrimType (Uint_typ _) as t) ]
      | [], [ (PrimType String_typ as t) ] ->
          let ityp = PrimType (Int_typ w) in
          elab_tfun_with_args_no_gas sc [ t; ityp ]
      | _, _ -> fail0 ~kind:"Failed to elaborate" ?inst:None
  end

  (* Unsigned integer operation *)
  module UintBuiltins = struct
    open Datatypes.DataTypeDictionary

    let eq_arity = 2

    let eq_type =
      tfun_typ "'A" (fun_typ (tvar "'A") @@ fun_typ (tvar "'A") bool_typ)

    let eq_elab sc targs ts =
      match (targs, ts) with
      | [], [ i1; i2 ] when [%equal: BIType.t] i1 i2 && is_uint_type i1 ->
          elab_tfun_with_args_no_gas sc [ i1 ]
      | _, _ -> fail0 ~kind:"Failed to elaborate" ?inst:None

    let binop_arity = 2

    let binop_type =
      tfun_typ "'A" (fun_typ (tvar "'A") @@ fun_typ (tvar "'A") (tvar "'A"))

    let binop_elab sc targs ts =
      match (targs, ts) with
      | [], [ i1; i2 ] when [%equal: BIType.t] i1 i2 && is_uint_type i1 ->
          elab_tfun_with_args_no_gas sc [ i1 ]
      | _, _ -> fail0 ~kind:"Failed to elaborate" ?inst:None

    let pow_arity = 2

    let pow_type =
      tfun_typ "'A" (fun_typ (tvar "'A") @@ fun_typ uint32_typ (tvar "'A"))

    let pow_elab t targs ts =
      match (targs, ts) with
      | [], [ i1; i2 ] when is_uint_type i1 && [%equal: BIType.t] i2 uint32_typ
        ->
          elab_tfun_with_args_no_gas t [ i1 ]
      | _, _ -> fail0 ~kind:"Failed to elaborate" ?inst:None

    let isqrt_arity = 1

    let isqrt_type = tfun_typ "'A" (fun_typ (tvar "'A") (tvar "'A"))

    let isqrt_elab t targs ts =
      match (targs, ts) with
      | [], [ i ] when is_uint_type i -> elab_tfun_with_args_no_gas t [ i ]
      | _, _ -> fail0 ~kind:"Failed to elaborate" ?inst:None

    let to_uint_arity = 1

    let to_uint_type =
      tfun_typ "'A"
      @@ tfun_typ "'B" (fun_typ (tvar "'A") (option_typ (tvar "'B")))

    let to_uint_elab w sc targs ts =
      match (targs, ts) with
      | [], [ (PrimType (Int_typ _) as t) ]
      | [], [ (PrimType (Uint_typ _) as t) ]
      | [], [ (PrimType String_typ as t) ] ->
          let ityp = PrimType (Uint_typ w) in
          elab_tfun_with_args_no_gas sc [ t; ityp ]
      | _, _ -> fail0 ~kind:"Failed to elaborate" ?inst:None

    let to_nat_arity = 1

    let to_nat_type = fun_typ uint32_typ nat_typ

    let to_bystrx_arity = 1

    let to_bystrx_type =
      tfun_typ "'A" @@ tfun_typ "'B" (fun_typ (tvar "'A") (tvar "'B"))

    let to_bystrx_elab x sc targs ts =
      let open Type.PrimType in
      match (targs, ts) with
      | [], [ (PrimType (Uint_typ w) as t) ] when int_bit_width_to_int w / 8 = x
        ->
          elab_tfun_with_args_no_gas sc
            [ t; PrimType (Bystrx_typ (int_bit_width_to_int w / 8)) ]
      | _, _ -> fail0 ~kind:"Failed to elaborate" ?inst:None
  end

  (***********************************************************)
  (* Working with block numbers *)
  (***********************************************************)
  module BNumBuiltins = struct
    open Datatypes.DataTypeDictionary

    let eq_type = fun_typ bnum_typ @@ fun_typ bnum_typ bool_typ

    let eq_arity = 2

    let blt_type = fun_typ bnum_typ @@ fun_typ bnum_typ bool_typ

    let blt_arity = 2

    let badd_arity = 2

    let badd_type =
      tfun_typ "'A" @@ tfun_typ "'B"
      @@ fun_typ (tvar "'A")
      @@ fun_typ (tvar "'B") bnum_typ

    (* Elaborator to run with arbitrary uints *)
    let badd_elab sc targs ts =
      match (targs, ts) with
      | [], [ PrimType Bnum_typ; PrimType (Uint_typ _) ] ->
          elab_tfun_with_args_no_gas sc ts
      | _, _ -> fail0 ~kind:"Failed to elaborate" ?inst:None

    let bsub_arity = 2

    let bsub_type =
      tfun_typ "'A" @@ tfun_typ "'B"
      @@ fun_typ (tvar "'A")
      @@ fun_typ (tvar "'B") int256_typ

    (* Elaborator to run with arbitrary uints *)
    let bsub_elab sc targs ts =
      match (targs, ts) with
      | [], [ PrimType Bnum_typ; PrimType Bnum_typ ] ->
          elab_tfun_with_args_no_gas sc ts
      | _, _ -> fail0 ~kind:"Failed to elaborate" ?inst:None
  end

  (***********************************************************)
  (******************** Crypto Builtins *************************)
  (***********************************************************)
  module CryptoBuiltins = struct
    open Datatypes.DataTypeDictionary
    open Scilla_crypto.Schnorr

    let eq_type =
      tfun_typ "'A" (fun_typ (tvar "'A") @@ fun_typ (tvar "'A") bool_typ)

    let eq_arity = 2

    let eq_elab sc targs ts =
      match (targs, ts) with
      | [], [ bstyp1; bstyp2 ]
        when (* Addresses should be compared as ByStr20 *)
             is_address_type bstyp1 || is_address_type bstyp2 ->
          elab_tfun_with_args_no_gas sc [ bystrx_typ Type.address_length ]
      | [], [ bstyp1; bstyp2 ]
        when (* We want both types to be ByStr with equal width. *)
             is_bystrx_type bstyp1 && [%equal: BIType.t] bstyp1 bstyp2 ->
          elab_tfun_with_args_no_gas sc [ bstyp1 ]
      | [], [ PrimType Bystr_typ; PrimType Bystr_typ ] ->
          elab_tfun_with_args_no_gas sc [ PrimType Bystr_typ ]
      | _, _ -> fail0 ~kind:"Failed to elaborate" ?inst:None

    let hash_type =
      tfun_typ "'A" @@ fun_typ (tvar "'A") (bystrx_typ hash_length)

    let hash_arity = 1

    let hash_elab sc targs ts =
      match (targs, ts) with
      | [], [ t ] when is_legal_hash_argument_type t ->
          elab_tfun_with_args_no_gas sc ts
      | _, _ -> fail0 ~kind:"Failed to elaborate" ?inst:None

    (* RIPEMD-160 is a 160 bit hash, so define a separate type for it. *)
    let ripemd160hash_type =
      tfun_typ "'A" @@ fun_typ (tvar "'A") (bystrx_typ Type.address_length)

    (* ByStr -> Option ByStrX *)
    let to_bystrx_type x = fun_typ bystr_typ (option_typ (bystrx_typ x))

    let to_bystrx_arity = 1

    (* ByStrX -> ByStr *)
    let to_bystr_type = tfun_typ "'A" @@ fun_typ (tvar "'A") bystr_typ

    let to_bystr_arity = 1

    let to_bystr_elab sc targs ts =
      match (targs, ts) with
      | [], [ t ] when is_bystrx_type t -> elab_tfun_with_args_no_gas sc ts
      | [], [ t ] when is_address_type t ->
          elab_tfun_with_args_no_gas sc [ bystrx_typ Type.address_length ]
      | _, _ -> fail0 ~kind:"Failed to elaborate" ?inst:None

    let substr_arity = 3

    let substr_type =
      fun_typ bystr_typ @@ fun_typ uint32_typ @@ fun_typ uint32_typ bystr_typ

    let to_uint_type x =
      tfun_typ "'A" @@ fun_typ (tvar "'A") (PrimType (Uint_typ x))

    let to_uint_arity = 1

    let to_uint_elab x sc targs ts =
      let open Type.PrimType in
      match (targs, ts) with
      | [], [ PrimType (Bystrx_typ w) ] when w <= int_bit_width_to_int x / 8 ->
          elab_tfun_with_args_no_gas sc ts
      | [], [ t ]
        when is_address_type t
             && Type.address_length <= int_bit_width_to_int x / 8 ->
          elab_tfun_with_args_no_gas sc [ bystrx_typ Type.address_length ]
      | _, _ -> fail0 ~kind:"Failed to elaborate" ?inst:None

    let bech32_to_bystr20_type =
      fun_typ string_typ
        (fun_typ string_typ (option_typ (bystrx_typ Type.address_length)))

    let bech32_to_bystr20_arity = 2

    let bystr20_to_bech32_type =
      fun_typ string_typ
        (fun_typ (bystrx_typ Type.address_length) (option_typ string_typ))

    let bystr20_to_bech32_arity = 2

    (* ByStrX + ByStrY -> ByStr(X+Y)*)
    let concat_type =
      tfun_typ "'A" @@ tfun_typ "'B" @@ tfun_typ "'C"
      @@ fun_typ (tvar "'A") (fun_typ (tvar "'B") (tvar "'C"))

    let concat_arity = 2

    let concat_elab sc targs ts =
      match (targs, ts) with
      | [], [ PrimType (Bystrx_typ w1); PrimType (Bystrx_typ w2) ] ->
          elab_tfun_with_args_no_gas sc (ts @ [ bystrx_typ (w1 + w2) ])
      | [], [ t1; PrimType (Bystrx_typ w2) ] when is_address_type t1 ->
          elab_tfun_with_args_no_gas sc
            (ts @ [ bystrx_typ (Type.address_length + w2) ])
      | [], [ PrimType (Bystrx_typ w1); t2 ] when is_address_type t2 ->
          elab_tfun_with_args_no_gas sc
            (ts @ [ bystrx_typ (w1 + Type.address_length) ])
      | [], [ t1; t2 ] when is_address_type t1 && is_address_type t2 ->
          elab_tfun_with_args_no_gas sc
            (ts @ [ bystrx_typ (Type.address_length + Type.address_length) ])
      | [], [ PrimType Bystr_typ; PrimType Bystr_typ ] ->
          elab_tfun_with_args_no_gas sc (ts @ [ bystr_typ ])
      | _, _ -> fail0 ~kind:"Failed to elaborate" ?inst:None

    let[@warning "-32"] ec_gen_key_pair_type =
      fun_typ unit_typ
        (pair_typ (bystrx_typ privkey_len) (bystrx_typ pubkey_len))

    let[@warning "-32"] ec_gen_key_pair_arity = 0

    let[@warning "-32"] schnorr_sign_type =
      fun_typ (bystrx_typ privkey_len)
      (* private key *)
      @@ fun_typ (bystrx_typ pubkey_len)
      (* public key *)
      @@ fun_typ bystr_typ
      @@ (* message to be signed *)
      bystrx_typ signature_len

    (* signature *)

    let[@warning "-32"] schnorr_sign_arity = 3

    let[@warning "-32"] schnorr_sign ls _ =
      match ls with
      | [ ByStrX privkey; ByStrX pubkey; ByStr msg ]
        when Bystrx.width privkey = privkey_len
             && Bystrx.width pubkey = pubkey_len -> (
          match
            sign
              (Bystrx.to_raw_bytes privkey)
              (Bystrx.to_raw_bytes pubkey)
              (Bystr.to_raw_bytes msg)
          with
          | Some s -> (
              match Bystrx.of_raw_bytes signature_len s with
              | Some bs -> pure @@ ByStrX bs
              | None ->
                  builtin_fail
                    "schnorr_sign: internal error, invalid signature." ls)
          | None -> builtin_fail "schnorr_sign: internal error." ls)
      | _ -> builtin_fail "schnorr_sign" ls

    let schnorr_verify_type =
      fun_typ (bystrx_typ pubkey_len)
      (* public key *)
      @@ fun_typ bystr_typ
      (* signed message *)
      @@ fun_typ (bystrx_typ signature_len)
      @@ (* signature *)
      bool_typ

    let schnorr_verify_arity = 3

    let[@warning "-32"] ecdsa_sign_type =
      fun_typ (bystrx_typ Secp256k1Wrapper.privkey_len)
      (* private key *)
      @@ fun_typ bystr_typ
      @@ (* message to be signed *)
      bystrx_typ Secp256k1Wrapper.signature_len

    (* signature *)

    let[@warning "-32"] ecdsa_sign_arity = 2

    let[@warning "-32"] ecdsa_sign ls _ =
      let open Secp256k1Wrapper in
      match ls with
      | [ ByStrX privkey; ByStr msg ] when Bystrx.width privkey = privkey_len
        -> (
          let%bind s =
            sign (Bystrx.to_raw_bytes privkey) (Bystr.to_raw_bytes msg)
          in
          match Bystrx.of_raw_bytes signature_len s with
          | Some bs -> pure @@ ByStrX bs
          | None ->
              builtin_fail "ecdsa_sign: internal error, invalid signature." ls)
      | _ -> builtin_fail "ecdsa_sign" ls

    let ecdsa_verify_type =
      fun_typ (bystrx_typ Secp256k1Wrapper.pubkey_len)
      (* public key *)
      @@ fun_typ bystr_typ
      (* signed message *)
      @@ fun_typ (bystrx_typ Secp256k1Wrapper.signature_len)
      @@ (* signature *)
      bool_typ

    let ecdsa_verify_arity = 3

    let ecdsa_recover_pk_arity = 3

    let ecdsa_recover_pk_type =
      (* signed message *)
      fun_typ bystr_typ
      (* signature *)
      @@ fun_typ (bystrx_typ Secp256k1Wrapper.signature_len)
      @@ (* recovery id *)
      fun_typ uint32_typ
        (* public key *)
        (bystrx_typ Secp256k1Wrapper.uncompressed_pubkey_len)

    let schnorr_get_address_type =
      fun_typ (bystrx_typ pubkey_len) (bystrx_typ Type.address_length)

    let schnorr_get_address_arity = 1

    open Datatypes.SnarkTypes

    (* alt_bn128_G1_add : zksnark_g1point_typ -> zksnark_g1point_type ->
                          Option {zksnark_g1point_type} *)
    let alt_bn128_G1_add_type =
      fun_typ g1point_type (fun_typ g1point_type (option_typ g1point_type))

    let alt_bn128_G1_add_arity = 2

    (* alt_bn128_G1_mul : zksnark_g1point_typ -> zksnark_scalar_type ->
                      Option {zksnark_g1point_type} *)
    let alt_bn128_G1_mul_type =
      fun_typ g1point_type (fun_typ scalar_type (option_typ g1point_type))

    let alt_bn128_G1_mul_arity = 2

    (* alt_bn128_G1_neg : zksnark_g1point_typ ->
                          Option {zksnark_g1point_type} *)
    let alt_bn128_G1_neg_type = fun_typ g1point_type (option_typ g1point_type)

    let alt_bn128_G1_neg_arity = 1

    (* alt_bn128_pairing_product : List (g1g2pair_type) -> Option {Bool} *)
    let alt_bn128_pairing_product_type =
      fun_typ g1g2pair_list_type (option_typ bool_typ)

    let alt_bn128_pairing_product_arity = 1
  end

  (***********************************************************)
  (*                        Maps                             *)
  (***********************************************************)
  module MapBuiltins = struct
    open Datatypes.DataTypeDictionary

    let contains_arity = 2

    let contains_type =
      tfun_typ "'K" @@ tfun_typ "'V"
      @@ fun_typ (map_typ (tvar "'K") (tvar "'V"))
      @@ fun_typ (tvar "'K") bool_typ

    let contains_elab sc targs ts =
      match (targs, ts) with
      | [], [ MapType (kt, vt); u ] when type_assignable ~expected:kt ~actual:u
        ->
          elab_tfun_with_args_no_gas sc [ kt; vt ]
      | _, _ -> fail0 ~kind:"Failed to elaborate" ?inst:None

    let put_arity = 3

    let put_type =
      tfun_typ "'K" @@ tfun_typ "'V"
      @@ fun_typ (map_typ (tvar "'K") (tvar "'V"))
      @@ fun_typ (tvar "'K")
      @@ fun_typ (tvar "'V") (map_typ (tvar "'K") (tvar "'V"))

    let put_elab sc targs ts =
      match (targs, ts) with
      | [], [ MapType (kt, vt); kt'; vt' ]
        when type_assignable ~expected:kt ~actual:kt'
             && type_assignable ~expected:vt ~actual:vt' ->
          elab_tfun_with_args_no_gas sc [ kt; vt ]
      | _, _ -> fail0 ~kind:"Failed to elaborate" ?inst:None

    (* Must take result type into the account *)
    let get_arity = 2

    let get_type =
      tfun_typ "'K" @@ tfun_typ "'V"
      @@ fun_typ (map_typ (tvar "'K") (tvar "'V"))
      @@ fun_typ (tvar "'K") (option_typ (tvar "'V"))

    let get_elab sc targs ts =
      match (targs, ts) with
      | [], [ MapType (kt, vt); kt' ]
        when type_assignable ~expected:kt ~actual:kt' ->
          elab_tfun_with_args_no_gas sc [ kt; vt ]
      | _, _ -> fail0 ~kind:"Failed to elaborate" ?inst:None

    let remove_arity = 2

    let remove_type =
      tfun_typ "'K" @@ tfun_typ "'V"
      @@ fun_typ (map_typ (tvar "'K") (tvar "'V"))
      @@ fun_typ (tvar "'K") (map_typ (tvar "'K") (tvar "'V"))

    let remove_elab sc targs ts =
      match (targs, ts) with
      | [], [ MapType (kt, vt); u ] when type_assignable ~expected:kt ~actual:u
        ->
          elab_tfun_with_args_no_gas sc [ kt; vt ]
      | _, _ -> fail0 ~kind:"Failed to elaborate" ?inst:None

    let to_list_arity = 1

    let to_list_type =
      tfun_typ "'K" @@ tfun_typ "'V"
      @@ fun_typ (map_typ (tvar "'K") (tvar "'V"))
      @@ list_typ (pair_typ (tvar "'K") (tvar "'V"))

    let to_list_elab sc targs ts =
      match (targs, ts) with
      | [], [ MapType (kt, vt) ] -> elab_tfun_with_args_no_gas sc [ kt; vt ]
      | _, _ -> fail0 ~kind:"Failed to elaborate" ?inst:None

    let size_arity = 1

    let size_type =
      tfun_typ "'K" @@ tfun_typ "'V"
      @@ fun_typ (map_typ (tvar "'K") (tvar "'V"))
      @@ uint32_typ

    let size_elab sc targs ts =
      match (targs, ts) with
      | [], [ MapType (kt, vt) ] -> elab_tfun_with_args_no_gas sc [ kt; vt ]
      | _, _ -> fail0 ~kind:"Failed to elaborate" ?inst:None
  end

  (* Identity elaborator *)
  let elab_id t _ _ = pure t

  (**********************************************************)
  (*                   Built-in  Dictionary                *)
  (**********************************************************)

  module BuiltInDictionary = struct
    (* A built-in record type:
       * arity
       * full, unelaborated type
       * elaborator, refining the type based on argument
         to support polymorphism -- e.g., for ints and maps
       * executor - operational semantics of the built-in
    *)
    type built_in_record = int * BIType.t * elaborator

    [@@@ocamlformat "disable"]

    (* All built-in functions *)
    let built_in_multidict : builtin -> built_in_record list = function
      (* Polymorphic builtins *)
      | Builtin_eq -> [StringBuiltins.eq_arity, StringBuiltins.eq_type, elab_id;
                       BNumBuiltins.eq_arity, BNumBuiltins.eq_type, elab_id;
                       CryptoBuiltins.eq_arity, CryptoBuiltins.eq_type, CryptoBuiltins.eq_elab;
                       IntBuiltins.eq_arity, IntBuiltins.eq_type, IntBuiltins.eq_elab;
                       UintBuiltins.eq_arity, UintBuiltins.eq_type, UintBuiltins.eq_elab]
      | Builtin_concat -> [StringBuiltins.concat_arity, StringBuiltins.concat_type, elab_id;
                           CryptoBuiltins.concat_arity, CryptoBuiltins.concat_type, CryptoBuiltins.concat_elab]
      | Builtin_substr -> [StringBuiltins.substr_arity, StringBuiltins.substr_type, elab_id;
                           CryptoBuiltins.substr_arity, CryptoBuiltins.substr_type, elab_id
                          ]
      | Builtin_strlen -> [StringBuiltins.strlen_arity, StringBuiltins.strlen_type, StringBuiltins.strlen_elab]
      | Builtin_to_string -> [StringBuiltins.to_string_arity, StringBuiltins.to_string_type, StringBuiltins.to_string_elab]
      | Builtin_to_ascii -> [StringBuiltins.to_ascii_arity, StringBuiltins.to_ascii_type, StringBuiltins.to_ascii_elab]
      | Builtin_strrev -> [ StringBuiltins.strrev_arity, StringBuiltins.strrev_type, StringBuiltins.strrev_elab]
      | Builtin_to_bystrx i -> [
        CryptoBuiltins.to_bystrx_arity, CryptoBuiltins.to_bystrx_type i, elab_id;
        UintBuiltins.to_bystrx_arity, UintBuiltins.to_bystrx_type, UintBuiltins.to_bystrx_elab i
      ]
    
      (* Block numbers *)
      | Builtin_blt -> [BNumBuiltins.blt_arity, BNumBuiltins.blt_type, elab_id]
      | Builtin_badd -> [BNumBuiltins.badd_arity, BNumBuiltins.badd_type, BNumBuiltins.badd_elab]
      | Builtin_bsub -> [BNumBuiltins.bsub_arity, BNumBuiltins.bsub_type, BNumBuiltins.bsub_elab]
    
      (* CryptoBuiltins *)
      | Builtin_sha256hash -> [CryptoBuiltins.hash_arity, CryptoBuiltins.hash_type,CryptoBuiltins.hash_elab]
      | Builtin_keccak256hash -> [CryptoBuiltins.hash_arity, CryptoBuiltins.hash_type,CryptoBuiltins.hash_elab]
      | Builtin_ripemd160hash -> [CryptoBuiltins.hash_arity, CryptoBuiltins.ripemd160hash_type,CryptoBuiltins.hash_elab]
      | Builtin_to_bystr -> [CryptoBuiltins.to_bystr_arity, CryptoBuiltins.to_bystr_type, CryptoBuiltins.to_bystr_elab]
      | Builtin_bech32_to_bystr20 -> [CryptoBuiltins.bech32_to_bystr20_arity, CryptoBuiltins.bech32_to_bystr20_type, elab_id]
      | Builtin_bystr20_to_bech32 -> [CryptoBuiltins.bystr20_to_bech32_arity, CryptoBuiltins.bystr20_to_bech32_type, elab_id]
      | Builtin_schnorr_verify -> [CryptoBuiltins.schnorr_verify_arity, CryptoBuiltins.schnorr_verify_type, elab_id]
      | Builtin_ecdsa_verify -> [CryptoBuiltins.ecdsa_verify_arity, CryptoBuiltins.ecdsa_verify_type, elab_id]
      | Builtin_ecdsa_recover_pk -> [CryptoBuiltins.ecdsa_recover_pk_arity, CryptoBuiltins.ecdsa_recover_pk_type, elab_id]
      | Builtin_schnorr_get_address -> [CryptoBuiltins.schnorr_get_address_arity, CryptoBuiltins.schnorr_get_address_type, elab_id]
      | Builtin_alt_bn128_G1_add -> [CryptoBuiltins.alt_bn128_G1_add_arity, CryptoBuiltins.alt_bn128_G1_add_type, elab_id]
      | Builtin_alt_bn128_G1_mul -> [CryptoBuiltins.alt_bn128_G1_mul_arity, CryptoBuiltins.alt_bn128_G1_mul_type, elab_id]
      | Builtin_alt_bn128_G1_neg -> [CryptoBuiltins.alt_bn128_G1_neg_arity, CryptoBuiltins.alt_bn128_G1_neg_type, elab_id]
      | Builtin_alt_bn128_pairing_product -> [CryptoBuiltins.alt_bn128_pairing_product_arity, 
                                              CryptoBuiltins.alt_bn128_pairing_product_type, elab_id]
    
      (* Maps *)
      | Builtin_contains -> [MapBuiltins.contains_arity, MapBuiltins.contains_type, MapBuiltins.contains_elab]
      | Builtin_put -> [MapBuiltins.put_arity, MapBuiltins.put_type, MapBuiltins.put_elab]
      | Builtin_get -> [MapBuiltins.get_arity, MapBuiltins.get_type, MapBuiltins.get_elab]
      | Builtin_remove -> [MapBuiltins.remove_arity, MapBuiltins.remove_type, MapBuiltins.remove_elab]
      | Builtin_to_list -> [MapBuiltins.to_list_arity, MapBuiltins.to_list_type, MapBuiltins.to_list_elab]
      | Builtin_size -> [MapBuiltins.size_arity, MapBuiltins.size_type, MapBuiltins.size_elab]
    
      (* Integers *)
      | Builtin_lt -> [IntBuiltins.eq_arity, IntBuiltins.eq_type, IntBuiltins.eq_elab;
                       UintBuiltins.eq_arity, UintBuiltins.eq_type, UintBuiltins.eq_elab]
      | Builtin_add -> [IntBuiltins.binop_arity, IntBuiltins.binop_type, IntBuiltins.binop_elab;
                        UintBuiltins.binop_arity, UintBuiltins.binop_type, UintBuiltins.binop_elab]
      | Builtin_sub -> [IntBuiltins.binop_arity, IntBuiltins.binop_type, IntBuiltins.binop_elab;
                        UintBuiltins.binop_arity, UintBuiltins.binop_type, UintBuiltins.binop_elab]
      | Builtin_mul -> [IntBuiltins.binop_arity, IntBuiltins.binop_type, IntBuiltins.binop_elab;
                        UintBuiltins.binop_arity, UintBuiltins.binop_type, UintBuiltins.binop_elab]
      | Builtin_div -> [IntBuiltins.binop_arity, IntBuiltins.binop_type, IntBuiltins.binop_elab;
                        UintBuiltins.binop_arity, UintBuiltins.binop_type, UintBuiltins.binop_elab]
      | Builtin_rem -> [IntBuiltins.binop_arity, IntBuiltins.binop_type, IntBuiltins.binop_elab;
                        UintBuiltins.binop_arity, UintBuiltins.binop_type, UintBuiltins.binop_elab]
      | Builtin_pow -> [IntBuiltins.pow_arity, IntBuiltins.pow_type, IntBuiltins.pow_elab;
                        UintBuiltins.pow_arity, UintBuiltins.pow_type, UintBuiltins.pow_elab]
      | Builtin_isqrt -> [UintBuiltins.isqrt_arity, UintBuiltins.isqrt_type, UintBuiltins.isqrt_elab]
    
      (* Signed integers specific builtins *)
      | Builtin_to_int32 -> [IntBuiltins.to_int_arity, IntBuiltins.to_int_type, IntBuiltins.to_int_elab Bits32]
      | Builtin_to_int64 -> [IntBuiltins.to_int_arity, IntBuiltins.to_int_type, IntBuiltins.to_int_elab Bits64]
      | Builtin_to_int128 -> [IntBuiltins.to_int_arity, IntBuiltins.to_int_type, IntBuiltins.to_int_elab Bits128]
      | Builtin_to_int256 -> [IntBuiltins.to_int_arity, IntBuiltins.to_int_type, IntBuiltins.to_int_elab Bits256]
    
      (* Unsigned integers specific builtins *)
      | Builtin_to_uint32 -> [
          CryptoBuiltins.to_uint_arity, CryptoBuiltins.to_uint_type Bits32, CryptoBuiltins.to_uint_elab Bits32;
          UintBuiltins.to_uint_arity, UintBuiltins.to_uint_type, UintBuiltins.to_uint_elab Bits32
        ]
      | Builtin_to_uint64 -> [
          CryptoBuiltins.to_uint_arity, CryptoBuiltins.to_uint_type Bits64, CryptoBuiltins.to_uint_elab Bits64;
          UintBuiltins.to_uint_arity, UintBuiltins.to_uint_type, UintBuiltins.to_uint_elab Bits64
        ]
      | Builtin_to_uint128 -> [
          CryptoBuiltins.to_uint_arity, CryptoBuiltins.to_uint_type Bits128, CryptoBuiltins.to_uint_elab Bits128;
          UintBuiltins.to_uint_arity, UintBuiltins.to_uint_type, UintBuiltins.to_uint_elab Bits128
        ]
      | Builtin_to_uint256 -> [
          CryptoBuiltins.to_uint_arity, CryptoBuiltins.to_uint_type Bits256, CryptoBuiltins.to_uint_elab Bits256;
          UintBuiltins.to_uint_arity, UintBuiltins.to_uint_type, UintBuiltins.to_uint_elab Bits256
        ]
      | Builtin_to_nat -> [UintBuiltins.to_nat_arity, UintBuiltins.to_nat_type, elab_id]

    [@@@ocamlformat "enable"]

    (* Dictionary lookup based on the operation name and type *)
    let find_builtin_op (op, rep) ~targtypes ~vargtypes =
      let finder = function
        | arity, optype, elab ->
            if arity = List.length vargtypes then
              (* First: elaborate based on argument types *)
              let%bind type_elab = elab optype targtypes vargtypes in
              (* Second: check applicability *)
              let%bind res_type =
                fun_type_applies type_elab vargtypes ~lc:(ER.get_loc rep)
              in
              pure res_type
            else fail0 ~kind:"Name or arity don't match" ?inst:None
      in
      let dict = built_in_multidict op in
      let%bind _, res_type =
        tryM dict ~f:finder ~msg:(fun () ->
            mk_error1
              ~kind:
                (sprintf
                   "Type error: cannot apply \"%s\" built-in to argument(s) of \
                    type(s)"
                   (pp_builtin op))
              ~inst:
                (sprintf "%s%s"
                   (if List.is_empty targtypes then ""
                   else sprintf "{%s} " (pp_typ_list_error targtypes))
                   (pp_typ_list_error vargtypes))
              (ER.get_loc rep))
      in
      pure res_type
  end
end
