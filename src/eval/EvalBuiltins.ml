(*
  This file is part of scilla.

  Copyright (c) 2021 - present Zilliqa Research Pvt. Ltd.

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

open Core_kernel
open Scilla_base
open Syntax
open ErrorUtils
open MonadUtil
open BuiltIns
open TypeUtil
open Big_int
open Stdint
open Integer256
open TypeUtilities
open Result.Let_syntax

module ScillaEvalBuiltIns (SR : Rep) (ER : Rep) = struct
  module BaseBuiltins = ScillaBuiltIns (SR) (ER)
  open BaseBuiltins
  open BILiteral
  open BIType

  let print_literal_list ls = PrettyPrinters.pp_literal_list ls

  let rec serialize_literal l =
    match l with
    (* we don't keep type information here *)
    | StringLit s -> s
    | IntLit il -> bstring_from_int_lit il
    | UintLit uil -> bstring_from_uint_lit uil
    | BNum s -> s
    | ByStr bs -> Bystr.to_raw_bytes bs
    | ByStrX bs -> Bystrx.to_raw_bytes bs
    | Msg entries ->
        let raw_entries =
          List.map entries ~f:(fun (s, _t, v) -> s ^ serialize_literal v)
        in
        Core_kernel.String.concat ~sep:"" raw_entries
    | Map (_, tbl) ->
        let raw_strings =
          let tbl' =
            (* Sort based on keys to keep the serialization predictable
             * across implementations / platforms. *)
            Caml.List.sort
              (fun (k1, _) (k2, _) ->
                String.compare (serialize_literal k1) (serialize_literal k2))
              (Caml.List.of_seq @@ Caml.Hashtbl.to_seq tbl)
          in
          Caml.List.fold_left
            (fun acc (k, v) ->
              serialize_literal k :: serialize_literal v :: acc)
            [] tbl'
        in
        Core_kernel.String.concat ~sep:"" raw_strings
    | ADTValue (cons_name, _, params) ->
        let raw_params = List.map params ~f:serialize_literal in
        Core_kernel.String.concat ~sep:""
          (BIName.as_string cons_name :: raw_params)
    | Clo _fun -> "(Clo <fun>)"
    | TAbs _fun -> "(Tabs <fun>)"

  let builtin_fail name ls =
    fail0
      ~kind:(sprintf "Cannot apply built-in %s to a list of arguments" name)
      ~inst:(print_literal_list ls)

  (*******************************************************)
  (**************** String *******************************)
  (*******************************************************)

  (* String operations *)
  module EvalStringBuiltins = struct
    include StringBuiltins

    let eq _ ls _ =
      match ls with
      | [ StringLit x; StringLit y ] -> pure @@ build_bool_lit String.(x = y)
      | _ -> builtin_fail "String.eq" ls

    let concat _ ls _ =
      match ls with
      | [ StringLit x; StringLit y ] -> pure @@ StringLit (x ^ y)
      | _ -> builtin_fail "String.concat" ls

    let substr _ ls _ =
      match ls with
      | [ StringLit x; UintLit (Uint32L s); UintLit (Uint32L e) ] -> (
          try
            pure
            @@ StringLit
                 (String.sub x ~pos:(Uint32.to_int s) ~len:(Uint32.to_int e))
          with Invalid_argument msg ->
            builtin_fail ("String.substr: " ^ msg) ls)
      | _ -> builtin_fail "String.substr" ls

    let strlen _ ls _ =
      match ls with
      | [ StringLit x ] ->
          pure @@ UintLit (Uint32L (Uint32.of_int (String.length x)))
      | [ ByStr bs ] ->
          pure @@ UintLit (Uint32L (Uint32.of_int (Bystr.length bs)))
      | _ -> builtin_fail "String.strlen" ls

    let to_string _ ls _ =
      let%bind s =
        match ls with
        | [ IntLit x ] -> pure @@ string_of_int_lit x
        | [ UintLit x ] -> pure @@ string_of_uint_lit x
        | [ ByStr x ] -> pure @@ Bystr.hex_encoding x
        | [ ByStrX x ] -> pure @@ Bystrx.hex_encoding x
        | _ -> builtin_fail (sprintf "String.to_string") ls
      in
      pure @@ StringLit s

    let to_ascii _ ls _ =
      let%bind s =
        match ls with
        | [ ByStr x ] -> pure @@ Bystr.to_raw_bytes x
        | [ ByStrX x ] -> pure @@ Bystrx.to_raw_bytes x
        | _ -> builtin_fail (sprintf "String.to_ascii") ls
      in
      if validate_string_literal s then pure @@ StringLit s
      else fail0 ~kind:"String.to_ascii: Not printable" ?inst:None

    let strrev _ ls _ =
      match ls with
      | [ StringLit x ] -> pure @@ StringLit (String.rev x)
      | [ ByStr x ] -> pure @@ ByStr (Bystr.rev x)
      | [ ByStrX x ] -> pure @@ ByStrX (Bystrx.rev x)
      | _ -> builtin_fail (sprintf "String.strrev") ls
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
  module EvalIntBuiltins = struct
    include IntBuiltins

    let eq _ ls _ =
      match ls with
      | [ IntLit x; IntLit y ] -> pure @@ build_bool_lit ([%equal: int_lit] x y)
      | _ -> builtin_fail "Int.eq: unsupported types" ls

    let add _ ls _ =
      try
        let%bind l =
          match ls with
          | [ IntLit (Int32L x); IntLit (Int32L y) ] ->
              pure @@ Int32L (Int32_safe.add x y)
          | [ IntLit (Int64L x); IntLit (Int64L y) ] ->
              pure @@ Int64L (Int64_safe.add x y)
          | [ IntLit (Int128L x); IntLit (Int128L y) ] ->
              pure @@ Int128L (Int128_safe.add x y)
          | [ IntLit (Int256L x); IntLit (Int256L y) ] ->
              pure @@ Int256L (Int256_safe.add x y)
          | _ -> builtin_fail "Int.add: unsupported types" ls
        in
        pure @@ IntLit l
      with IntOverflow | IntUnderflow ->
        builtin_fail "Int.add: an overflow/underflow occurred" ls

    let sub _ ls _ =
      try
        let%bind l =
          match ls with
          | [ IntLit (Int32L x); IntLit (Int32L y) ] ->
              pure @@ Int32L (Int32_safe.sub x y)
          | [ IntLit (Int64L x); IntLit (Int64L y) ] ->
              pure @@ Int64L (Int64_safe.sub x y)
          | [ IntLit (Int128L x); IntLit (Int128L y) ] ->
              pure @@ Int128L (Int128_safe.sub x y)
          | [ IntLit (Int256L x); IntLit (Int256L y) ] ->
              pure @@ Int256L (Int256_safe.sub x y)
          | _ -> builtin_fail "Int.sub: unsupported types" ls
        in
        pure @@ IntLit l
      with IntOverflow | IntUnderflow ->
        builtin_fail "Int.sub: an overflow/underflow occurred" ls

    let mul _ ls _ =
      try
        let%bind l =
          match ls with
          | [ IntLit (Int32L x); IntLit (Int32L y) ] ->
              pure @@ Int32L (Int32_safe.mul x y)
          | [ IntLit (Int64L x); IntLit (Int64L y) ] ->
              pure @@ Int64L (Int64_safe.mul x y)
          | [ IntLit (Int128L x); IntLit (Int128L y) ] ->
              pure @@ Int128L (Int128_safe.mul x y)
          | [ IntLit (Int256L x); IntLit (Int256L y) ] ->
              pure @@ Int256L (Int256_safe.mul x y)
          | _ -> builtin_fail "Int.mul: unsupported types" ls
        in
        pure @@ IntLit l
      with IntOverflow | IntUnderflow ->
        builtin_fail "Int.mul: an overflow/underflow occurred" ls

    let div _ ls _ =
      try
        let%bind l =
          match ls with
          | [ IntLit (Int32L x); IntLit (Int32L y) ] ->
              pure @@ Int32L (Int32_safe.div x y)
          | [ IntLit (Int64L x); IntLit (Int64L y) ] ->
              pure @@ Int64L (Int64_safe.div x y)
          | [ IntLit (Int128L x); IntLit (Int128L y) ] ->
              pure @@ Int128L (Int128_safe.div x y)
          | [ IntLit (Int256L x); IntLit (Int256L y) ] ->
              pure @@ Int256L (Int256_safe.div x y)
          | _ -> builtin_fail "Int.div: unsupported types" ls
        in
        pure @@ IntLit l
      with Division_by_zero | IntOverflow ->
        builtin_fail "Int.div: Division by zero / IntOverflow error occurred" ls

    let rem _ ls _ =
      try
        let%bind l =
          match ls with
          | [ IntLit (Int32L x); IntLit (Int32L y) ] ->
              pure @@ Int32L (Int32_safe.rem x y)
          | [ IntLit (Int64L x); IntLit (Int64L y) ] ->
              pure @@ Int64L (Int64_safe.rem x y)
          | [ IntLit (Int128L x); IntLit (Int128L y) ] ->
              pure @@ Int128L (Int128_safe.rem x y)
          | [ IntLit (Int256L x); IntLit (Int256L y) ] ->
              pure @@ Int256L (Int256_safe.rem x y)
          | _ -> builtin_fail "Int.rem: unsupported types" ls
        in
        pure @@ IntLit l
      with Division_by_zero ->
        builtin_fail "Int.rem: Division by zero error occurred" ls

    let pow _ ls _ =
      try
        let%bind l =
          match ls with
          | [ IntLit (Int32L x); UintLit (Uint32L y) ] ->
              pure @@ Int32L (Int32_safe.pow x y)
          | [ IntLit (Int64L x); UintLit (Uint32L y) ] ->
              pure @@ Int64L (Int64_safe.pow x y)
          | [ IntLit (Int128L x); UintLit (Uint32L y) ] ->
              pure @@ Int128L (Int128_safe.pow x y)
          | [ IntLit (Int256L x); UintLit (Uint32L y) ] ->
              pure @@ Int256L (Int256_safe.pow x y)
          | _ -> builtin_fail "Int.pow: unsupported types" ls
        in
        pure @@ IntLit l
      with IntOverflow | IntUnderflow ->
        builtin_fail "Int.pow: an overflow/underflow occurred" ls

    let lt _ ls _ =
      try
        match ls with
        | [ IntLit (Int32L x); IntLit (Int32L y) ] ->
            pure @@ build_bool_lit (Int32_safe.lt x y)
        | [ IntLit (Int64L x); IntLit (Int64L y) ] ->
            pure @@ build_bool_lit (Int64_safe.lt x y)
        | [ IntLit (Int128L x); IntLit (Int128L y) ] ->
            pure @@ build_bool_lit (Int128_safe.lt x y)
        | [ IntLit (Int256L x); IntLit (Int256L y) ] ->
            pure @@ build_bool_lit (Int256_safe.lt x y)
        | _ -> builtin_fail "Int.lt: unsupported types" ls
      with IntOverflow | IntUnderflow ->
        builtin_fail "Int.lt: an overflow/underflow occurred" ls

    let to_int_helper ls w =
      let open Type.PrimType in
      let%bind xs =
        match ls with
        | [ IntLit x ] -> pure @@ string_of_int_lit x
        | [ UintLit x ] -> pure @@ string_of_uint_lit x
        | [ StringLit x ] -> pure x
        | _ -> builtin_fail ("Int.to_int" ^ int_bit_width_to_string w) ls
      in
      let iptyp = Int_typ w in
      match build_prim_literal iptyp xs with
      | Some lit -> pure @@ build_some_lit lit (PrimType iptyp)
      | None -> pure @@ build_none_lit (PrimType iptyp)

    let to_int32 _ ls _ = to_int_helper ls Bits32

    let to_int64 _ ls _ = to_int_helper ls Bits64

    let to_int128 _ ls _ = to_int_helper ls Bits128

    let to_int256 _ ls _ = to_int_helper ls Bits256
  end

  (* Unsigned integer operation *)
  module EvalUintBuiltins = struct
    include UintBuiltins

    let eq _ ls _ =
      match ls with
      | [ UintLit x; UintLit y ] ->
          pure @@ build_bool_lit ([%equal: uint_lit] x y)
      | _ -> builtin_fail "Uint.eq: unsupported types" ls

    let add _ ls _ =
      try
        let%bind l =
          match ls with
          | [ UintLit (Uint32L x); UintLit (Uint32L y) ] ->
              pure @@ Uint32L (Uint32_safe.add x y)
          | [ UintLit (Uint64L x); UintLit (Uint64L y) ] ->
              pure @@ Uint64L (Uint64_safe.add x y)
          | [ UintLit (Uint128L x); UintLit (Uint128L y) ] ->
              pure @@ Uint128L (Uint128_safe.add x y)
          | [ UintLit (Uint256L x); UintLit (Uint256L y) ] ->
              pure @@ Uint256L (Uint256_safe.add x y)
          | _ -> builtin_fail "Uint.add: unsupported types" ls
        in
        pure @@ UintLit l
      with IntOverflow | IntUnderflow ->
        builtin_fail "Uint.add: an overflow/underflow occurred" ls

    let sub _ ls _ =
      try
        let%bind l =
          match ls with
          | [ UintLit (Uint32L x); UintLit (Uint32L y) ] ->
              pure @@ Uint32L (Uint32_safe.sub x y)
          | [ UintLit (Uint64L x); UintLit (Uint64L y) ] ->
              pure @@ Uint64L (Uint64_safe.sub x y)
          | [ UintLit (Uint128L x); UintLit (Uint128L y) ] ->
              pure @@ Uint128L (Uint128_safe.sub x y)
          | [ UintLit (Uint256L x); UintLit (Uint256L y) ] ->
              pure @@ Uint256L (Uint256_safe.sub x y)
          | _ -> builtin_fail "Uint.sub: unsupported types" ls
        in
        pure @@ UintLit l
      with IntOverflow | IntUnderflow ->
        builtin_fail "Uint.sub: an overflow/underflow occurred" ls

    let mul _ ls _ =
      try
        let%bind l =
          match ls with
          | [ UintLit (Uint32L x); UintLit (Uint32L y) ] ->
              pure @@ Uint32L (Uint32_safe.mul x y)
          | [ UintLit (Uint64L x); UintLit (Uint64L y) ] ->
              pure @@ Uint64L (Uint64_safe.mul x y)
          | [ UintLit (Uint128L x); UintLit (Uint128L y) ] ->
              pure @@ Uint128L (Uint128_safe.mul x y)
          | [ UintLit (Uint256L x); UintLit (Uint256L y) ] ->
              pure @@ Uint256L (Uint256_safe.mul x y)
          | _ -> builtin_fail "Uint.mul: unsupported types" ls
        in
        pure @@ UintLit l
      with IntOverflow | IntUnderflow ->
        builtin_fail "Uint.mul: an overflow/underflow occurred" ls

    let div _ ls _ =
      try
        let%bind l =
          match ls with
          | [ UintLit (Uint32L x); UintLit (Uint32L y) ] ->
              pure @@ Uint32L (Uint32_safe.div x y)
          | [ UintLit (Uint64L x); UintLit (Uint64L y) ] ->
              pure @@ Uint64L (Uint64_safe.div x y)
          | [ UintLit (Uint128L x); UintLit (Uint128L y) ] ->
              pure @@ Uint128L (Uint128_safe.div x y)
          | [ UintLit (Uint256L x); UintLit (Uint256L y) ] ->
              pure @@ Uint256L (Uint256_safe.div x y)
          | _ -> builtin_fail "Uint.div: unsupported types" ls
        in
        pure @@ UintLit l
      with Division_by_zero ->
        builtin_fail "Uint.div: Division by zero / UintOverflow error occurred"
          ls

    let rem _ ls _ =
      try
        let%bind l =
          match ls with
          | [ UintLit (Uint32L x); UintLit (Uint32L y) ] ->
              pure @@ Uint32L (Uint32_safe.rem x y)
          | [ UintLit (Uint64L x); UintLit (Uint64L y) ] ->
              pure @@ Uint64L (Uint64_safe.rem x y)
          | [ UintLit (Uint128L x); UintLit (Uint128L y) ] ->
              pure @@ Uint128L (Uint128_safe.rem x y)
          | [ UintLit (Uint256L x); UintLit (Uint256L y) ] ->
              pure @@ Uint256L (Uint256_safe.rem x y)
          | _ -> builtin_fail "Uint.rem: unsupported types" ls
        in
        pure @@ UintLit l
      with Division_by_zero ->
        builtin_fail "Uint.rem: Division by zero error occurred" ls

    let pow _ ls _ =
      try
        let%bind l =
          match ls with
          | [ UintLit (Uint32L x); UintLit (Uint32L y) ] ->
              pure @@ Uint32L (Uint32_safe.pow x y)
          | [ UintLit (Uint64L x); UintLit (Uint32L y) ] ->
              pure @@ Uint64L (Uint64_safe.pow x y)
          | [ UintLit (Uint128L x); UintLit (Uint32L y) ] ->
              pure @@ Uint128L (Uint128_safe.pow x y)
          | [ UintLit (Uint256L x); UintLit (Uint32L y) ] ->
              pure @@ Uint256L (Uint256_safe.pow x y)
          | _ -> builtin_fail "Int.pow: unsupported types" ls
        in
        pure @@ UintLit l
      with IntOverflow | IntUnderflow ->
        builtin_fail "Int.pow: an overflow/underflow occurred" ls

    let isqrt _ ls _ =
      try
        let%bind l =
          match ls with
          | [ UintLit (Uint32L x) ] -> pure @@ Uint32L (Uint32_safe.isqrt x)
          | [ UintLit (Uint64L x) ] -> pure @@ Uint64L (Uint64_safe.isqrt x)
          | [ UintLit (Uint128L x) ] -> pure @@ Uint128L (Uint128_safe.isqrt x)
          | [ UintLit (Uint256L x) ] -> pure @@ Uint256L (Uint256_safe.isqrt x)
          | _ -> builtin_fail "Uint.isqrt: unsupported types" ls
        in
        pure @@ UintLit l
      with IntOverflow | IntUnderflow ->
        builtin_fail "Uint.isqrt: isqrt cannot throw, impossible!" ls

    let lt _ ls _ =
      try
        match ls with
        | [ UintLit (Uint32L x); UintLit (Uint32L y) ] ->
            pure @@ build_bool_lit (Uint32_safe.lt x y)
        | [ UintLit (Uint64L x); UintLit (Uint64L y) ] ->
            pure @@ build_bool_lit (Uint64_safe.lt x y)
        | [ UintLit (Uint128L x); UintLit (Uint128L y) ] ->
            pure @@ build_bool_lit (Uint128_safe.lt x y)
        | [ UintLit (Uint256L x); UintLit (Uint256L y) ] ->
            pure @@ build_bool_lit (Uint256_safe.lt x y)
        | _ -> builtin_fail "Uint.lt: unsupported types" ls
      with IntOverflow | IntUnderflow ->
        builtin_fail "Uint.lt: an overflow/underflow occurred" ls

    let to_uint_helper ls w =
      let open Type.PrimType in
      let%bind xs =
        match ls with
        | [ IntLit x ] -> pure @@ string_of_int_lit x
        | [ UintLit x ] -> pure @@ string_of_uint_lit x
        | [ StringLit x ] -> pure x
        | _ -> builtin_fail ("UInt.to_uint" ^ int_bit_width_to_string w) ls
      in

      let iptyp = Uint_typ w in
      match build_prim_literal iptyp xs with
      | Some lit -> pure @@ build_some_lit lit (PrimType iptyp)
      | None -> pure @@ build_none_lit (PrimType iptyp)

    let to_uint32 _ ls _ = to_uint_helper ls Bits32

    let to_uint64 _ ls _ = to_uint_helper ls Bits64

    let to_uint128 _ ls _ = to_uint_helper ls Bits128

    let to_uint256 _ ls _ = to_uint_helper ls Bits256

    let to_nat _ ls _ =
      match ls with
      | [ UintLit (Uint32L n) ] ->
          let rec nat_builder (i : Uint32.t) (acc : BILiteral.t) =
            if [%equal: uint32] i Uint32.zero then acc
            else nat_builder (Uint32.pred i) (build_succ_lit acc)
          in
          pure @@ nat_builder n zero_lit
      (* Other integer widths can be in the library, using integer conversions. *)
      | _ -> builtin_fail "Uint.to_nat only supported for Uint32" ls

    let to_bystrx _ ls _ =
      match ls with
      | [ UintLit nl ] -> (
          match
            Bystrx.of_raw_bytes
              (uint_lit_width nl / 8)
              (bstring_from_uint_lit nl)
          with
          | Some nlbs -> pure @@ ByStrX nlbs
          | None -> fail0 ~kind:"Internal error in converting UintLit to ByStrX" ?inst:None)
      | _ -> builtin_fail "Uint.to_bystrx: unsupported type" ls
  end

  (***********************************************************)
  (* Working with block numbers *)
  (***********************************************************)
  module EvalBNumBuiltins = struct
    include BNumBuiltins

    let eq _ ls _ =
      match ls with
      | [ BNum x; BNum y ] -> pure @@ build_bool_lit Core_kernel.String.(x = y)
      | _ -> builtin_fail "BNum.eq" ls

    let blt _ ls _ =
      match ls with
      | [ BNum x; BNum y ] ->
          pure
            (let i1 = big_int_of_string x in
             let i2 = big_int_of_string y in
             build_bool_lit (lt_big_int i1 i2))
      | _ -> builtin_fail "BNum.blt" ls

    let badd _ ls _ =
      match ls with
      | [ BNum x; UintLit y ] ->
          let i1 = big_int_of_string x in
          let i2 = big_int_of_string (string_of_uint_lit y) in
          if ge_big_int i2 (big_int_of_int 0) then
            pure @@ BNum (string_of_big_int (add_big_int i1 i2))
          else
            fail0
              ~kind:"Cannot add a negative value to a block"
              ~inst:(string_of_uint_lit y)
      | _ -> builtin_fail "BNum.badd" ls

    let bsub _ ls _ =
      match ls with
      | [ BNum x; BNum y ] -> (
          let i1 = big_int_of_string x in
          let i2 = big_int_of_string y in
          let d = Big_int.sub_big_int i1 i2 in
          match
            build_prim_literal (Int_typ Bits256) (Big_int.string_of_big_int d)
          with
          | Some l -> pure l
          | None ->
              fail0
                ~kind:"Unable to express result of BNum subtraction in Int256"
                ?inst:None)
      | _ -> builtin_fail "BNum.bsub" ls
  end

  (***********************************************************)
  (******************** Crypto Builtins *************************)
  (***********************************************************)
  module EvalCryptoBuiltins = struct
    include CryptoBuiltins
    open Cryptokit
    open Datatypes.DataTypeDictionary
    open Scilla_crypto.Schnorr

    let eq _ ls _ =
      match ls with
      | [ ByStrX bs1; ByStrX bs2 ] ->
          pure @@ build_bool_lit (Bystrx.equal bs1 bs2)
      | [ ByStr bs1; ByStr bs2 ] -> pure @@ build_bool_lit (Bystr.equal bs1 bs2)
      | _ -> builtin_fail "Crypto.eq" ls

    (* Hash raw bytes / binary string. *)
    let sha256_hasher s = hash_string (Hash.sha2 256) s

    (* Keccak256 hash raw bytes / binary string. *)
    let keccak256_hasher s = hash_string (Hash.keccak 256) s

    (* Ripemd hash raw bytes/ binary string. *)
    let ripemd160_hasher s = hash_string (Hash.ripemd160 ()) s

    let hash_helper hasher name len ls =
      match ls with
      | [ l ] -> (
          let lhash = hasher (serialize_literal l) in
          match Bystrx.of_raw_bytes len lhash with
          | Some bs -> pure @@ ByStrX bs
          | None ->
              builtin_fail
                ("Crypto." ^ name ^ ": internal error, invalid hash")
                ls)
      | _ -> builtin_fail ("Crypto." ^ name) ls

    let sha256hash _ ls _ =
      hash_helper sha256_hasher "sha256hash" hash_length ls

    let keccak256hash _ ls _ =
      hash_helper keccak256_hasher "keccak256hash" hash_length ls

    let ripemd160hash _ ls _ =
      hash_helper ripemd160_hasher "ripemd160hash" Type.address_length ls

    let to_bystrx x _ ls _ =
      match ls with
      | [ ByStr bs ] -> (
          match Bystrx.of_raw_bytes x (Bystr.to_raw_bytes bs) with
          | Some l' -> pure @@ build_some_lit (ByStrX l') (bystrx_typ x)
          | None -> pure @@ build_none_lit (bystrx_typ x))
      | _ -> builtin_fail "Crypto.to_bystr" ls

    let to_bystr _ ls _ =
      match ls with
      | [ ByStrX bs ] -> pure @@ ByStr (Bystrx.to_bystr bs)
      | _ -> builtin_fail "Crypto.to_bystr" ls

    let substr _ ls _ =
      try
        match ls with
        | [ ByStr x; UintLit (Uint32L s); UintLit (Uint32L e) ] ->
            pure
            @@ ByStr (Bystr.sub x ~pos:(Uint32.to_int s) ~len:(Uint32.to_int e))
        | _ -> builtin_fail "Crypto.substr" ls
      with Invalid_argument msg -> builtin_fail ("Crypto.substr: " ^ msg) ls

    let to_uint x _ ls _ =
      let open Type.PrimType in
      let width_bytes = int_bit_width_to_int x / 8 in
      match ls with
      | [ ByStrX bs ] when Bystrx.width bs <= width_bytes ->
          (* of_bytes_big_endian functions expect 2^n number of bytes exactly *)
          let rem = width_bytes - Bystrx.width bs in
          let pad = Core_kernel.String.make rem '\000' in
          let bs_padded = Bytes.of_string (pad ^ Bystrx.to_raw_bytes bs) in
          let l =
            match x with
            | Bits32 -> Uint32L (Uint32.of_bytes_big_endian bs_padded 0)
            | Bits64 -> Uint64L (Uint64.of_bytes_big_endian bs_padded 0)
            | Bits128 -> Uint128L (Uint128.of_bytes_big_endian bs_padded 0)
            | Bits256 -> Uint256L (Uint256.of_bytes_big_endian bs_padded 0)
          in
          pure (UintLit l)
      | _ -> builtin_fail ("Crypto.to_uint" ^ int_bit_width_to_string x) ls

    let bech32_to_bystr20 _ ls _ =
      match ls with
      | [ StringLit prfx; StringLit addr ] -> (
          if Core_kernel.String.(prfx <> "zil") then
            fail0 ~kind:"Only zil bech32 addresses are supported" ?inst:None
          else
            match Bech32.decode_bech32_addr ~prfx ~addr with
            | Some bys20 -> (
                match Bystrx.of_raw_bytes 20 bys20 with
                | Some b ->
                    pure
                    @@ build_some_lit (ByStrX b)
                         (bystrx_typ Type.address_length)
                | None -> fail0 ~kind:"Invalid bech32 decode" ?inst:None)
            | None -> fail0 ~kind:"bech32 decoding failed" ?inst:None)
      | _ -> builtin_fail "Crypto.bech32_to_bystr20" ls

    let bystr20_to_bech32 _ ls _ =
      match ls with
      | [ StringLit prfx; ByStrX addr ] -> (
          if Core_kernel.String.(prfx <> "zil") then
            fail0 ~kind:"Only zil bech32 addresses are supported" ?inst:None
          else
            match
              Bech32.encode_bech32_addr ~prfx ~addr:(Bystrx.to_raw_bytes addr)
            with
            | Some bech32 ->
                pure @@ build_some_lit (StringLit bech32) string_typ
            | None -> fail0 ~kind:"bech32 encoding failed" ?inst:None)
      | _ -> builtin_fail "Crypto.bystr20_to_bech32" ls

    let concat _ ls _ =
      match ls with
      | [ ByStrX bs1; ByStrX bs2 ] -> pure @@ ByStrX (Bystrx.concat bs1 bs2)
      | [ ByStr bs1; ByStr bs2 ] -> pure @@ ByStr (Bystr.concat bs1 bs2)
      | _ -> builtin_fail "Crypto.concat" ls

    let[@warning "-32"] ec_gen_key_pair ls _ =
      match ls with
      | [] -> (
          match genKeyPair () with
          | Some (privK, pubK) -> (
              let privK_lit_o = Bystrx.of_raw_bytes privkey_len privK in
              let pubK_lit_o = Bystrx.of_raw_bytes pubkey_len pubK in
              match (privK_lit_o, pubK_lit_o) with
              | Some privK', Some pubK' ->
                  pure
                  @@ build_pair_lit (ByStrX privK') (bystrx_typ privkey_len)
                       (ByStrX pubK') (bystrx_typ pubkey_len)
              | _ ->
                  builtin_fail
                    "ec_gen_key_pair: internal error, invalid private/public \
                     key(s)."
                    ls)
          | None -> builtin_fail "ec_gen_key_pair: internal error." ls)
      | _ -> builtin_fail "ec_gen_key_pair" ls

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

    let schnorr_verify _ ls _ =
      match ls with
      | [ ByStrX pubkey; ByStr msg; ByStrX signature ]
        when Bystrx.width pubkey = pubkey_len
             && Bystrx.width signature = signature_len -> (
          match
            verify
              (Bystrx.to_raw_bytes pubkey)
              (Bystr.to_raw_bytes msg)
              (Bystrx.to_raw_bytes signature)
          with
          | Some v -> pure @@ build_bool_lit v
          | None -> builtin_fail "schnorr_verify: internal error" ls)
      | _ -> builtin_fail "schnorr_verify" ls

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

    let ecdsa_verify _ ls _ =
      let open Secp256k1Wrapper in
      match ls with
      | [ ByStrX pubkey; ByStr msg; ByStrX signature ]
        when Bystrx.width signature = signature_len
             && Bystrx.width pubkey = pubkey_len ->
          let%bind v =
            verify
              (Bystrx.to_raw_bytes pubkey)
              (Bystr.to_raw_bytes msg)
              (Bystrx.to_raw_bytes signature)
          in
          pure @@ build_bool_lit v
      | _ -> builtin_fail "ecdsa_verify" ls

    let ecdsa_recover_pk _ ls _ =
      let open Secp256k1Wrapper in
      match ls with
      | [ ByStr msg; ByStrX signature; UintLit (Uint32L recid) ]
        when Bystrx.width signature = signature_len -> (
          let%bind pk =
            recover_pk (Bystr.to_raw_bytes msg)
              (Bystrx.to_raw_bytes signature)
              (Stdint.Uint32.to_int recid)
          in
          match Bystrx.of_raw_bytes uncompressed_pubkey_len pk with
          | Some pk' -> pure (ByStrX pk')
          | None -> builtin_fail "ecdsa_recover_pk: Internal error." ls)
      | _ -> builtin_fail "ecdsa_recover_pk" ls

    let schnorr_get_address _ ls _ =
      match ls with
      | [ ByStrX pubkey ] when Bystrx.width pubkey = pubkey_len -> (
          let pks = Bystrx.to_raw_bytes pubkey in
          (* Hash the public key *)
          let pkh = sha256_hasher pks in
          (* and extract the least significant 20 bytes. *)
          let addr = Core_kernel.String.suffix pkh 20 in
          match Bystrx.of_raw_bytes Type.address_length addr with
          | Some bs -> pure @@ ByStrX bs
          | None -> builtin_fail "schnorr_get_address: Internal error." ls)
      | _ -> builtin_fail "schnorr_get_address" ls

    open Datatypes.SnarkTypes
    open Scilla_crypto

    let alt_bn128_G1_add _ ls _ =
      match ls with
      | [ p1; p2 ] -> (
          let%bind p1' = scilla_g1point_to_ocaml p1 in
          let%bind p2' = scilla_g1point_to_ocaml p2 in
          match Snark.alt_bn128_G1_add p1' p2' with
          | None -> pure @@ build_none_lit g1point_type
          | Some pr ->
              let%bind pr' = ocaml_g1point_to_scilla_lit pr in
              pure @@ build_some_lit pr' g1point_type)
      | _ -> builtin_fail "Crypto.alt_bn128_G1_add" ls

    let alt_bn128_G1_mul _ ls _ =
      match ls with
      | [ p1; s ] -> (
          let%bind p1' = scilla_g1point_to_ocaml p1 in
          let%bind s' = scilla_scalar_to_ocaml s in
          match Snark.alt_bn128_G1_mul p1' s' with
          | None -> pure @@ build_none_lit g1point_type
          | Some pr ->
              let%bind pr' = ocaml_g1point_to_scilla_lit pr in
              pure @@ build_some_lit pr' g1point_type)
      | _ -> builtin_fail "Crypto.alt_bn128_G1_mul" ls

    let alt_bn128_G1_neg _ ls _ =
      match ls with
      | [ p1 ] -> (
          let%bind p1' = scilla_g1point_to_ocaml p1 in
          match Snark.alt_bn128_G1_neg p1' with
          | None -> pure @@ build_none_lit g1point_type
          | Some pr ->
              let%bind pr' = ocaml_g1point_to_scilla_lit pr in
              pure @@ build_some_lit pr' g1point_type)
      | _ -> builtin_fail "Crypto.alt_bn128_G1_neg" ls

    let alt_bn128_pairing_product _ ls _ =
      match ls with
      | [ pairs ] -> (
          let%bind pairs' = scilla_g1g2pairlist_to_ocaml pairs in
          match Snark.alt_bn128_pairing_product pairs' with
          | None -> pure @@ build_none_lit bool_typ
          | Some b -> pure @@ build_some_lit (build_bool_lit b) bool_typ)
      | _ -> builtin_fail "Crypto.alt_bn128_G1_mul" ls
  end

  (***********************************************************)
  (*                       Maps                              *)
  (***********************************************************)
  module EvalMapBuiltins = struct
    include MapBuiltins
    open Datatypes.DataTypeDictionary

    let contains_elab sc targs ts =
      match (targs, ts) with
      | [], [ MapType (kt, vt); _u ] when is_address_type kt ->
          (* Special case - need to treat kt as a ByStr20 *)
          elab_tfun_with_args_no_gas sc [ bystrx_typ Type.address_length; vt ]
      | _, _ -> MapBuiltins.contains_elab sc targs ts

    let contains _ ls _ =
      match ls with
      | [ Map (_, entries); key ] ->
          let res = Caml.Hashtbl.mem entries key in
          pure @@ build_bool_lit res
      | _ -> builtin_fail "Map.contains" ls

    let put_elab sc targs ts =
      match (targs, ts) with
      | [], [ MapType (kt, vt); _kt'; _vt' ]
        when is_address_type kt || is_address_type vt ->
          (* Special case - need to treat kt and vt as a ByStr20 *)
          let new_kt =
            if is_address_type kt then bystrx_typ Type.address_length else kt
          in
          let new_vt =
            if is_address_type vt then bystrx_typ Type.address_length else vt
          in
          elab_tfun_with_args_no_gas sc [ new_kt; new_vt ]
      | _, _ -> MapBuiltins.put_elab sc targs ts

    let put _ ls _ =
      match ls with
      | [ Map (tm, entries); key; value ] ->
          (* Scilla semantics is not in-place modification. *)
          let entries' = Caml.Hashtbl.copy entries in
          let _ = Caml.Hashtbl.replace entries' key value in
          pure @@ Map (tm, entries')
      | _ -> builtin_fail "Map.put" ls

    let get_elab sc targs ts =
      match (targs, ts) with
      | [], [ MapType (kt, vt); _kt' ] when is_address_type kt ->
          (* Special case - need to treat kt and vt as a ByStr20 *)
          elab_tfun_with_args_no_gas sc [ bystrx_typ Type.address_length; vt ]
      | _, _ -> MapBuiltins.get_elab sc targs ts

    (* Notice that get passes return type *)
    let get _ ls rt =
      match (ls, rt) with
      | [ Map (_, entries); key ], ADT (tname, [ targ ])
        when Datatypes.is_option_adt_name (BIIdentifier.get_id tname) -> (
          let res = Caml.Hashtbl.find_opt entries key in
          match res with
          | None -> pure @@ build_none_lit targ
          | Some v -> pure @@ build_some_lit v targ)
      | _ -> builtin_fail "Map.get" ls

    let remove_elab sc targs ts =
      match (targs, ts) with
      | [], [ MapType (kt, vt); _u ] when is_address_type kt ->
          (* Special case - need to treat kt and vt as a ByStr20 *)
          elab_tfun_with_args_no_gas sc [ bystrx_typ Type.address_length; vt ]
      | _, _ -> MapBuiltins.remove_elab sc targs ts

    let remove _ ls _ =
      match ls with
      | [ Map (tm, entries); key ] ->
          (* Scilla semantics is not in-place modification. *)
          let entries' = Caml.Hashtbl.copy entries in
          let _ = Caml.Hashtbl.remove entries' key in
          pure @@ Map (tm, entries')
      | _ -> builtin_fail "Map.remove" ls

    let to_list _ ls _ =
      match ls with
      | [ Map ((kt, vt), entries) ] ->
          (* The type of the output list will be "Pair (kt) (vt)" *)
          let otyp = pair_typ kt vt in
          let nil = build_nil_lit otyp in
          let ol =
            let kvl' =
              (* Sort map entries in reverse order, so that when we
               * build the Scilla list next, it'll be in ascending order. *)
              Caml.List.sort
                (fun (k1, _) (k2, _) ->
                  String.descending (serialize_literal k1)
                    (serialize_literal k2))
                (Caml.List.of_seq @@ Caml.Hashtbl.to_seq entries)
            in
            Caml.List.fold_left
              (fun accum (k, v) ->
                let kv = build_pair_lit k kt v vt in
                let kvl = build_cons_lit kv otyp accum in
                kvl)
              nil kvl'
          in
          pure ol
      | _ -> builtin_fail "Map.to_list" ls

    let size _ ls _ =
      match ls with
      | [ Map (_, entries) ] ->
          (* The type of the output will be "Uint32" *)
          let ol = Caml.Hashtbl.length entries in
          pure (UintLit (Uint32L (Stdint.Uint32.of_int ol)))
      | _ -> builtin_fail "Map.size" ls
  end

  module EvalBuiltInDictionary = struct
    (* Elaborates the operation type based on the arguments types *)
    type elaborator = BaseBuiltins.elaborator

    (* Takes the expected type as an argument to elaborate the result *)
    type ('a, 'b) built_in_executor =
      BIType.t list ->
      (* type arguments *)
      BILiteral.t list ->
      (* value arguments *)
      BIType.t ->
      (* result type *)
      (BILiteral.t, scilla_error list) result

    (* A built-in record type:
       * arity
       * full, unelaborated type
       * elaborator, refining the type based on argument
         to support polymorphism -- e.g., for ints and maps
       * executor - operational semantics of the built-in
    *)
    type ('a, 'b) eval_built_in_record =
      int * BIType.t * elaborator * ('a, 'b) built_in_executor

    [@@@ocamlformat "disable"]

    (* All built-in evaluator functions *)
    let eval_built_in_multidict : builtin -> ('a, 'b) eval_built_in_record list = function
      (* Polymorphic builtins *)
      | Builtin_eq -> [EvalStringBuiltins.eq_arity, EvalStringBuiltins.eq_type, elab_id, EvalStringBuiltins.eq;
                       EvalBNumBuiltins.eq_arity, EvalBNumBuiltins.eq_type, elab_id , EvalBNumBuiltins.eq;
                       EvalCryptoBuiltins.eq_arity, EvalCryptoBuiltins.eq_type, EvalCryptoBuiltins.eq_elab, EvalCryptoBuiltins.eq;
                       EvalIntBuiltins.eq_arity, EvalIntBuiltins.eq_type, EvalIntBuiltins.eq_elab, EvalIntBuiltins.eq;
                       EvalUintBuiltins.eq_arity, EvalUintBuiltins.eq_type, EvalUintBuiltins.eq_elab, EvalUintBuiltins.eq]
      | Builtin_concat -> [EvalStringBuiltins.concat_arity, EvalStringBuiltins.concat_type, elab_id, EvalStringBuiltins.concat;
                           EvalCryptoBuiltins.concat_arity, EvalCryptoBuiltins.concat_type, EvalCryptoBuiltins.concat_elab, EvalCryptoBuiltins.concat]
      | Builtin_substr -> [EvalStringBuiltins.substr_arity, EvalStringBuiltins.substr_type, elab_id, EvalStringBuiltins.substr;
                           EvalCryptoBuiltins.substr_arity, EvalCryptoBuiltins.substr_type, elab_id, EvalCryptoBuiltins.substr
                          ]
      | Builtin_strlen -> [EvalStringBuiltins.strlen_arity, EvalStringBuiltins.strlen_type, EvalStringBuiltins.strlen_elab, EvalStringBuiltins.strlen]
      | Builtin_to_string -> [EvalStringBuiltins.to_string_arity, EvalStringBuiltins.to_string_type, EvalStringBuiltins.to_string_elab, EvalStringBuiltins.to_string]
      | Builtin_to_ascii -> [EvalStringBuiltins.to_ascii_arity, EvalStringBuiltins.to_ascii_type, EvalStringBuiltins.to_ascii_elab, EvalStringBuiltins.to_ascii]
      | Builtin_strrev -> [ EvalStringBuiltins.strrev_arity, EvalStringBuiltins.strrev_type, EvalStringBuiltins.strrev_elab, EvalStringBuiltins.strrev ]
      | Builtin_to_bystrx i -> [
          EvalCryptoBuiltins.to_bystrx_arity, EvalCryptoBuiltins.to_bystrx_type i, elab_id, EvalCryptoBuiltins.to_bystrx i;
          EvalUintBuiltins.to_bystrx_arity, EvalUintBuiltins.to_bystrx_type, EvalUintBuiltins.to_bystrx_elab i, EvalUintBuiltins.to_bystrx
        ]
    
      (* Block numbers *)
      | Builtin_blt -> [EvalBNumBuiltins.blt_arity, EvalBNumBuiltins.blt_type, elab_id, EvalBNumBuiltins.blt]
      | Builtin_badd -> [EvalBNumBuiltins.badd_arity, EvalBNumBuiltins.badd_type, EvalBNumBuiltins.badd_elab, EvalBNumBuiltins.badd]
      | Builtin_bsub -> [EvalBNumBuiltins.bsub_arity, EvalBNumBuiltins.bsub_type, EvalBNumBuiltins.bsub_elab, EvalBNumBuiltins.bsub]
    
      (* EvalCryptoBuiltins *)
      | Builtin_sha256hash -> [EvalCryptoBuiltins.hash_arity, EvalCryptoBuiltins.hash_type,EvalCryptoBuiltins.hash_elab, EvalCryptoBuiltins.sha256hash]
      | Builtin_keccak256hash -> [EvalCryptoBuiltins.hash_arity, EvalCryptoBuiltins.hash_type,EvalCryptoBuiltins.hash_elab, EvalCryptoBuiltins.keccak256hash]
      | Builtin_ripemd160hash -> [EvalCryptoBuiltins.hash_arity, EvalCryptoBuiltins.ripemd160hash_type,EvalCryptoBuiltins.hash_elab, EvalCryptoBuiltins.ripemd160hash]
      | Builtin_to_bystr -> [EvalCryptoBuiltins.to_bystr_arity, EvalCryptoBuiltins.to_bystr_type, EvalCryptoBuiltins.to_bystr_elab, EvalCryptoBuiltins.to_bystr]
      | Builtin_bech32_to_bystr20 -> [EvalCryptoBuiltins.bech32_to_bystr20_arity, EvalCryptoBuiltins.bech32_to_bystr20_type, elab_id, EvalCryptoBuiltins.bech32_to_bystr20]
      | Builtin_bystr20_to_bech32 -> [EvalCryptoBuiltins.bystr20_to_bech32_arity, EvalCryptoBuiltins.bystr20_to_bech32_type, elab_id, EvalCryptoBuiltins.bystr20_to_bech32]
      | Builtin_schnorr_verify -> [EvalCryptoBuiltins.schnorr_verify_arity, EvalCryptoBuiltins.schnorr_verify_type, elab_id, EvalCryptoBuiltins.schnorr_verify]
      | Builtin_ecdsa_verify -> [EvalCryptoBuiltins.ecdsa_verify_arity, EvalCryptoBuiltins.ecdsa_verify_type, elab_id, EvalCryptoBuiltins.ecdsa_verify]
      | Builtin_ecdsa_recover_pk -> [EvalCryptoBuiltins.ecdsa_recover_pk_arity, EvalCryptoBuiltins.ecdsa_recover_pk_type, elab_id, EvalCryptoBuiltins.ecdsa_recover_pk]
      | Builtin_schnorr_get_address -> [EvalCryptoBuiltins.schnorr_get_address_arity, EvalCryptoBuiltins.schnorr_get_address_type, elab_id, EvalCryptoBuiltins.schnorr_get_address]
      | Builtin_alt_bn128_G1_add -> [EvalCryptoBuiltins.alt_bn128_G1_add_arity, EvalCryptoBuiltins.alt_bn128_G1_add_type, elab_id, EvalCryptoBuiltins.alt_bn128_G1_add]
      | Builtin_alt_bn128_G1_mul -> [EvalCryptoBuiltins.alt_bn128_G1_mul_arity, EvalCryptoBuiltins.alt_bn128_G1_mul_type, elab_id, EvalCryptoBuiltins.alt_bn128_G1_mul]
      | Builtin_alt_bn128_G1_neg -> [EvalCryptoBuiltins.alt_bn128_G1_neg_arity, EvalCryptoBuiltins.alt_bn128_G1_neg_type, elab_id, EvalCryptoBuiltins.alt_bn128_G1_neg]
      | Builtin_alt_bn128_pairing_product -> [EvalCryptoBuiltins.alt_bn128_pairing_product_arity, 
                                              EvalCryptoBuiltins.alt_bn128_pairing_product_type, elab_id, EvalCryptoBuiltins.alt_bn128_pairing_product]
    
      (* Maps *)
      | Builtin_contains -> [EvalMapBuiltins.contains_arity, EvalMapBuiltins.contains_type, EvalMapBuiltins.contains_elab, EvalMapBuiltins.contains]
      | Builtin_put -> [EvalMapBuiltins.put_arity, EvalMapBuiltins.put_type, EvalMapBuiltins.put_elab, EvalMapBuiltins.put]
      | Builtin_get -> [EvalMapBuiltins.get_arity, EvalMapBuiltins.get_type, EvalMapBuiltins.get_elab, EvalMapBuiltins.get]
      | Builtin_remove -> [EvalMapBuiltins.remove_arity, EvalMapBuiltins.remove_type, EvalMapBuiltins.remove_elab, EvalMapBuiltins.remove]
      | Builtin_to_list -> [EvalMapBuiltins.to_list_arity, EvalMapBuiltins.to_list_type, EvalMapBuiltins.to_list_elab, EvalMapBuiltins.to_list]
      | Builtin_size -> [EvalMapBuiltins.size_arity, EvalMapBuiltins.size_type, EvalMapBuiltins.size_elab, EvalMapBuiltins.size]
    
      (* Integers *)
      | Builtin_lt -> [EvalIntBuiltins.eq_arity, EvalIntBuiltins.eq_type, EvalIntBuiltins.eq_elab, EvalIntBuiltins.lt;
                       EvalUintBuiltins.eq_arity, EvalUintBuiltins.eq_type, EvalUintBuiltins.eq_elab, EvalUintBuiltins.lt]
      | Builtin_add -> [EvalIntBuiltins.binop_arity, EvalIntBuiltins.binop_type, EvalIntBuiltins.binop_elab, EvalIntBuiltins.add;
                        EvalUintBuiltins.binop_arity, EvalUintBuiltins.binop_type, EvalUintBuiltins.binop_elab, EvalUintBuiltins.add]
      | Builtin_sub -> [EvalIntBuiltins.binop_arity, EvalIntBuiltins.binop_type, EvalIntBuiltins.binop_elab, EvalIntBuiltins.sub;
                        EvalUintBuiltins.binop_arity, EvalUintBuiltins.binop_type, EvalUintBuiltins.binop_elab, EvalUintBuiltins.sub]
      | Builtin_mul -> [EvalIntBuiltins.binop_arity, EvalIntBuiltins.binop_type, EvalIntBuiltins.binop_elab, EvalIntBuiltins.mul;
                        EvalUintBuiltins.binop_arity, EvalUintBuiltins.binop_type, EvalUintBuiltins.binop_elab, EvalUintBuiltins.mul]
      | Builtin_div -> [EvalIntBuiltins.binop_arity, EvalIntBuiltins.binop_type, EvalIntBuiltins.binop_elab, EvalIntBuiltins.div;
                        EvalUintBuiltins.binop_arity, EvalUintBuiltins.binop_type, EvalUintBuiltins.binop_elab, EvalUintBuiltins.div]
      | Builtin_rem -> [EvalIntBuiltins.binop_arity, EvalIntBuiltins.binop_type, EvalIntBuiltins.binop_elab, EvalIntBuiltins.rem;
                        EvalUintBuiltins.binop_arity, EvalUintBuiltins.binop_type, EvalUintBuiltins.binop_elab, EvalUintBuiltins.rem]
      | Builtin_pow -> [EvalIntBuiltins.pow_arity, EvalIntBuiltins.pow_type, EvalIntBuiltins.pow_elab, EvalIntBuiltins.pow;
                        EvalUintBuiltins.pow_arity, EvalUintBuiltins.pow_type, EvalUintBuiltins.pow_elab, EvalUintBuiltins.pow]
      | Builtin_isqrt -> [EvalUintBuiltins.isqrt_arity, EvalUintBuiltins.isqrt_type, EvalUintBuiltins.isqrt_elab, EvalUintBuiltins.isqrt]
    
      (* Signed integers specific builtins *)
      | Builtin_to_int32 -> [EvalIntBuiltins.to_int_arity, EvalIntBuiltins.to_int_type, EvalIntBuiltins.to_int_elab Bits32, EvalIntBuiltins.to_int32]
      | Builtin_to_int64 -> [EvalIntBuiltins.to_int_arity, EvalIntBuiltins.to_int_type, EvalIntBuiltins.to_int_elab Bits64, EvalIntBuiltins.to_int64]
      | Builtin_to_int128 -> [EvalIntBuiltins.to_int_arity, EvalIntBuiltins.to_int_type, EvalIntBuiltins.to_int_elab Bits128, EvalIntBuiltins.to_int128]
      | Builtin_to_int256 -> [EvalIntBuiltins.to_int_arity, EvalIntBuiltins.to_int_type, EvalIntBuiltins.to_int_elab Bits256, EvalIntBuiltins.to_int256]
    
      (* Unsigned integers specific builtins *)
      | Builtin_to_uint32 -> [
          EvalCryptoBuiltins.to_uint_arity, EvalCryptoBuiltins.to_uint_type Bits32, EvalCryptoBuiltins.to_uint_elab Bits32, EvalCryptoBuiltins.to_uint Bits32;
          EvalUintBuiltins.to_uint_arity, EvalUintBuiltins.to_uint_type, EvalUintBuiltins.to_uint_elab Bits32, EvalUintBuiltins.to_uint32
        ]
      | Builtin_to_uint64 -> [
          EvalCryptoBuiltins.to_uint_arity, EvalCryptoBuiltins.to_uint_type Bits64, EvalCryptoBuiltins.to_uint_elab Bits64, EvalCryptoBuiltins.to_uint Bits64;
          EvalUintBuiltins.to_uint_arity, EvalUintBuiltins.to_uint_type, EvalUintBuiltins.to_uint_elab Bits64, EvalUintBuiltins.to_uint64
        ]
      | Builtin_to_uint128 -> [
          EvalCryptoBuiltins.to_uint_arity, EvalCryptoBuiltins.to_uint_type Bits128, EvalCryptoBuiltins.to_uint_elab Bits128, EvalCryptoBuiltins.to_uint Bits128;
          EvalUintBuiltins.to_uint_arity, EvalUintBuiltins.to_uint_type, EvalUintBuiltins.to_uint_elab Bits128, EvalUintBuiltins.to_uint128
        ]
      | Builtin_to_uint256 -> [
          EvalCryptoBuiltins.to_uint_arity, EvalCryptoBuiltins.to_uint_type Bits256, EvalCryptoBuiltins.to_uint_elab Bits256, EvalCryptoBuiltins.to_uint Bits256;
          EvalUintBuiltins.to_uint_arity, EvalUintBuiltins.to_uint_type, EvalUintBuiltins.to_uint_elab Bits256, EvalUintBuiltins.to_uint256
        ]
      | Builtin_to_nat -> [EvalUintBuiltins.to_nat_arity, EvalUintBuiltins.to_nat_type, elab_id, EvalUintBuiltins.to_nat]

    [@@@ocamlformat "enable"]

    (* Dictionary lookup based on the operation name and type *)
    let find_builtin_op (op, rep) ~targtypes ~vargtypes =
      let finder = function
        | arity, optype, elab, exec ->
            if arity = List.length vargtypes then
              (* First: elaborate based on argument types *)
              let%bind type_elab = elab optype targtypes vargtypes in
              (* Second: check applicability *)
              let%bind res_type =
                fun_type_applies type_elab vargtypes ~lc:(ER.get_loc rep)
              in
              pure (res_type, exec)
            else fail0 ~kind:"Name or arity don't match" ?inst:None
      in
      let dict = eval_built_in_multidict op in
      let%bind _, (res_type, exec) =
        tryM dict ~f:finder ~msg:(fun () ->
            mk_error1
              ~kind:(sprintf "Type error: cannot apply \"%s\" built-in to argument(s) of type(s)" (pp_builtin op))
              ~inst:
                 (sprintf "%s %s"
                    (if List.is_empty targtypes then ""
                      else sprintf "{%s} " (pp_typ_list_error targtypes))
                    (pp_typ_list_error vargtypes))
              (ER.get_loc rep))
      in
      pure (res_type, exec)
  end
end
