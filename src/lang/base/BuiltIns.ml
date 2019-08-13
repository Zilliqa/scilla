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
open Core
open ErrorUtils
open MonadUtil
open Result.Let_syntax
open Big_int
open Stdint
open TypeUtil
open Integer256
open TypeUtilities

module UsefulLiterals = struct
  let true_lit = ADTValue ("True", [], [])
  let false_lit = ADTValue ("False", [], [])
  let to_Bool b = if b then true_lit else false_lit

  let some_lit l =
    let%bind t = literal_type l in
    pure @@ ADTValue ("Some", [t], [l])
  let none_lit t = ADTValue ("None", [t], [])

  let pair_lit l1 l2 =
    let%bind t1 = literal_type l1 in
    let%bind t2 = literal_type l2 in
    pure @@ ADTValue ("Pair", [t1;t2], [l1;l2])
end

module ScillaBuiltIns
    (SR : Rep)
    (ER : Rep) = struct

  let print_literal_list ls =
    PrettyPrinters.pp_literal_list ls

  let builtin_fail name ls =
    fail0 @@ sprintf "Cannot apply built-in %s to a list of arguments:%s."
      name (print_literal_list ls)

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

  (*******************************************************)
  (**************** String *******************************)
  (*******************************************************)

  (* String operations *)
  module String = struct
    open UsefulLiterals
    open PrimTypes
    open Datatypes.DataTypeDictionary

    (* let string_eq_type = FunType (string_typ, FunType (string_typ, )) *)

    let eq_arity = 2    
    let eq_type = fun_typ string_typ @@ fun_typ string_typ bool_typ
    let eq ls _ = match ls with
      | [StringLit x; StringLit y] ->
          pure @@ to_Bool (x = y)
      | _ -> builtin_fail "String.eq" ls

    let concat_arity = 2    
    let concat_type = fun_typ string_typ @@ fun_typ string_typ string_typ    
    let concat ls _ = match ls with
      | [StringLit x; StringLit y] ->
          pure @@ StringLit (x ^ y)
      | _ -> builtin_fail "String.concat" ls

    let substr_arity = 3    
    let substr_type = fun_typ string_typ @@ fun_typ uint32_typ @@ fun_typ uint32_typ string_typ
    let substr ls _ = match ls with
      | [StringLit x; UintLit (Uint32L s); UintLit (Uint32L e)] ->
        (try
          pure @@ StringLit (Core.String.sub x ~pos:(Uint32.to_int s) ~len:(Uint32.to_int e))
        with Invalid_argument msg -> builtin_fail ("String.substr: " ^ msg) ls)
      | _ -> builtin_fail "String.substr" ls

    let strlen_arity = 1
    let strlen_type = fun_typ string_typ uint32_typ
    let strlen ls _ = match ls with
      | [StringLit x] ->
        pure @@ UintLit (Uint32L (Uint32.of_int (String.length x)))
      | _ -> builtin_fail "String.strlen" ls

    let to_string_arity = 1
    let to_string_type = tfun_typ "'A" (fun_typ (tvar "'A") (string_typ))
    let to_string_elab _ ts = match ts with
      | [t] when is_int_type t || is_uint_type t ||
                 is_bystrx_type t || t = bystr_typ ->
          elab_tfun_with_args to_string_type ts
      | _ -> fail0 "Failed to elaborate"

    let to_string ls _ =
      let%bind s = match ls with
        | [IntLit x] -> pure @@ string_of_int_lit x
        | [UintLit x] -> pure @@ string_of_uint_lit x
        | [ByStr x] -> pure @@ Bystr.hex_encoding x
        | [ByStrX x] -> pure @@ Bystrx.hex_encoding x
        | _ -> builtin_fail (sprintf "String.to_string") ls
      in pure @@ StringLit s

  end

  (* Instantiating the functors *)
  open SafeArith

  module Int32_safe = SafeInt(Int32)
  module Int64_safe = SafeInt(Int64)
  module Int128_safe = SafeInt(Int128)
  module Int256_safe = SafeInt(Int256)

  module Uint32_safe = SafeUint(Uint32)
  module Uint64_safe = SafeUint(Uint64)
  module Uint128_safe = SafeUint(Uint128)
  module Uint256_safe = SafeUint(Uint256)

  (*******************************************************)
  (*******************************************************)

  (* Integer operations *)
  module Int = struct
    open UsefulLiterals
    open PrimTypes
    open Datatypes.DataTypeDictionary

    let eq_arity = 2
    let eq_type = tfun_typ "'A" (fun_typ (tvar "'A") @@ fun_typ (tvar "'A") bool_typ)
    let eq_elab t ts = match ts with
      | [i1; i2] when i1 = i2 && is_int_type i1 -> elab_tfun_with_args t [i1]
      | _ -> fail0 "Failed to elaborate"

    let binop_arity = 2
    let binop_type = tfun_typ "'A" (fun_typ (tvar "'A") @@ fun_typ (tvar "'A") (tvar "'A"))
    let binop_elab t ts = match ts with
      | [i1; i2] when i1 = i2 && is_int_type i1 -> elab_tfun_with_args t [i1]
      | _ -> fail0 "Failed to elaborate"

    let eq ls _ = match ls with
      | [IntLit (Int32L _) as x; IntLit (Int32L _) as y]
      | [IntLit (Int64L _) as x; IntLit (Int64L _) as y]
      | [IntLit (Int128L _) as x; IntLit (Int128L _) as y]
      | [IntLit (Int256L _) as x; IntLit (Int256L _) as y] ->
        pure @@ to_Bool (x = y)
      | _ -> builtin_fail "Int.eq: unsupported types" ls

    let add ls _ = 
      try 
        let%bind l = (match ls with
          | [IntLit (Int32L x); IntLit (Int32L y)] ->
              pure @@ Int32L(Int32_safe.add x y)
          | [IntLit (Int64L x); IntLit (Int64L y)] ->
              pure @@ Int64L(Int64_safe.add x y)
          | [IntLit (Int128L x); IntLit (Int128L y)] ->
              pure @@ Int128L(Int128_safe.add x y)
          | [IntLit (Int256L x); IntLit (Int256L y)] ->
              pure @@ Int256L(Int256_safe.add x y)
          | _ -> builtin_fail "Int.add: unsupported types" ls)
        in pure @@ IntLit l
      with | IntOverflow | IntUnderflow ->
        builtin_fail "Int.add: an overflow/underflow occurred" ls

    let sub ls _ = 
      try 
        let%bind l =(match ls with
          | [IntLit (Int32L x); IntLit (Int32L y)] ->
              pure @@ Int32L(Int32_safe.sub x y)
          | [IntLit (Int64L x); IntLit (Int64L y)] ->
              pure @@ Int64L(Int64_safe.sub x y)
          | [IntLit (Int128L x); IntLit (Int128L y)] ->
              pure @@ Int128L (Int128_safe.sub x y)
          | [IntLit (Int256L x); IntLit (Int256L y)] ->
              pure @@ Int256L (Int256_safe.sub x y)
          | _ -> builtin_fail "Int.sub: unsupported types" ls)
        in pure @@ IntLit l
      with | IntOverflow | IntUnderflow ->
        builtin_fail "Int.sub: an overflow/underflow occurred" ls

    let mul ls _ = 
      try 
        let%bind l = (match ls with
          | [IntLit (Int32L x); IntLit (Int32L y)] ->
              pure @@ Int32L(Int32_safe.mul x y)
          | [IntLit (Int64L x); IntLit (Int64L y)] ->
              pure @@ Int64L(Int64_safe.mul x y)
          | [IntLit (Int128L x); IntLit (Int128L y)] ->
              pure @@ Int128L(Int128_safe.mul x y)
          | [IntLit (Int256L x); IntLit (Int256L y)] ->
              pure @@ Int256L(Int256_safe.mul x y)
          | _ -> builtin_fail "Int.mul: unsupported types" ls)
        in pure @@ IntLit l
      with | IntOverflow | IntUnderflow ->
        builtin_fail "Int.mul: an overflow/underflow occurred" ls

    let div ls _ = 
      try 
        let%bind l = (match ls with
          | [IntLit (Int32L x); IntLit (Int32L y)] ->
              pure @@ Int32L(Int32_safe.div x y)
          | [IntLit (Int64L x); IntLit (Int64L y)] ->
              pure @@ Int64L(Int64_safe.div x y)
          | [IntLit (Int128L x); IntLit (Int128L y)] ->
              pure @@ Int128L(Int128_safe.div x y)
          | [IntLit (Int256L x); IntLit (Int256L y)] ->
              pure @@ Int256L(Int256_safe.div x y)
          | _ -> builtin_fail "Int.div: unsupported types" ls)
        in pure @@ IntLit l
        with | Division_by_zero | IntOverflow ->
          builtin_fail "Int.div: Division by zero / IntOverflow error occurred" ls

    let rem ls  _ =
      try 
        let%bind l = (match ls with
          | [IntLit (Int32L x); IntLit (Int32L y)] ->
              pure @@ Int32L(Int32_safe.rem x y)
          | [IntLit (Int64L x); IntLit (Int64L y)] ->
              pure @@ Int64L(Int64_safe.rem x y)
          | [IntLit (Int128L x); IntLit (Int128L y)] ->
              pure @@ Int128L(Int128_safe.rem x y)
          | [IntLit (Int256L x); IntLit (Int256L y)] ->
              pure @@ Int256L(Int256_safe.rem x y)
          | _ -> builtin_fail "Int.rem: unsupported types" ls)
        in pure @@ IntLit l
      with | Division_by_zero ->
               builtin_fail "Int.rem: Division by zero error occurred" ls

    let pow_arity = 2
    let pow_type = tfun_typ "'A" (fun_typ (tvar "'A") @@ fun_typ (uint32_typ) (tvar "'A"))
    let pow_elab t ts = match ts with
    | [i1; i2] when is_int_type i1 && (i2 = uint32_typ) ->
       elab_tfun_with_args t [i1]
    | _ -> fail0 "Failed to elaborate"
    let pow ls _ = 
    try 
      let%bind l = (match ls with
        | [IntLit (Int32L x); UintLit (Uint32L y)] ->
            pure @@ Int32L(Int32_safe.pow x y)
        | [IntLit (Int64L x); UintLit (Uint32L y)] ->
            pure @@ Int64L(Int64_safe.pow x y)
        | [IntLit (Int128L x); UintLit (Uint32L y)] ->
            pure @@ Int128L(Int128_safe.pow x y)
        | [IntLit (Int256L x); UintLit (Uint32L y)] ->
            pure @@ Int256L(Int256_safe.pow x y)
        | _ -> builtin_fail "Int.pow: unsupported types" ls)
      in pure @@ IntLit l
    with | IntOverflow | IntUnderflow ->
      builtin_fail "Int.pow: an overflow/underflow occurred" ls

    let lt ls _ =
      try 
        (match ls with
          | [IntLit (Int32L x); IntLit (Int32L y)] ->
              pure @@ to_Bool (Int32_safe.lt x y)
          | [IntLit (Int64L x); IntLit (Int64L y)] ->
              pure @@ to_Bool (Int64_safe.lt x y)
          | [IntLit (Int128L x); IntLit (Int128L y)] ->
              pure @@ to_Bool (Int128_safe.lt x y)
          | [IntLit (Int256L x); IntLit (Int256L y)] ->
              pure @@ to_Bool (Int256_safe.lt x y)
          | _ -> builtin_fail "Int.lt: unsupported types" ls)
        with | IntOverflow | IntUnderflow ->
          builtin_fail "Int.lt: an overflow/underflow occurred" ls

    let to_int_arity = 1
    let to_int_type = tfun_typ "'A" @@ tfun_typ "'B" (fun_typ (tvar "'A") (option_typ (tvar "'B")))
    let to_int_elab w sc ts = match ts with
      | [PrimType (Int_typ _) as t] | [PrimType (Uint_typ _) as t]
      | [PrimType String_typ as t] ->
          let ityp = PrimType (Int_typ w) in
          elab_tfun_with_args sc [t; ityp]
      | _ -> fail0 "Failed to elaborate"

    let to_int_helper ls w =
      let%bind xs = match ls with
        | [IntLit x] -> pure @@ string_of_int_lit x
        | [UintLit x] -> pure @@ string_of_uint_lit x
        | [StringLit x] -> pure x
        | _ -> builtin_fail ("Int.to_int" ^ int_bit_width_to_string w) ls
      in
        let iptyp = Int_typ w in
        (match build_prim_literal iptyp xs with
        | Some lit ->
          pure (ADTValue ("Some", [PrimType iptyp], [lit]))
        | None ->
          pure (ADTValue ("None", [PrimType iptyp], [])))

    let to_int32 ls _ = to_int_helper ls Bits32
    let to_int64 ls _ = to_int_helper ls Bits64
    let to_int128 ls _ = to_int_helper ls Bits128
    let to_int256 ls _ = to_int_helper ls Bits256

  end

  (* Unsigned integer operation *)
  module Uint = struct
    open UsefulLiterals
    open PrimTypes
    open Datatypes.DataTypeDictionary

    let eq_arity = 2
    let eq_type = tfun_typ "'A" (fun_typ (tvar "'A") @@ fun_typ (tvar "'A") bool_typ)
    let eq_elab sc ts = match ts with
      | [i1; i2] when i1 = i2 && is_uint_type i1 -> elab_tfun_with_args sc [i1]
      | _ -> fail0 "Failed to elaborate"

    let binop_arity = 2
    let binop_type = tfun_typ "'A" (fun_typ (tvar "'A") @@ fun_typ (tvar "'A") (tvar "'A"))
    let binop_elab sc ts = match ts with
      | [i1; i2] when i1 = i2 && is_uint_type i1 -> elab_tfun_with_args sc [i1]
      | _ -> fail0 "Failed to elaborate"

    let eq ls _ = match ls with
      | [UintLit (Uint32L _) as x; UintLit (Uint32L _) as y]
      | [UintLit (Uint64L _) as x; UintLit (Uint64L _) as y]
      | [UintLit (Uint128L _) as x; UintLit (Uint128L _) as y]
      | [UintLit (Uint256L _) as x; UintLit (Uint256L _) as y] ->
        pure @@ to_Bool (x = y)
      | _ -> builtin_fail "Uint.eq: unsupported types" ls

    let add ls _ = 
      try 
        let%bind l = (match ls with
          | [UintLit (Uint32L x); UintLit (Uint32L y)] ->
              pure @@ Uint32L(Uint32_safe.add x y)
          | [UintLit (Uint64L x); UintLit (Uint64L y)] ->
              pure @@ Uint64L(Uint64_safe.add x y)
          | [UintLit (Uint128L x); UintLit (Uint128L y)] ->
              pure @@ Uint128L(Uint128_safe.add x y)
          | [UintLit (Uint256L x); UintLit (Uint256L y)] ->
              pure @@ Uint256L(Uint256_safe.add x y)
          | _ -> builtin_fail "Uint.add: unsupported types" ls)
        in pure @@ UintLit l
      with | IntOverflow | IntUnderflow ->
        builtin_fail "Uint.add: an overflow/underflow occurred" ls

    let sub ls _ = 
      try 
        let%bind l = (match ls with
          | [UintLit (Uint32L x); UintLit (Uint32L y)] ->
              pure @@ Uint32L(Uint32_safe.sub x y)
          | [UintLit (Uint64L x); UintLit (Uint64L y)] ->
              pure @@ Uint64L(Uint64_safe.sub x y)
          | [UintLit (Uint128L x); UintLit (Uint128L y)] ->
              pure @@ Uint128L(Uint128_safe.sub x y)
          | [UintLit (Uint256L x); UintLit (Uint256L y)] ->
              pure @@ Uint256L(Uint256_safe.sub x y)
          | _ -> builtin_fail "Uint.sub: unsupported types" ls)
        in pure @@ UintLit l
      with | IntOverflow | IntUnderflow ->
        builtin_fail "Uint.sub: an overflow/underflow occurred" ls

    let mul ls _ = 
      try 
        let%bind l = (match ls with
          | [UintLit (Uint32L x); UintLit (Uint32L y)] ->
              pure @@ Uint32L(Uint32_safe.mul x y)
          | [UintLit (Uint64L x); UintLit (Uint64L y)] ->
              pure @@ Uint64L(Uint64_safe.mul x y)
          | [UintLit (Uint128L x); UintLit (Uint128L y)] ->
              pure @@ Uint128L(Uint128_safe.mul x y)
          | [UintLit (Uint256L x); UintLit (Uint256L y)] ->
              pure @@ Uint256L(Uint256_safe.mul x y)
          | _ -> builtin_fail "Uint.mul: unsupported types" ls)
        in pure @@ UintLit l
      with | IntOverflow | IntUnderflow ->
        builtin_fail "Uint.mul: an overflow/underflow occurred" ls

    let div ls _ = 
      try 
        let%bind l = (match ls with
          | [UintLit (Uint32L x); UintLit (Uint32L y)] ->
              pure @@ Uint32L(Uint32_safe.div x y)
          | [UintLit (Uint64L x); UintLit (Uint64L y)] ->
              pure @@ Uint64L(Uint64_safe.div x y)
          | [UintLit (Uint128L x); UintLit (Uint128L y)] ->
              pure @@ Uint128L(Uint128_safe.div x y)
          | [UintLit (Uint256L x); UintLit (Uint256L y)] ->
              pure @@ Uint256L(Uint256_safe.div x y)
          | _ -> builtin_fail "Uint.div: unsupported types" ls)
        in pure @@ UintLit l
      with | Division_by_zero ->
        builtin_fail "Uint.div: Division by zero / UintOverflow error occurred" ls

    let rem ls  _ =
      try 
        let%bind l = (match ls with
          | [UintLit (Uint32L x); UintLit (Uint32L y)] ->
              pure @@ Uint32L(Uint32_safe.rem x y)
          | [UintLit (Uint64L x); UintLit (Uint64L y)] ->
              pure @@ Uint64L(Uint64_safe.rem x y)
          | [UintLit (Uint128L x); UintLit (Uint128L y)] ->
              pure @@ Uint128L(Uint128_safe.rem x y)
          | [UintLit (Uint256L x); UintLit (Uint256L y)] ->
              pure @@ Uint256L(Uint256_safe.rem x y)
          | _ -> builtin_fail "Uint.rem: unsupported types" ls)
        in pure @@ UintLit l
      with | Division_by_zero ->
        builtin_fail "Uint.rem: Division by zero error occurred" ls

    let pow_arity = 2
    let pow_type = tfun_typ "'A" (fun_typ (tvar "'A") @@ fun_typ (uint32_typ) (tvar "'A"))
    let pow_elab t ts = match ts with
    | [i1; i2] when is_uint_type i1 && (i2 = uint32_typ) ->
       elab_tfun_with_args t [i1]
    | _ -> fail0 "Failed to elaborate"
    let pow ls _ = 
      try 
        let%bind l = (match ls with
          | [UintLit (Uint32L x); UintLit (Uint32L y)] ->
              pure @@ Uint32L(Uint32_safe.pow x y)
          | [UintLit (Uint64L x); UintLit (Uint32L y)] ->
              pure @@ Uint64L(Uint64_safe.pow x y)
          | [UintLit (Uint128L x); UintLit (Uint32L y)] ->
              pure @@ Uint128L(Uint128_safe.pow x y)
          | [UintLit (Uint256L x); UintLit (Uint32L y)] ->
              pure @@ Uint256L(Uint256_safe.pow x y)
          | _ -> builtin_fail "Int.pow: unsupported types" ls)
        in pure @@ UintLit l
      with | IntOverflow | IntUnderflow ->
        builtin_fail "Int.pow: an overflow/underflow occurred" ls

    let lt ls _ =
      try 
        (match ls with
          | [UintLit (Uint32L x); UintLit (Uint32L y)] ->
              pure @@ to_Bool (Uint32_safe.lt x y)
          | [UintLit (Uint64L x); UintLit (Uint64L y)] ->
              pure @@ to_Bool (Uint64_safe.lt x y)
          | [UintLit (Uint128L x); UintLit (Uint128L y)] ->
              pure @@ to_Bool (Uint128_safe.lt x y)
          | [UintLit (Uint256L x); UintLit (Uint256L y)] ->
              pure @@ to_Bool (Uint256_safe.lt x y)
          | _ -> builtin_fail "Uint.lt: unsupported types" ls)
      with | IntOverflow | IntUnderflow ->
        builtin_fail "Uint.lt: an overflow/underflow occurred" ls

    let to_uint_arity = 1
    let to_uint_type = tfun_typ "'A" @@ tfun_typ "'B"
        (fun_typ (tvar "'A") (option_typ (tvar "'B")))

    let to_uint_elab w sc ts = match ts with
      | [PrimType (Int_typ _) as t] | [PrimType (Uint_typ _) as t]
      | [PrimType String_typ as t] ->
          let ityp = PrimType (Uint_typ w) in
          elab_tfun_with_args sc [t; ityp]
      | _ -> fail0 "Failed to elaborate"

    let to_uint_helper ls w = 
      let%bind xs = match ls with
        | [IntLit x] -> pure @@ string_of_int_lit x
        | [UintLit x] -> pure @@ string_of_uint_lit x
        | [StringLit x] -> pure x
        | _ -> builtin_fail ("UInt.to_uint" ^ int_bit_width_to_string w) ls

      in
        let iptyp = Uint_typ w in
        (match build_prim_literal iptyp xs with
        | Some lit ->
          pure (ADTValue ("Some", [PrimType iptyp], [lit]))
        | None ->
          pure (ADTValue ("None", [PrimType iptyp], [])))

    let to_uint32 ls _ = to_uint_helper ls Bits32
    let to_uint64 ls _ = to_uint_helper ls Bits64
    let to_uint128 ls _ = to_uint_helper ls Bits128
    let to_uint256 ls _ = to_uint_helper ls Bits256

    let to_nat_arity = 1
    let to_nat_type = tfun_typ "'A" @@ (fun_typ (tvar "'A") nat_typ)
    let to_nat_elab sc ts = match ts with
      | [t] when is_uint_type t ->
          elab_tfun_with_args sc [t]
      | _ -> fail0 "Failed to elaborate"

    let to_nat ls _ = match ls with
      | [UintLit (Uint32L n)] ->
        let rec nat_builder (i : Uint32.t) (acc : Syntax.literal) =
          if i = Uint32.zero then acc
          else nat_builder (Uint32.pred i) (ADTValue ("Succ", [], [acc]))
        in
        let zero = ADTValue ("Zero", [], []) in
        pure @@ nat_builder n zero
      (* Other integer widths can be in the library, using integer conversions. *)
      | _ -> builtin_fail "Uint.to_nat only supported for Uint32" ls

  end

  (***********************************************************)
  (* Working with block numbers *)
  (***********************************************************)
  module BNum = struct
    open UsefulLiterals
    open PrimTypes
    open Datatypes.DataTypeDictionary

    let eq_type = fun_typ bnum_typ @@ fun_typ bnum_typ bool_typ
    let eq_arity = 2    
    let eq ls _ = match ls with
      | [BNum x; BNum y] ->
          pure @@ to_Bool (x = y)
      | _ -> builtin_fail "BNum.eq" ls

    let blt_type = fun_typ bnum_typ @@ fun_typ bnum_typ bool_typ
    let blt_arity = 2    
    let blt ls _ = match ls with
      | [BNum x; BNum y] -> pure (
          let i1 = big_int_of_string x in
          let i2 = big_int_of_string y in
          to_Bool (lt_big_int i1 i2))
      | _ -> builtin_fail "BNum.blt" ls

    let badd_arity = 2    
    let badd_type =
      tfun_typ "'A" @@ tfun_typ "'B" @@
      (fun_typ (tvar "'A") @@ fun_typ (tvar "'B") bnum_typ)
    (* Elaborator to run with arbitrary uints *)
    let badd_elab sc ts = match ts with
      | [s; u] when s = bnum_typ && is_uint_type u ->
          elab_tfun_with_args sc ts
      | _ -> fail0 "Failed to elaborate"

    let badd ls _ = match ls with
      | [BNum x; UintLit y] ->
          let i1 = big_int_of_string x in
          let i2 = big_int_of_string (string_of_uint_lit y) in
          if ge_big_int i2 (big_int_of_int 0)
          then pure @@ BNum (string_of_big_int (add_big_int i1 i2))
          else fail0 @@ sprintf
              "Cannot add a negative value (%s) to a block." (string_of_uint_lit y)
      | _ -> builtin_fail "BNum.badd" ls

    let bsub_arity = 2    
    let bsub_type =
      tfun_typ "'A" @@ tfun_typ "'B" @@
      (fun_typ (tvar "'A") @@ fun_typ (tvar "'B") int256_typ)
    (* Elaborator to run with arbitrary uints *)
    let bsub_elab sc ts = match ts with
      | [b1; b2] when b1 = bnum_typ && b2 = bnum_typ ->
          elab_tfun_with_args sc ts
      | _ -> fail0 "Failed to elaborate"
    let bsub ls _ = match ls with
      | [BNum x; BNum y] ->
          let i1 = big_int_of_string x in
          let i2 = big_int_of_string y in
          let d = Big_int.sub_big_int i1 i2 in
          (match build_prim_literal (Int_typ Bits256) (Big_int.string_of_big_int d) with
          | Some l -> pure l
          | None -> fail0 @@ sprintf "Unable to express result of BNum subtraction in Int256")
      | _ -> builtin_fail "BNum.bsub" ls

  end

  (***********************************************************)
  (******************** Crypto Builtins *************************)
  (***********************************************************)
  module Crypto = struct
    open UsefulLiterals
    open Cryptokit
    open PrimTypes
    open Datatypes.DataTypeDictionary
    open Schnorr

    (* Hash raw bytes / binary string. *)
    let sha256_hasher s = hash_string (Hash.sha2 256) s
    (* Keccak256 hash raw bytes / binary string. *)
    let keccak256_hasher s = hash_string (Hash.keccak 256) s
    (* Ripemd hash raw bytes/ binary string. *)
    let ripemd160_hasher s = hash_string (Hash.ripemd160 ()) s

    let hash_helper hasher name len ls = match ls with
      | [l] ->
          let rec raw_bytes l = match l with
          (* we don't keep type information here *)
            | StringLit s -> s
            | IntLit il -> bstring_from_int_lit il
            | UintLit uil -> bstring_from_uint_lit uil
            | BNum s -> s
            | ByStr bs -> Bystr.to_raw_bytes bs
            | ByStrX bs -> Bystrx.to_raw_bytes bs
            | Msg entries ->
                let raw_entries = List.map entries ~f:(fun (s, v) -> s ^ raw_bytes v) in
                Core.String.concat ~sep:"" raw_entries
            | Map (_, tbl) ->
                let raw_strings = Caml.Hashtbl.fold (fun k v acc -> raw_bytes k :: raw_bytes v :: acc) tbl [] in
                Core.String.concat ~sep:"" raw_strings
            | ADTValue (cons_name, _, params) ->
                let raw_params = List.map params ~f:raw_bytes in
                Core.String.concat ~sep:"" (cons_name :: raw_params)
            | Clo _fun -> "(Clo <fun>)"
            | TAbs _fun -> "(Tabs <fun>)"
          in
          let lhash = hasher (raw_bytes l) in
          (match Bystrx.of_raw_bytes len lhash with
          | Some bs -> pure @@ ByStrX bs
          | None -> builtin_fail ("Crypto." ^ name ^ ": internal error, invalid hash") ls)
      | _ -> builtin_fail ("Crypto." ^ name) ls

    let eq_type = tfun_typ "'A" (fun_typ (tvar "'A") @@ fun_typ (tvar "'A") bool_typ)
    let eq_arity = 2
    let eq_elab sc ts =
      match ts with
      | [bstyp1; bstyp2] when
          (* We want both types to be ByStr with equal width. *)
          is_bystrx_type bstyp1 && bstyp1 = bstyp2
        -> elab_tfun_with_args sc [bstyp1]
      | _ -> fail0 "Failed to elaborate"
    let eq ls _ = match ls with
      | [ByStrX bs1; ByStrX bs2] ->
          pure @@ to_Bool (Bystrx.equal bs1 bs2)
      | _ -> builtin_fail "Crypto.eq" ls

    let hash_type = tfun_typ "'A" @@ fun_typ (tvar "'A") (bystrx_typ hash_length)
    let hash_arity = 1
    let hash_elab sc ts = match ts with
      | [_]  -> elab_tfun_with_args sc ts
      | _ -> fail0 "Failed to elaborate"
    let sha256hash ls _ = hash_helper sha256_hasher "sha256hash" hash_length ls
    let keccak256hash ls _ = hash_helper keccak256_hasher "keccak256hash" hash_length ls
    (* RIPEMD-160 is a 160 bit hash, so define a separate type for it. *)
    let ripemd160hash_type = tfun_typ "'A" @@ fun_typ (tvar "'A") (bystrx_typ address_length)
    let ripemd160hash ls _ = hash_helper ripemd160_hasher "ripemd160hash" address_length ls

    (* ByStrX -> ByStr *)
    let to_bystr_type = tfun_typ "'A" @@ fun_typ (tvar "'A") bystr_typ
    let to_bystr_arity = 1
    let to_bystr_elab sc ts = match ts with
      | [t] when is_bystrx_type t -> elab_tfun_with_args sc ts
      | _ -> fail0 "Failed to elaborate"
    let to_bystr ls _ = match ls with
      | [ByStrX bs] -> pure @@ ByStr (Bystrx.to_bystr bs)
      | _ -> builtin_fail "Crypto.to_bystr" ls

    let to_uint256_type = tfun_typ "'A" @@ fun_typ (tvar "'A") uint256_typ
    let to_uint256_arity = 1
    let to_uint256_elab sc ts = match ts with
      | [PrimType (Bystrx_typ w)] when w <= 32 -> elab_tfun_with_args sc ts
      | _ -> fail0 "Failed to elaborate"
    let to_uint256 ls _ = match ls with
      | [ByStrX bs] when Bystrx.width bs <= 32 ->
        (* of_bytes_big_endian functions expect 2^n number of bytes exactly *)
        let rem = 32 - Bystrx.width bs in
        let pad = Core.String.make rem '\000' in
        let bs_padded = pad ^ (Bystrx.to_raw_bytes bs) in
        let u = Uint256.of_bytes_big_endian (Bytes.of_string bs_padded) 0 in
        pure (UintLit (Uint256L u))
      | _ -> builtin_fail "Crypto.to_uint256" ls

    let bech32_to_bystr20_type = fun_typ string_typ (fun_typ string_typ (option_typ (bystrx_typ address_length)))
    let bech32_to_bystr20_arity = 2
    let bech32_to_bystr20 ls _ = match ls with
      | [StringLit prefix; StringLit addr] ->
        if prefix <> "zil" && prefix <> "tzil"
        then fail0 "Only zil and tzil bech32 addresses are supported" else
        (match Bech32.decode_bech32_addr ~prefix ~addr with
        | Some bys20 ->
          (match Bystrx.of_raw_bytes 20 bys20 with
          | Some b -> some_lit @@ ByStrX b
          | None -> fail0 "Invalid bech32 decode"
          )
        | None -> fail0 "bech32 decoding failed"
        )
      | _ -> builtin_fail "Crypto.bech32_to_bystr20" ls

    let bystr20_to_bech32_type = fun_typ string_typ (fun_typ (bystrx_typ address_length) (option_typ (string_typ)))
    let bystr20_to_bech32_arity = 2
    let bystr20_to_bech32 ls _ = match ls with
      | [StringLit prefix; ByStrX addr] ->
        if prefix <> "zil" && prefix <> "tzil"
        then fail0 "Only zil and tzil bech32 addresses are supported" else
        (match Bech32.encode_bech32_addr ~prefix ~addr:(Bystrx.to_raw_bytes addr) with
        | Some bech32 -> some_lit @@ (StringLit bech32)
        | None -> fail0 "bech32 encoding failed"
        )
      | _ -> builtin_fail "Crypto.bystr20_to_bech32" ls

    (* ByStrX + ByStrY -> ByStr(X+Y)*)
    let concat_type = tfun_typ "'A" @@ tfun_typ "'B" @@ tfun_typ "'C" @@
                      fun_typ (tvar "'A") (fun_typ (tvar "'B") (tvar "'C"))
    let concat_arity = 2
    let concat_elab sc ts = match ts with
      | [PrimType (Bystrx_typ w1); PrimType (Bystrx_typ w2)] ->
          elab_tfun_with_args sc (ts @ [bystrx_typ (w1 + w2)])
      | _ -> fail0 "Failed to elaborate"
    let concat ls _ = match ls with
      | [ByStrX bs1; ByStrX bs2] -> pure @@ ByStrX (Bystrx.concat bs1 bs2)
      | _ -> builtin_fail "Crypto.concat" ls


    let [@warning "-32"] ec_gen_key_pair_type =
      fun_typ unit_typ (pair_typ (bystrx_typ privkey_len) (bystrx_typ pubkey_len))
    let [@warning "-32"] ec_gen_key_pair_arity = 0
    let [@warning "-32"] ec_gen_key_pair ls _ =
      match ls with
      | [] ->
        let privK, pubK = genKeyPair () in
        let privK_lit_o = Bystrx.of_raw_bytes privkey_len privK in
        let pubK_lit_o = Bystrx.of_raw_bytes pubkey_len pubK in
        (match privK_lit_o, pubK_lit_o with
        | Some privK', Some pubK' -> pair_lit (ByStrX privK') (ByStrX pubK')
        | _ -> builtin_fail "ec_gen_key_pair: internal error, invalid private/public key(s)." ls)
      | _ -> builtin_fail "ec_gen_key_pair" ls

    let [@warning "-32"] schnorr_sign_type =
      fun_typ (bystrx_typ privkey_len) @@ (* private key *)
      fun_typ (bystrx_typ pubkey_len) @@ (* public key *)
      fun_typ (bystr_typ) @@ (* message to be signed *)
      (bystrx_typ signature_len) (* signature *)
    let [@warning "-32"] schnorr_sign_arity = 3
    let [@warning "-32"] schnorr_sign ls _ =
      match ls with
      | [ByStrX privkey; ByStrX pubkey; ByStr msg]
          when Bystrx.width privkey = privkey_len &&
               Bystrx.width pubkey = pubkey_len ->
        let s = sign (Bystrx.to_raw_bytes privkey) (Bystrx.to_raw_bytes pubkey) (Bystr.to_raw_bytes msg) in
        (match Bystrx.of_raw_bytes signature_len s with
        | Some bs -> pure @@ ByStrX bs
        | None -> builtin_fail "schnorr_sign: internal error, invalid signature." ls)
      | _ -> builtin_fail "schnorr_sign" ls

    let schnorr_verify_type = fun_typ (bystrx_typ pubkey_len) @@ (* public key *)
                              fun_typ (bystr_typ) @@ (* signed message *)
                              fun_typ (bystrx_typ signature_len) @@ (* signature *)
                              bool_typ
    let schnorr_verify_arity = 3
    let schnorr_verify ls _ =
      match ls with
      | [ByStrX pubkey; ByStr msg; ByStrX signature]
          when Bystrx.width pubkey = pubkey_len &&
               Bystrx.width signature = signature_len ->
        let v = verify (Bystrx.to_raw_bytes pubkey) (Bystr.to_raw_bytes msg) (Bystrx.to_raw_bytes signature) in
        pure @@ to_Bool v
      | _ -> builtin_fail "schnorr_verify" ls

    let [@warning "-32"] ecdsa_sign_type =
      fun_typ (bystrx_typ Secp256k1Wrapper.privkey_len) @@ (* private key *)
      fun_typ (bystr_typ) @@ (* message to be signed *)
      (bystrx_typ Secp256k1Wrapper.signature_len) (* signature *)
    let [@warning "-32"] ecdsa_sign_arity = 2
    let [@warning "-32"] ecdsa_sign ls _ =
      let open Secp256k1Wrapper in
      match ls with
      | [ByStrX privkey; ByStr msg]
          when Bystrx.width privkey = privkey_len ->
        let%bind s = sign (Bystrx.to_raw_bytes privkey) (Bystr.to_raw_bytes msg) in
        (match Bystrx.of_raw_bytes signature_len s with
        | Some bs -> pure @@ ByStrX bs
        | None -> builtin_fail "ecdsa_sign: internal error, invalid signature." ls)
      | _ -> builtin_fail "ecdsa_sign" ls

    let ecdsa_verify_type = fun_typ (bystrx_typ Secp256k1Wrapper.pubkey_len) @@ (* public key *)
                              fun_typ (bystr_typ) @@ (* signed message *)
                              fun_typ (bystrx_typ Secp256k1Wrapper.signature_len) @@ (* signature *)
                              bool_typ
    let ecdsa_verify_arity = 3
    let ecdsa_verify ls _ =
      let open Secp256k1Wrapper in
      match ls with
      | [ByStrX pubkey; ByStr msg; ByStrX signature]
          when Bystrx.width signature = signature_len &&
               Bystrx.width pubkey = pubkey_len ->
        let%bind v = verify (Bystrx.to_raw_bytes pubkey) (Bystr.to_raw_bytes msg) (Bystrx.to_raw_bytes signature) in
        pure @@ to_Bool v
      | _ -> builtin_fail "ecdsa_verify" ls

    let schnorr_get_address_type = fun_typ (bystrx_typ pubkey_len) (bystrx_typ address_length)
    let schnorr_get_address_arity = 1
    let schnorr_get_address ls _ =
      match ls with
      | [ByStrX pubkey] when Bystrx.width pubkey = pubkey_len ->
        let pks = Bystrx.to_raw_bytes pubkey in
        (* Hash the public key *)
        let pkh = sha256_hasher pks in
        (* and extract the least significant 20 bytes. *)
        let addr = Core.String.suffix pkh 20 in
        (match Bystrx.of_raw_bytes address_length addr with
        | Some bs -> pure @@ ByStrX bs
        | None -> builtin_fail "schnorr_get_address: Internal error." ls)
      | _ -> builtin_fail "schnorr_get_address" ls

  end

  (***********************************************************)
  (* Maps *)
  (***********************************************************)
  module Maps = struct
    open PrimTypes
    open UsefulLiterals
    open Datatypes.DataTypeDictionary

    let contains_arity = 2
    let contains_type =
      tfun_typ "'K" @@ tfun_typ "'V" @@
      (fun_typ (map_typ (tvar "'K") (tvar "'V")) @@ fun_typ (tvar "'K") bool_typ)
    let contains_elab sc ts = match ts with
      | [MapType (kt, vt); u] when kt = u  ->
          elab_tfun_with_args sc [kt; vt]
      | _ -> fail0 "Failed to elaborate"
    let contains ls _ = match ls with
      | [Map (_, entries); key] ->
          let res = Caml.Hashtbl.mem entries key in
          pure @@ to_Bool res
      | _ -> builtin_fail "Map.contains" ls


    let put_arity = 3
    let put_type =
      tfun_typ "'K" @@ tfun_typ "'V" @@
      (fun_typ (map_typ (tvar "'K") (tvar "'V")) @@
       fun_typ (tvar "'K") @@
       fun_typ (tvar "'V") (map_typ (tvar "'K") (tvar "'V")))
    let put_elab sc ts = match ts with
      | [MapType (kt, vt); kt'; vt'] when kt = kt' && vt = vt'  ->
          elab_tfun_with_args sc [kt; vt]
      | _ -> fail0 "Failed to elaborate"
    let put ls _ = match ls with
      | [Map (tm, entries); key; value] ->
          (* Scilla semantics is not in-place modification. *)
          let entries' = Caml.Hashtbl.copy entries in
          let _ = Caml.Hashtbl.replace entries' key value in
          pure @@ Map (tm, entries') 
      | _ -> builtin_fail "Map.put" ls


    (* Must take result type into the account *)
    let get_arity = 2
    let get_type =
      tfun_typ "'K" @@ tfun_typ "'V" @@
      (fun_typ (map_typ (tvar "'K") (tvar "'V")) @@
       fun_typ (tvar "'K") (option_typ (tvar "'V")))
    let get_elab sc ts = match ts with
      | [MapType (kt, vt); kt'] when kt = kt'  ->
          elab_tfun_with_args sc [kt; vt]
      | _ -> fail0 "Failed to elaborate"
    (* Notice that get passes return type *)
    let get ls rt = match ls, rt with
      | [Map (_, entries); key], ADT ("Option", [targ]) ->
          let res = Caml.Hashtbl.find_opt entries key in
          (match res with
           | None -> pure @@ none_lit targ
           | Some v -> some_lit v)
      | _ -> builtin_fail "Map.get" ls

    let remove_arity = 2
    let remove_type =
      tfun_typ "'K" @@ tfun_typ "'V" @@
      (fun_typ (map_typ (tvar "'K") (tvar "'V")) @@
       fun_typ (tvar "'K") (map_typ (tvar "'K") (tvar "'V")))
    let remove_elab sc ts = match ts with
      | [MapType (kt, vt); u] when kt = u  ->
          elab_tfun_with_args sc [kt; vt]
      | _ -> fail0 "Failed to elaborate" 
    let remove ls _ = match ls with
      | [Map (tm, entries); key] ->
          (* Scilla semantics is not in-place modification. *)
          let entries' = Caml.Hashtbl.copy entries in
          let _ = Caml.Hashtbl.remove entries' key in
          pure @@ Map (tm, entries')
      | _ -> builtin_fail "Map.remove" ls


    let to_list_arity = 1
    let to_list_type =
      tfun_typ "'K" @@ tfun_typ "'V" @@
      (fun_typ (map_typ (tvar "'K") (tvar "'V")) @@
       (list_typ (pair_typ (tvar "'K") (tvar "'V"))))
    let to_list_elab sc ts = match ts with
      | [MapType (kt, vt)]  -> elab_tfun_with_args sc [kt; vt]
      | _ -> fail0 "Failed to elaborate" 
    let to_list ls _ = match ls with
      | [Map ((kt, vt), entries)] ->
          (* The type of the output list will be "Pair (kt) (vt)" *)
          let otyp = pair_typ kt vt in
          let nil = ADTValue ("Nil", [otyp], []) in
          let ol = Caml.Hashtbl.fold
              (fun k v accum ->
               let kv = ADTValue ("Pair", [kt; vt], [k; v]) in
               let kvl = ADTValue ("Cons", [otyp], [kv; accum]) in
               kvl) entries nil
          in pure (ol)
      | _ -> builtin_fail "Map.to_list" ls

    let size_arity = 1
    let size_type =
      tfun_typ "'K" @@ tfun_typ "'V" @@
      (fun_typ (map_typ (tvar "'K") (tvar "'V")) @@
       uint32_typ)
    let size_elab sc ts = match ts with
      | [MapType (kt, vt)]  -> elab_tfun_with_args sc [kt; vt]
      | _ -> fail0 "Failed to elaborate" 
    let size ls _ = match ls with
      | [Map (_, entries)] ->
          (* The type of the output will be "Uint32" *)
          let ol = Caml.Hashtbl.length entries in
          pure (UintLit (Uint32L (Stdint.Uint32.of_int ol)))
      | _ -> builtin_fail "Map.size" ls
  end


  (* Identity elaborator *)
  let elab_id = fun t _ -> pure t


  (**********************************************************)
  (*                   Built-in  Dictionary                *)
  (**********************************************************)

  module BuiltInDictionary = struct 

    (* Elaborates the operation type based on the arguments types *)
    type elaborator = typ -> typ list -> (typ, scilla_error list) result      

    (* Takes the expected type as an argument to elaborate the result *)
    type built_in_executor = literal list -> typ -> (literal, scilla_error list) result

    (* A built-in record type:
       * arity
       * full, unelaborated type
       * elaborator, refining the type based on argument 
         to support polymorphism -- e.g., for ints and maps
       * executor - operational semantics of the built-in
    *)
    type built_in_record = int * typ * elaborator * built_in_executor

    (* All built-in functions *)
    let built_in_multidict : builtin -> built_in_record list = function
      | Builtin_eq -> [String.eq_arity, String.eq_type, elab_id, String.eq;
                       BNum.eq_arity, BNum.eq_type, elab_id , BNum.eq;
                       Crypto.eq_arity, Crypto.eq_type, Crypto.eq_elab, Crypto.eq;
                       Int.eq_arity, Int.eq_type, Int.eq_elab, Int.eq;
                       Uint.eq_arity, Uint.eq_type, Uint.eq_elab, Uint.eq]
      | Builtin_concat -> [String.concat_arity, String.concat_type, elab_id, String.concat;
                           Crypto.concat_arity, Crypto.concat_type, Crypto.concat_elab, Crypto.concat]
      | Builtin_to_uint256 -> [Crypto.to_uint256_arity, Crypto.to_uint256_type, Crypto.to_uint256_elab, Crypto.to_uint256;
                               Uint.to_uint_arity, Uint.to_uint_type, Uint.to_uint_elab Bits256, Uint.to_uint256]
      (* Strings *)
      | Builtin_substr -> [String.substr_arity, String.substr_type, elab_id, String.substr]
      | Builtin_strlen -> [String.strlen_arity, String.strlen_type, elab_id, String.strlen]
      | Builtin_to_string -> [String.to_string_arity, String.to_string_type, String.to_string_elab, String.to_string]

      (* Block numbers *)
      | Builtin_blt -> [BNum.blt_arity, BNum.blt_type, elab_id, BNum.blt]
      | Builtin_badd -> [BNum.badd_arity, BNum.badd_type, BNum.badd_elab, BNum.badd]
      | Builtin_bsub -> [BNum.bsub_arity, BNum.bsub_type, BNum.bsub_elab, BNum.bsub]

      (* Crypto *)
      | Builtin_sha256hash -> [Crypto.hash_arity, Crypto.hash_type,Crypto.hash_elab, Crypto.sha256hash]
      | Builtin_keccak256hash -> [Crypto.hash_arity, Crypto.hash_type,Crypto.hash_elab, Crypto.keccak256hash]
      | Builtin_ripemd160hash -> [Crypto.hash_arity, Crypto.ripemd160hash_type,Crypto.hash_elab, Crypto.ripemd160hash]
      | Builtin_to_bystr -> [Crypto.to_bystr_arity, Crypto.to_bystr_type, Crypto.to_bystr_elab, Crypto.to_bystr]
      | Builtin_bech32_to_bystr20 -> [Crypto.bech32_to_bystr20_arity, Crypto.bech32_to_bystr20_type, elab_id, Crypto.bech32_to_bystr20]
      | Builtin_bystr20_to_bech32 -> [Crypto.bystr20_to_bech32_arity, Crypto.bystr20_to_bech32_type, elab_id, Crypto.bystr20_to_bech32]
      | Builtin_schnorr_verify -> [Crypto.schnorr_verify_arity, Crypto.schnorr_verify_type, elab_id, Crypto.schnorr_verify]
      | Builtin_ecdsa_verify -> [Crypto.ecdsa_verify_arity, Crypto.ecdsa_verify_type, elab_id, Crypto.ecdsa_verify]
      | Builtin_schnorr_get_address -> [Crypto.schnorr_get_address_arity, Crypto.schnorr_get_address_type, elab_id, Crypto.schnorr_get_address]

      (* Maps *)
      | Builtin_contains -> [Maps.contains_arity, Maps.contains_type, Maps.contains_elab, Maps.contains]
      | Builtin_put -> [Maps.put_arity, Maps.put_type, Maps.put_elab, Maps.put]
      | Builtin_get -> [Maps.get_arity, Maps.get_type, Maps.get_elab, Maps.get]
      | Builtin_remove -> [Maps.remove_arity, Maps.remove_type, Maps.remove_elab, Maps.remove]
      | Builtin_to_list -> [Maps.to_list_arity, Maps.to_list_type, Maps.to_list_elab, Maps.to_list]
      | Builtin_size -> [Maps.size_arity, Maps.size_type, Maps.size_elab, Maps.size]

      (* Integers *)
      | Builtin_lt -> [Int.eq_arity, Int.eq_type, Int.eq_elab, Int.lt;
                       Uint.eq_arity, Uint.eq_type, Uint.eq_elab, Uint.lt]
      | Builtin_add -> [Int.binop_arity, Int.binop_type, Int.binop_elab, Int.add;
                        Uint.binop_arity, Uint.binop_type, Uint.binop_elab, Uint.add]
      | Builtin_sub -> [Int.binop_arity, Int.binop_type, Int.binop_elab, Int.sub;
                        Uint.binop_arity, Uint.binop_type, Uint.binop_elab, Uint.sub]
      | Builtin_mul -> [Int.binop_arity, Int.binop_type, Int.binop_elab, Int.mul;
                        Uint.binop_arity, Uint.binop_type, Uint.binop_elab, Uint.mul]
      | Builtin_div -> [Int.binop_arity, Int.binop_type, Int.binop_elab, Int.div;
                        Uint.binop_arity, Uint.binop_type, Uint.binop_elab, Uint.div]
      | Builtin_rem -> [Int.binop_arity, Int.binop_type, Int.binop_elab, Int.rem;
                        Uint.binop_arity, Uint.binop_type, Uint.binop_elab, Uint.rem]
      | Builtin_pow -> [Int.pow_arity, Int.pow_type, Int.pow_elab, Int.pow;
                        Uint.pow_arity, Uint.pow_type, Uint.pow_elab, Uint.pow]

      (* Signed integers specific builtins *)
      | Builtin_to_int32 -> [Int.to_int_arity, Int.to_int_type, Int.to_int_elab Bits32, Int.to_int32]
      | Builtin_to_int64 -> [Int.to_int_arity, Int.to_int_type, Int.to_int_elab Bits64, Int.to_int64]
      | Builtin_to_int128 -> [Int.to_int_arity, Int.to_int_type, Int.to_int_elab Bits128, Int.to_int128]
      | Builtin_to_int256 -> [Int.to_int_arity, Int.to_int_type, Int.to_int_elab Bits256, Int.to_int256]

      (* Unsigned integers specific builtins *)
      | Builtin_to_uint32 -> [Uint.to_uint_arity, Uint.to_uint_type, Uint.to_uint_elab Bits32, Uint.to_uint32]
      | Builtin_to_uint64 -> [Uint.to_uint_arity, Uint.to_uint_type, Uint.to_uint_elab Bits64, Uint.to_uint64]
      | Builtin_to_uint128 -> [Uint.to_uint_arity, Uint.to_uint_type, Uint.to_uint_elab Bits128, Uint.to_uint128]
      | Builtin_to_nat -> [Uint.to_nat_arity, Uint.to_nat_type, Uint.to_nat_elab, Uint.to_nat]

    (* Dictionary lookup based on the operation name and type *)
    let find_builtin_op (op, rep) argtypes =
      let finder = (function (arity, optype, elab, exec) ->
          if arity = List.length argtypes
          then
            (* First: elaborate based on argument types *)
            let%bind type_elab = elab optype argtypes in
            (* Second: check applicability *)
            let%bind res_type = fun_type_applies type_elab argtypes in
            pure (type_elab, res_type, exec)
          else fail0 @@ "Name or arity don't match") in
      let dict = built_in_multidict op in
      let%bind (_, (type_elab, res_type, exec)) = tryM dict ~f:finder
          ~msg:(fun () ->
              mk_error1
                (sprintf "Type error: cannot apply \"%s\" built-in to argument(s) of type(s) %s." (pp_builtin op) (pp_typ_list argtypes))
                (ER.get_loc rep))
      in pure (type_elab, res_type, exec)

  end

end
