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

exception IntOverflow
exception IntUnderflow

module ScillaBuiltIns
    (SR : Rep)
    (ER : Rep) = struct

  module BuiltinTypeUtilities = TypeUtilities (SR) (ER)
  open BuiltinTypeUtilities

  let print_literal_list ls =
    PrettyPrinters.pp_literal_list ls

  let builtin_fail name ls =
    fail0 @@ sprintf "Cannot apply built-in %s to a list of arguments:%s."
      name (print_literal_list ls)

  module UsefulLiterals = struct
    let true_lit = ADTValue ("True", [], [])
    let false_lit = ADTValue ("False", [], [])

    let some_lit l =
      let%bind t = literal_type l in
      pure @@ ADTValue ("Some", [t], [l])

    let none_lit t = ADTValue ("None", [t], [])

    let pair_lit l1 l2 =
      let%bind t1 = literal_type l1 in
      let%bind t2 = literal_type l2 in
      pure @@ ADTValue ("Pair", [t1;t2], [l1;l2])

    let to_Bool b = if b then true_lit else false_lit
  end

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
    let substr_type =
      tfun_typ "'A" @@ tfun_typ "'B" @@ tfun_typ "'C" @@
      (fun_typ (tvar "'A") @@ fun_typ (tvar "'B") @@ fun_typ (tvar "'C") string_typ)
    (* Elaborator to run with arbitrary uints *)
    let substr_elab _ ts = match ts with
      | [s; u1; u2]
        when s = string_typ &&
             is_uint_type u1 &&
             is_uint_type u2 ->
          elab_tfun_with_args substr_type ts
      | _ -> fail0 "Failed to elaborate"

    let substr ls _ = match ls with
      | [StringLit x; UintLit (Uint32L s); UintLit (Uint32L e)] ->
          pure @@ StringLit (Core.String.sub x ~pos:(Uint32.to_int s) ~len:(Uint32.to_int e))
      | _ -> builtin_fail "String.substr" ls

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
        | [ByStr x] | [ByStrX (_, x)] -> pure x
        | _ -> builtin_fail (sprintf "String.to_string") ls
      in pure (BatOption.get (build_prim_literal string_typ s))

  end

  (*******************************************************)
  (* Manipulating with arbitrary integer representations *)
  (*******************************************************)
  module type IntRep = sig
    type t
    val compare : t -> t -> int
    val add: t -> t -> t
    val sub: t -> t -> t
    val mul: t -> t -> t
    val div: t -> t -> t
    val rem : t -> t -> t
    val zero: t
    val one : t
    val min_int: t
  end

  module StdIntWrapper(R: IntRep) = struct
    open R

    let safe_add a b =
      let r = add a b in
      (* if a > 0 && b > 0 && r < 0 then we have an overflow*)
      if ((compare a zero) > 0 && (compare b zero) > 0 && (compare r zero) < 0) 
      then raise IntOverflow
      (* if a < 0 && b < 0 && r > 0 then we have an underflow*)
      else if ((compare a zero) < 0 && (compare b zero) < 0 && (compare r zero) > 0) 
      then raise IntUnderflow
      else r

    let safe_sub a b =
      let r = R.sub a b in
      (* if a > 0 && b < 0 && r < 0 then we have an overflow *)
      if ((compare a zero) > 0 && (compare b zero) < 0 && (compare r zero) < 0) 
      then raise IntOverflow
      (* if a < 0 && b > 0 && r > 0 then we have an underflow*)
      else if ((compare a zero) < 0 && (compare b zero) > 0 && (compare r zero) > 0) 
      then raise IntUnderflow
      else r

    let safe_mul a b  =
      let r = mul a b in
      (* http://www.informit.com/articles/article.aspx?p=1959565&seqNum=13 *)
      (* if b < 0 && a = int_min OR if b != 0 && r / b != a *)
      if (compare b zero) < 0 && a = min_int then raise IntOverflow else
      if compare b zero <> 0
      then
        let d = div r b in
        if (compare d a <> 0)
        then raise IntOverflow
        else r
      else r

    let safe_div a b =
      (* Integer overflow during division occurs in a very specific case. *)
      (* https://stackoverflow.com/a/30400252/2128804 *)
      if a = min_int && b = (sub zero one) then raise IntOverflow else
        (* Division_by_zero is taken care of by underlying implementation. *)
        div a b

    let safe_rem a b =
      (* Division_by_zero is taken care of by underlying implementation. *)
      rem a b

    let safe_pow a b =
      let rec pow acc b' =
        if b' = Uint32.zero then
          acc
        else
          pow (safe_mul a acc) (Uint32.sub b' Uint32.one)
      in
      pow one b

    let safe_lt a b =
      if (compare a b) < 0 then true else false

  end

  module StdUintWrapper(R: IntRep) = struct
    open R

    let safe_add a b =
      let r = add a b in
      (* if r < a || r < b then we have an overflow *)
      if ((compare r a) < 0 || (compare r b) < 0)
      then raise IntOverflow
      else r

    let safe_sub a b =
      let r = sub a b in
      (* if a < b then we have an underflow *)
      if (compare a b) < 0
      then raise IntUnderflow
      else r

    let safe_mul a b  =
      let r = mul a b in
      (* if b != 0 && r / b != a *)
      if compare b zero <> 0
      then
        let d = div r b in
        if (compare d a <> 0)
        then raise IntOverflow
        else r
      else r

    let safe_div a b =
      (* Division_by_zero is taken care of by underlying implementation. *)
      div a b
        
    let safe_rem a b =
      (* Division_by_zero is taken care of by underlying implementation. *)
      rem a b

    let safe_pow a b =
      let rec pow acc b' =
        if b' = Uint32.zero then
          acc
        else
          pow (safe_mul a acc) (Uint32.sub b' Uint32.one)
      in
      pow one b

    let safe_lt a b =
      if (compare a b) < 0 then true else false

  end

  (* Instantiating the functors *)
  module Int32Wrapper = StdIntWrapper(Int32)
  module Int64Wrapper = StdIntWrapper(Int64)
  module Int128Wrapper = StdIntWrapper(Int128)
  module Int256Wrapper = StdIntWrapper(Int256)
  module Uint32Wrapper = StdUintWrapper(Uint32)
  module Uint64Wrapper = StdUintWrapper(Uint64)
  module Uint128Wrapper = StdUintWrapper(Uint128)
  module Uint256Wrapper = StdUintWrapper(Uint256)

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
              pure @@ Int32L(Int32Wrapper.safe_add x y)
          | [IntLit (Int64L x); IntLit (Int64L y)] ->
              pure @@ Int64L(Int64Wrapper.safe_add x y)
          | [IntLit (Int128L x); IntLit (Int128L y)] ->
              pure @@ Int128L(Int128Wrapper.safe_add x y)
          | [IntLit (Int256L x); IntLit (Int256L y)] ->
              pure @@ Int256L(Int256Wrapper.safe_add x y)
          | _ -> builtin_fail "Int.add: unsupported types" ls)
        in pure @@ IntLit l
      with | IntOverflow | IntUnderflow ->
        builtin_fail "Int.add: an overflow/underflow occurred" ls

    let sub ls _ = 
      try 
        let%bind l =(match ls with
          | [IntLit (Int32L x); IntLit (Int32L y)] ->
              pure @@ Int32L(Int32Wrapper.safe_sub x y)
          | [IntLit (Int64L x); IntLit (Int64L y)] ->
              pure @@ Int64L(Int64Wrapper.safe_sub x y)
          | [IntLit (Int128L x); IntLit (Int128L y)] ->
              pure @@ Int128L (Int128Wrapper.safe_sub x y)
          | [IntLit (Int256L x); IntLit (Int256L y)] ->
              pure @@ Int256L (Int256Wrapper.safe_sub x y)
          | _ -> builtin_fail "Int.sub: unsupported types" ls)
        in pure @@ IntLit l
      with | IntOverflow | IntUnderflow ->
        builtin_fail "Int.sub: an overflow/underflow occurred" ls

    let mul ls _ = 
      try 
        let%bind l = (match ls with
          | [IntLit (Int32L x); IntLit (Int32L y)] ->
              pure @@ Int32L(Int32Wrapper.safe_mul x y)
          | [IntLit (Int64L x); IntLit (Int64L y)] ->
              pure @@ Int64L(Int64Wrapper.safe_mul x y)
          | [IntLit (Int128L x); IntLit (Int128L y)] ->
              pure @@ Int128L(Int128Wrapper.safe_mul x y)
          | [IntLit (Int256L x); IntLit (Int256L y)] ->
              pure @@ Int256L(Int256Wrapper.safe_mul x y)
          | _ -> builtin_fail "Int.mul: unsupported types" ls)
        in pure @@ IntLit l
      with | IntOverflow | IntUnderflow ->
        builtin_fail "Int.mul: an overflow/underflow occurred" ls

    let div ls _ = 
      try 
        let%bind l = (match ls with
          | [IntLit (Int32L x); IntLit (Int32L y)] ->
              pure @@ Int32L(Int32Wrapper.safe_div x y)
          | [IntLit (Int64L x); IntLit (Int64L y)] ->
              pure @@ Int64L(Int64Wrapper.safe_div x y)
          | [IntLit (Int128L x); IntLit (Int128L y)] ->
              pure @@ Int128L(Int128Wrapper.safe_div x y)
          | [IntLit (Int256L x); IntLit (Int256L y)] ->
              pure @@ Int256L(Int256Wrapper.safe_div x y)
          | _ -> builtin_fail "Int.div: unsupported types" ls)
        in pure @@ IntLit l
        with | Division_by_zero | IntOverflow ->
          builtin_fail "Int.div: Division by zero / IntOverflow error occurred" ls

    let rem ls  _ =
      try 
        let%bind l = (match ls with
          | [IntLit (Int32L x); IntLit (Int32L y)] ->
              pure @@ Int32L(Int32Wrapper.safe_rem x y)
          | [IntLit (Int64L x); IntLit (Int64L y)] ->
              pure @@ Int64L(Int64Wrapper.safe_rem x y)
          | [IntLit (Int128L x); IntLit (Int128L y)] ->
              pure @@ Int128L(Int128Wrapper.safe_rem x y)
          | [IntLit (Int256L x); IntLit (Int256L y)] ->
              pure @@ Int256L(Int256Wrapper.safe_rem x y)
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
            pure @@ Int32L(Int32Wrapper.safe_pow x y)
        | [IntLit (Int64L x); UintLit (Uint32L y)] ->
            pure @@ Int64L(Int64Wrapper.safe_pow x y)
        | [IntLit (Int128L x); UintLit (Uint32L y)] ->
            pure @@ Int128L(Int128Wrapper.safe_pow x y)
        | [IntLit (Int256L x); UintLit (Uint32L y)] ->
            pure @@ Int256L(Int256Wrapper.safe_pow x y)
        | _ -> builtin_fail "Int.pow: unsupported types" ls)
      in pure @@ IntLit l
    with | IntOverflow | IntUnderflow ->
      builtin_fail "Int.pow: an overflow/underflow occurred" ls

    let lt ls _ =
      try 
        (match ls with
          | [IntLit (Int32L x); IntLit (Int32L y)] ->
              pure @@ to_Bool (Int32Wrapper.safe_lt x y)
          | [IntLit (Int64L x); IntLit (Int64L y)] ->
              pure @@ to_Bool (Int64Wrapper.safe_lt x y)
          | [IntLit (Int128L x); IntLit (Int128L y)] ->
              pure @@ to_Bool (Int128Wrapper.safe_lt x y)
          | [IntLit (Int256L x); IntLit (Int256L y)] ->
              pure @@ to_Bool (Int256Wrapper.safe_lt x y)
          | _ -> builtin_fail "Int.lt: unsupported types" ls)
        with | IntOverflow | IntUnderflow ->
          builtin_fail "Int.lt: an overflow/underflow occurred" ls

    let mk_int_type w = match w with
      | 32 -> pure int32_typ
      | 64 -> pure int64_typ
      | 128 -> pure int128_typ
      | 256 -> pure int256_typ
      | _ -> fail0 "Failed to convert" 

    let to_int_arity = 1
    let to_int_type = tfun_typ "'A" @@ tfun_typ "'B" (fun_typ (tvar "'A") (option_typ (tvar "'B")))
    let to_int_elab w sc ts = match ts with
      | [t] when is_int_type t || is_uint_type t || (t = string_typ) ->
          let%bind ityp = mk_int_type w in
          elab_tfun_with_args sc [t; ityp]
      | _ -> fail0 "Failed to elaborate"

    let to_int_helper ls w = 
      let%bind xs = match ls with
        | [IntLit x] -> pure @@ string_of_int_lit x
        | [UintLit x] -> pure @@ string_of_uint_lit x
        | [StringLit x] -> pure x
        | _ -> builtin_fail (sprintf "Int.to_int%i" w) ls
      in
        let%bind ityp = mk_int_type w in
        (match build_prim_literal ityp xs with
        | Some lit ->
          pure (ADTValue ("Some", [ityp], [lit]))
        | None ->
          pure (ADTValue ("None", [ityp], [])))

    let to_int32 ls _ = to_int_helper ls 32
    let to_int64 ls _ = to_int_helper ls 64
    let to_int128 ls _ = to_int_helper ls 128
    let to_int256 ls _ = to_int_helper ls 256

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
              pure @@ Uint32L(Uint32Wrapper.safe_add x y)
          | [UintLit (Uint64L x); UintLit (Uint64L y)] ->
              pure @@ Uint64L(Uint64Wrapper.safe_add x y)
          | [UintLit (Uint128L x); UintLit (Uint128L y)] ->
              pure @@ Uint128L(Uint128Wrapper.safe_add x y)
          | [UintLit (Uint256L x); UintLit (Uint256L y)] ->
              pure @@ Uint256L(Uint256Wrapper.safe_add x y)
          | _ -> builtin_fail "Uint.add: unsupported types" ls)
        in pure @@ UintLit l
      with | IntOverflow | IntUnderflow ->
        builtin_fail "Uint.add: an overflow/underflow occurred" ls

    let sub ls _ = 
      try 
        let%bind l = (match ls with
          | [UintLit (Uint32L x); UintLit (Uint32L y)] ->
              pure @@ Uint32L(Uint32Wrapper.safe_sub x y)
          | [UintLit (Uint64L x); UintLit (Uint64L y)] ->
              pure @@ Uint64L(Uint64Wrapper.safe_sub x y)
          | [UintLit (Uint128L x); UintLit (Uint128L y)] ->
              pure @@ Uint128L(Uint128Wrapper.safe_sub x y)
          | [UintLit (Uint256L x); UintLit (Uint256L y)] ->
              pure @@ Uint256L(Uint256Wrapper.safe_sub x y)
          | _ -> builtin_fail "Uint.sub: unsupported types" ls)
        in pure @@ UintLit l
      with | IntOverflow | IntUnderflow ->
        builtin_fail "Uint.sub: an overflow/underflow occurred" ls

    let mul ls _ = 
      try 
        let%bind l = (match ls with
          | [UintLit (Uint32L x); UintLit (Uint32L y)] ->
              pure @@ Uint32L(Uint32Wrapper.safe_mul x y)
          | [UintLit (Uint64L x); UintLit (Uint64L y)] ->
              pure @@ Uint64L(Uint64Wrapper.safe_mul x y)
          | [UintLit (Uint128L x); UintLit (Uint128L y)] ->
              pure @@ Uint128L(Uint128Wrapper.safe_mul x y)
          | [UintLit (Uint256L x); UintLit (Uint256L y)] ->
              pure @@ Uint256L(Uint256Wrapper.safe_mul x y)
          | _ -> builtin_fail "Uint.mul: unsupported types" ls)
        in pure @@ UintLit l
      with | IntOverflow | IntUnderflow ->
        builtin_fail "Uint.mul: an overflow/underflow occurred" ls

    let div ls _ = 
      try 
        let%bind l = (match ls with
          | [UintLit (Uint32L x); UintLit (Uint32L y)] ->
              pure @@ Uint32L(Uint32Wrapper.safe_div x y)
          | [UintLit (Uint64L x); UintLit (Uint64L y)] ->
              pure @@ Uint64L(Uint64Wrapper.safe_div x y)
          | [UintLit (Uint128L x); UintLit (Uint128L y)] ->
              pure @@ Uint128L(Uint128Wrapper.safe_div x y)
          | [UintLit (Uint256L x); UintLit (Uint256L y)] ->
              pure @@ Uint256L(Uint256Wrapper.safe_div x y)
          | _ -> builtin_fail "Uint.div: unsupported types" ls)
        in pure @@ UintLit l
      with | Division_by_zero ->
        builtin_fail "Uint.div: Division by zero / UintOverflow error occurred" ls

    let rem ls  _ =
      try 
        let%bind l = (match ls with
          | [UintLit (Uint32L x); UintLit (Uint32L y)] ->
              pure @@ Uint32L(Uint32Wrapper.safe_rem x y)
          | [UintLit (Uint64L x); UintLit (Uint64L y)] ->
              pure @@ Uint64L(Uint64Wrapper.safe_rem x y)
          | [UintLit (Uint128L x); UintLit (Uint128L y)] ->
              pure @@ Uint128L(Uint128Wrapper.safe_rem x y)
          | [UintLit (Uint256L x); UintLit (Uint256L y)] ->
              pure @@ Uint256L(Uint256Wrapper.safe_rem x y)
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
              pure @@ Uint32L(Uint32Wrapper.safe_pow x y)
          | [UintLit (Uint64L x); UintLit (Uint32L y)] ->
              pure @@ Uint64L(Uint64Wrapper.safe_pow x y)
          | [UintLit (Uint128L x); UintLit (Uint32L y)] ->
              pure @@ Uint128L(Uint128Wrapper.safe_pow x y)
          | [UintLit (Uint256L x); UintLit (Uint32L y)] ->
              pure @@ Uint256L(Uint256Wrapper.safe_pow x y)
          | _ -> builtin_fail "Int.pow: unsupported types" ls)
        in pure @@ UintLit l
      with | IntOverflow | IntUnderflow ->
        builtin_fail "Int.pow: an overflow/underflow occurred" ls

    let lt ls _ =
      try 
        (match ls with
          | [UintLit (Uint32L x); UintLit (Uint32L y)] ->
              pure @@ to_Bool (Uint32Wrapper.safe_lt x y)
          | [UintLit (Uint64L x); UintLit (Uint64L y)] ->
              pure @@ to_Bool (Uint64Wrapper.safe_lt x y)
          | [UintLit (Uint128L x); UintLit (Uint128L y)] ->
              pure @@ to_Bool (Uint128Wrapper.safe_lt x y)
          | [UintLit (Uint256L x); UintLit (Uint256L y)] ->
              pure @@ to_Bool (Uint256Wrapper.safe_lt x y)
          | _ -> builtin_fail "Uint.lt: unsupported types" ls)
      with | IntOverflow | IntUnderflow ->
        builtin_fail "Uint.lt: an overflow/underflow occurred" ls

    let mk_uint_type w = match w with
      | 32 -> pure uint32_typ
      | 64 -> pure uint64_typ
      | 128 -> pure uint128_typ
      | 256 -> pure uint256_typ
      | _ -> fail0 "Failed to convert" 

    let to_uint_arity = 1
    let to_uint_type = tfun_typ "'A" @@ tfun_typ "'B"
        (fun_typ (tvar "'A") (option_typ (tvar "'B")))

    let to_uint_elab w sc ts = match ts with
      | [t] when is_uint_type t || is_int_type t || (t = string_typ) ->
          let%bind ityp = mk_uint_type w in
          elab_tfun_with_args sc [t; ityp]
      | _ -> fail0 "Failed to elaborate"

    let to_uint_helper ls w = 
      let%bind xs = match ls with
        | [IntLit x] -> pure @@ string_of_int_lit x
        | [UintLit x] -> pure @@ string_of_uint_lit x
        | [StringLit x] -> pure x
        | _ -> builtin_fail (sprintf "UInt.to_uint%i" w) ls
      in
        let%bind ityp = mk_uint_type w in
        (match build_prim_literal ityp xs with
        | Some lit ->
          pure (ADTValue ("Some", [ityp], [lit]))
        | None ->
          pure (ADTValue ("None", [ityp], [])))

    let to_uint32 ls _ = to_uint_helper ls 32
    let to_uint64 ls _ = to_uint_helper ls 64
    let to_uint128 ls _ = to_uint_helper ls 128
    let to_uint256 ls _ = to_uint_helper ls 256

    let to_nat_arity = 1
    let to_nat_type = tfun_typ "'A" @@ (fun_typ (tvar "'A") nat_typ)
    let to_nat_elab sc ts = match ts with
      | [t] when is_uint_type t ->
          elab_tfun_with_args sc [t]
      | _ -> fail0 "Failed to elaborate"

    let to_nat ls _ = match ls with
      | [UintLit (Uint32L n)] ->
        let zero = ADTValue ("Zero", [], []) in
        let rec nat_builder (i : Uint32.t) =
          if i = Uint32.zero then zero
          else
            let prev = nat_builder (Uint32.sub i Uint32.one) in
            ADTValue ("Succ", [], (prev::[]))
        in
        pure (nat_builder n)
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
          (match build_prim_literal int256_typ (Big_int.string_of_big_int d) with
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

    (* Create binary / bytes from ASCII hexadecimal string 0x... *)
    let fromhex s = transform_string (Hexa.decode()) (Core.String.sub s ~pos:2 ~len:((Core.String.length s)-2))
    (* Create ASCII hexadecimal string from raw binary / bytes. *)
    let tohex s = "0x" ^ (transform_string (Hexa.encode()) s)
    (* Hash raw bytes / binary string. *)
    let sha256_hasher s = hash_string (Hash.sha2 256) s
    (* Keccak256 hash raw bytes / binary string. *)
    let keccak256_hasher s = hash_string (Hash.keccak 256) s
    (* Ripemd hash raw bytes/ binary string. *)
    let ripemd160_hasher s = hash_string (Hash.ripemd160 ()) s

    let hash_helper hasher name len ls = match ls with
      | [l] ->
          let lstr =
            (match l with
            | StringLit s -> s
            | IntLit il -> bstring_from_int_lit il
            | UintLit uil -> bstring_from_uint_lit uil
            | ByStr s | ByStrX (_, s) -> fromhex s
            (* Anything else, just serialize with SExp. *)
            | _ -> sexp_of_literal l |> Sexplib.Sexp.to_string) in
          let lhash = hasher lstr in
          let lhash_hex = tohex lhash in
          let lo = build_prim_literal (bystrx_typ len) lhash_hex in
          (match lo with
          | Some l' -> pure @@ l'
          | None -> builtin_fail ("Crypto." ^ name ^ ": internal error, invalid hash") ls)
      | _ -> builtin_fail ("Crypto." ^ name) ls

    let eq_type = tfun_typ "'A" (fun_typ (tvar "'A") @@ fun_typ (tvar "'A") bool_typ)
    let eq_arity = 2    
    let eq_elab sc ts =
      match ts with
      | [bstyp1; bstyp2] when
          (* We want both the types to be ByStr with equal width. *)
          is_bystrx_type bstyp1 && is_bystrx_type bstyp2 && bstyp1 = bstyp2
        -> elab_tfun_with_args sc [bstyp1]
      | _ -> fail0 "Failed to elaborate"
    let eq ls _ = match ls with
      | [ByStrX (w1, x1); ByStrX(w2, x2)] ->
          pure @@ to_Bool (w1 = w2 && x1 = x2)
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

    let big_int_of_hash h =
      let s = match Hex.of_string h with
        | `Hex s -> s
      in Big_int.big_int_of_string s

    (* TODO: define for other ByStrX types? *)
    let dist_type = fun_typ (bystrx_typ hash_length) @@ fun_typ (bystrx_typ hash_length) uint128_typ
    let dist_arity = 2    
    let dist ls _ = match ls with
      | [ByStrX(_, x); ByStrX(_, y)] ->
          let i1 = big_int_of_hash x in
          let i2 = big_int_of_hash y in
          let two256 = power_big_int_positive_big_int (big_int_of_int 2) (big_int_of_int 256) in
          let i1' = mod_big_int i1 two256 in
          let i2' = mod_big_int i2 two256 in
          let dist = abs_big_int (sub_big_int i1' i2') in
          let i = build_prim_literal uint256_typ (string_of_big_int dist) in
          (match i with
           | Some ui -> pure ui
           | None -> builtin_fail "Crypto.dist: Error building Uint256 from hash distance" ls
          )
      | _ -> builtin_fail "Crypto.dist" ls

    (* ByStrX -> ByStr *)
    let to_bystr_type = tfun_typ "'A" @@ fun_typ (tvar "'A") bystr_typ
    let to_bystr_arity = 1
    let to_bystr_elab sc ts = match ts with
      | [t] when is_bystrx_type t -> elab_tfun_with_args sc ts
      | _ -> fail0 "Failed to elaborate"
    let to_bystr ls _ = match ls with
      | [ByStrX(_, s)] -> 
        let res = build_prim_literal bystr_typ s in
        (match res with
         | Some l' -> pure l'
         | None -> builtin_fail "Crypto.to_bystr: internal error" ls)
      | _ -> builtin_fail "Crypto.to_bystr" ls

    (* ByStrX + ByStrY -> ByStr(X+Y)*)
    let concat_type = tfun_typ "'A" @@ tfun_typ "'B" @@ tfun_typ "'C" @@
                      fun_typ (tvar "'A") (fun_typ (tvar "'B") (tvar "'C"))
    let concat_arity = 2
    let concat_elab sc ts = match ts with
      | [t1;t2] when is_bystrx_type t1 && is_bystrx_type t2 ->
        let t1w = BatOption.get (bystrx_width t1) in
        let t2w = BatOption.get (bystrx_width t2) in
        elab_tfun_with_args sc (ts @ [(bystrx_typ (t1w+t2w))])
      | _ -> fail0 "Failed to elaborate"
    let concat ls _ = match ls with
      | [ByStrX(w1, s1);ByStrX(w2, s2)] -> 
        let res = build_prim_literal 
          (bystrx_typ (w1+w2)) 
          (s1 ^ (Core.String.sub s2 ~pos:2 ~len:((Core.String.length s2) - 2))) in
        (match res with
         | Some l' -> pure l'
         | None -> builtin_fail "Crypto.concat: internal error" ls)
      | _ -> builtin_fail "Crypto.bystr" ls


    let ec_gen_key_pair_type = fun_typ unit_typ (pair_typ (bystrx_typ privkey_len) (bystrx_typ pubkey_len))
    let ec_gen_key_pair_arity = 0  
    let ec_gen_key_pair ls _ =
      match ls with
      | [] ->
        let privK, pubK = genKeyPair () in
        let privK_lit_o = build_prim_literal (bystrx_typ privkey_len) privK in
        let pubK_lit_o = build_prim_literal (bystrx_typ pubkey_len) pubK in
        (match privK_lit_o, pubK_lit_o with
        | Some privK', Some pubK' -> pair_lit privK' pubK'
        | _ -> builtin_fail "ec_gen_key_pair: internal error, invalid private/public key(s)." ls)
      | _ -> builtin_fail "ec_gen_key_pair" ls

    let schnorr_sign_type = fun_typ (bystrx_typ privkey_len) @@ (* private key *)
                            fun_typ (bystrx_typ pubkey_len) @@ (* public key *)
                            fun_typ (bystr_typ) @@ (* message to be signed *)
                            (bystrx_typ signature_len) (* signature *)
    let schnorr_sign_arity = 3
    let schnorr_sign ls _ =
      match ls with
      | [ByStrX(privklen, privkey); ByStrX(pubklen, pubkey); ByStr(msg)]
          when privklen = privkey_len && pubklen = pubkey_len ->
        let s = sign privkey pubkey msg in
        let s' = build_prim_literal (bystrx_typ signature_len) s in
        (match s' with
        | Some s'' -> pure s''
        | None -> builtin_fail "schnorr_sign: internal error, invalid signature." ls)
      | _ -> builtin_fail "schnorr_sign" ls

    let schnorr_verify_type = fun_typ (bystrx_typ pubkey_len) @@ (* public key *)
                              fun_typ (bystr_typ) @@ (* signed message *)
                              fun_typ (bystrx_typ signature_len) @@ (* signature *)
                              bool_typ
    let schnorr_verify_arity = 3
    let schnorr_verify ls _ =
      match ls with
      | [ByStrX(pubklen, pubkey); ByStr(msg); ByStrX(siglen, signature)]
          when siglen = signature_len && pubklen = pubkey_len ->
        let v = verify pubkey msg signature in
        pure @@ to_Bool v
      | _ -> builtin_fail "schnorr_verify" ls

    let ecdsa_sign_type = fun_typ (bystrx_typ Secp256k1Wrapper.privkey_len) @@ (* private key *)
                            fun_typ (bystr_typ) @@ (* message to be signed *)
                            (bystrx_typ Secp256k1Wrapper.signature_len) (* signature *)
    let ecdsa_sign_arity = 2
    let ecdsa_sign ls _ =
      let open Secp256k1Wrapper in
      match ls with
      | [ByStrX(privklen, privkey); ByStr(msg)]
          when privklen = privkey_len ->
        let%bind s = sign privkey msg in
        let s' = build_prim_literal (bystrx_typ signature_len) s in
        (match s' with
        | Some s'' -> pure s''
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
      | [ByStrX(pubklen, pubkey); ByStr(msg); ByStrX(siglen, signature)]
          when siglen = signature_len && pubklen = pubkey_len ->
        let%bind v = verify pubkey msg signature in
        pure @@ to_Bool v
      | _ -> builtin_fail "ecdsa_verify" ls

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
          let nil = ADTValue ("Nil", (otyp::[]), []) in
          let ol = Caml.Hashtbl.fold
              (fun k v accum ->
               let kv = ADTValue ("Pair", (kt::vt::[]), k::v::[]) in
               let kvl = ADTValue ("Cons", (otyp::[]), (kv::accum::[])) in
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
       * built-in name
       * arity
       * full, unelaborated type
       * elaborator, refining the type based on argument 
         to support polymorphism -- e.g., for ints and maps
       * executor - operational semantics of the built-in
    *)
    type built_in_record = string * int * typ * elaborator * built_in_executor

    (* All built-in functions *)
    let built_in_dict : built_in_record list = [
      (* Strings *)
      ("eq", String.eq_arity, String.eq_type, elab_id, String.eq);
      ("concat", String.concat_arity, String.concat_type, elab_id, String.concat);
      ("substr", String.substr_arity, String.substr_type, String.substr_elab, String.substr);
      ("to_string", String.to_string_arity, String.to_string_type, String.to_string_elab, String.to_string);

      (* Block numbers *)
      ("eq", BNum.eq_arity, BNum.eq_type, elab_id , BNum.eq);
      ("blt", BNum.blt_arity, BNum.blt_type, elab_id , BNum.blt);
      ("badd", BNum.badd_arity, BNum.badd_type, BNum.badd_elab , BNum.badd);
      ("bsub", BNum.bsub_arity, BNum.bsub_type, BNum.bsub_elab , BNum.bsub);

      (* Crypto *)
      ("eq", Crypto.eq_arity, Crypto.eq_type, Crypto.eq_elab, Crypto.eq);
      ("dist", Crypto.dist_arity, Crypto.dist_type, elab_id , Crypto.dist);
      ("sha256hash", Crypto.hash_arity, Crypto.hash_type,Crypto.hash_elab, Crypto.sha256hash);
      ("keccak256hash", Crypto.hash_arity, Crypto.hash_type,Crypto.hash_elab, Crypto.keccak256hash);
      ("ripemd160hash", Crypto.hash_arity, Crypto.ripemd160hash_type,Crypto.hash_elab, Crypto.ripemd160hash);
      ("to_bystr", Crypto.to_bystr_arity, Crypto.to_bystr_type, Crypto.to_bystr_elab, Crypto.to_bystr);
      ("ec_gen_key_pair", Crypto.ec_gen_key_pair_arity, Crypto.ec_gen_key_pair_type, elab_id, Crypto.ec_gen_key_pair);
      ("schnorr_sign", Crypto.schnorr_sign_arity, Crypto.schnorr_sign_type, elab_id, Crypto.schnorr_sign);
      ("schnorr_verify", Crypto.schnorr_verify_arity, Crypto.schnorr_verify_type, elab_id, Crypto.schnorr_verify);
      ("ecdsa_sign", Crypto.ecdsa_sign_arity, Crypto.ecdsa_sign_type, elab_id, Crypto.ecdsa_sign);
      ("ecdsa_verify", Crypto.ecdsa_verify_arity, Crypto.ecdsa_verify_type, elab_id, Crypto.ecdsa_verify);
      ("concat", Crypto.concat_arity, Crypto.concat_type, Crypto.concat_elab, Crypto.concat);

      (* Maps *)
      ("contains", Maps.contains_arity, Maps.contains_type, Maps.contains_elab, Maps.contains);
      ("put", Maps.put_arity, Maps.put_type, Maps.put_elab, Maps.put);
      ("get", Maps.get_arity, Maps.get_type, Maps.get_elab, Maps.get);
      ("remove", Maps.remove_arity, Maps.remove_type, Maps.remove_elab, Maps.remove);
      ("to_list", Maps.to_list_arity, Maps.to_list_type, Maps.to_list_elab, Maps.to_list);
      ("size", Maps.size_arity, Maps.size_type, Maps.size_elab, Maps.size);

      (* Integers *)
      ("eq", Int.eq_arity, Int.eq_type, Int.eq_elab, Int.eq);
      ("lt", Int.eq_arity, Int.eq_type, Int.eq_elab, Int.lt);
      ("add", Int.binop_arity, Int.binop_type, Int.binop_elab, Int.add);
      ("sub", Int.binop_arity, Int.binop_type, Int.binop_elab, Int.sub);
      ("mul", Int.binop_arity, Int.binop_type, Int.binop_elab, Int.mul);
      ("div", Int.binop_arity, Int.binop_type, Int.binop_elab, Int.div);
      ("rem", Int.binop_arity, Int.binop_type, Int.binop_elab, Int.rem);
      ("pow", Int.pow_arity, Int.pow_type, Int.pow_elab, Int.pow);
      ("to_int32", Int.to_int_arity, Int.to_int_type, Int.to_int_elab 32, Int.to_int32);
      ("to_int64", Int.to_int_arity, Int.to_int_type, Int.to_int_elab 64, Int.to_int64);
      ("to_int128", Int.to_int_arity, Int.to_int_type, Int.to_int_elab 128, Int.to_int128);
      ("to_int256", Int.to_int_arity, Int.to_int_type, Int.to_int_elab 256, Int.to_int256);

      (* Unsigned integers *)
      ("eq", Uint.eq_arity, Uint.eq_type, Uint.eq_elab, Uint.eq);
      ("lt", Uint.eq_arity, Uint.eq_type, Uint.eq_elab, Uint.lt);
      ("add", Uint.binop_arity, Uint.binop_type, Uint.binop_elab, Uint.add);
      ("sub", Uint.binop_arity, Uint.binop_type, Uint.binop_elab, Uint.sub);
      ("mul", Uint.binop_arity, Uint.binop_type, Uint.binop_elab, Uint.mul);
      ("div", Uint.binop_arity, Uint.binop_type, Uint.binop_elab, Uint.div);
      ("rem", Uint.binop_arity, Uint.binop_type, Uint.binop_elab, Uint.rem);
      ("pow", Uint.pow_arity, Uint.pow_type, Uint.pow_elab, Uint.pow);
      ("to_uint32", Uint.to_uint_arity, Uint.to_uint_type, Uint.to_uint_elab 32, Uint.to_uint32);
      ("to_uint64", Uint.to_uint_arity, Uint.to_uint_type, Uint.to_uint_elab 64, Uint.to_uint64);
      ("to_uint128", Uint.to_uint_arity, Uint.to_uint_type, Uint.to_uint_elab 128, Uint.to_uint128);
      ("to_uint256", Uint.to_uint_arity, Uint.to_uint_type, Uint.to_uint_elab 256, Uint.to_uint256);
      ("to_nat", Uint.to_nat_arity, Uint.to_nat_type, Uint.to_nat_elab, Uint.to_nat);
    ]

    let built_in_hashtbl =
      let open Caml in
      let ht : ((string, built_in_record list) Hashtbl.t) = Hashtbl.create 64 in
      List.iter (fun row ->
          let (opname, _, _, _, _) = row in
          match Hashtbl.find_opt ht opname with
          | Some p ->  Hashtbl.add ht opname (row::p)
          | None -> Hashtbl.add ht opname (row::[])
        ) built_in_dict;
      ht
      
    (* Dictionary lookup based on the operation name and type *)
    let find_builtin_op op argtypes =
      let opname = get_id op in
      let finder = (function (name, arity, optype, elab, exec) ->
          if name = opname && arity = List.length argtypes
          then
            (* First: elaborate based on argument types *)
            let%bind type_elab = elab optype argtypes in
            (* Second: check applicability *)
            let%bind res_type = fun_type_applies type_elab argtypes in
            pure (type_elab, res_type, exec)
          else fail0 @@ "Name or arity don't match") in
      let open Caml in
      let dict = Option.value ~default:[] @@ Hashtbl.find_opt built_in_hashtbl opname in
      let%bind (_, (type_elab, res_type, exec)) = tryM dict ~f:finder
          ~msg:(fun () ->
              mk_error1
                (sprintf "Cannot find built-in with name \"%s\" and argument types %s." opname (pp_typ_list argtypes))
                (ER.get_loc (get_rep op)))
      in pure (type_elab, res_type, exec)

  end

end
