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
    fail @@ sprintf "Cannot apply built-in %s to a list of arguments:%s."
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
      | _ -> fail "Failed to elaborate"

    let substr ls _ = match ls with
      | [StringLit x; UintLit (Uint32L s); UintLit (Uint32L e)] ->
          pure @@ StringLit (Core.String.sub x ~pos:(Uint32.to_int s) ~len:(Uint32.to_int e))
      | _ -> builtin_fail "String.substr" ls
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
      | _ -> fail "Failed to elaborate"

    let binop_arity = 2
    let binop_type = tfun_typ "'A" (fun_typ (tvar "'A") @@ fun_typ (tvar "'A") (tvar "'A"))
    let binop_elab t ts = match ts with
      | [i1; i2] when i1 = i2 && is_int_type i1 -> elab_tfun_with_args t [i1]
      | _ -> fail "Failed to elaborate"

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
      | _ -> fail "Failed to convert" 

    let to_int_arity = 1
    let to_int_type = tfun_typ "'A" @@ tfun_typ "'B" (fun_typ (tvar "'A") (tvar "'B"))
    let to_int_elab w sc ts = match ts with
      | [t] when is_int_type t || is_uint_type t ->
          let%bind ityp = mk_int_type w in
          elab_tfun_with_args sc [t; ityp]
      | _ -> fail "Failed to elaborate"

    let to_int_helper ls w = 
      let%bind xs = match ls with
        | [IntLit x] -> pure @@ string_of_int_lit x
        | [UintLit x] -> pure @@ string_of_uint_lit x
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
      | _ -> fail "Failed to elaborate"

    let binop_arity = 2
    let binop_type = tfun_typ "'A" (fun_typ (tvar "'A") @@ fun_typ (tvar "'A") (tvar "'A"))
    let binop_elab sc ts = match ts with
      | [i1; i2] when i1 = i2 && is_uint_type i1 -> elab_tfun_with_args sc [i1]
      | _ -> fail "Failed to elaborate"

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
      | _ -> fail "Failed to convert" 

    let to_uint_arity = 1
    let to_uint_type = tfun_typ "'A" @@ tfun_typ "'B"
        (fun_typ (tvar "'A") (option_typ (tvar "'B")))

    let to_uint_elab w sc ts = match ts with
      | [t] when is_uint_type t || is_int_type t ->
          let%bind ityp = mk_uint_type w in
          elab_tfun_with_args sc [t; ityp]
      | _ -> fail "Failed to elaborate"

    let to_uint_helper ls w = 
      let%bind xs = match ls with
        | [IntLit x] -> pure @@ string_of_int_lit x
        | [UintLit x] -> pure @@ string_of_uint_lit x
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
      | _ -> fail "Failed to elaborate"

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
      | _ -> fail "Failed to elaborate"

    let badd ls _ = match ls with
      | [BNum x; UintLit y] ->
          let i1 = big_int_of_string x in
          let i2 = big_int_of_string (string_of_uint_lit y) in
          if ge_big_int i2 (big_int_of_int 0)
          then pure @@ BNum (string_of_big_int (add_big_int i1 i2))
          else fail @@ sprintf
              "Cannot add a negative value (%s) to a block." (string_of_uint_lit y)
      | _ -> builtin_fail "BNum.badd" ls

  end

  (***********************************************************)
  (******************** Byte Strings *************************)
  (***********************************************************)
  module Hashing = struct
    open UsefulLiterals
    open Cryptokit
    open PrimTypes
    open Datatypes.DataTypeDictionary
    open Schnorr

    (* let hex s = transform_string (Hexa.decode()) s *)
    let tohex s = transform_string (Hexa.encode()) s
    let hash s = hash_string (Hash.sha2 256) s

    let eq_type = tfun_typ "'A" (fun_typ (tvar "'A") @@ fun_typ (tvar "'A") bool_typ)
    let eq_arity = 2    
    let eq_elab sc ts =
      match ts with
      | [bstyp1; bstyp2] when
          (* We want both the types to be ByStr with equal width. *)
          is_bystrx_type bstyp1 && is_bystrx_type bstyp2 && bstyp1 = bstyp2
        -> elab_tfun_with_args sc [bstyp1]
      | _ -> fail "Failed to elaborate"
    let eq ls _ = match ls with
      | [ByStrX (w1, x1); ByStrX(w2, x2)] ->
          pure @@ to_Bool (w1 = w2 && x1 = x2)
      | _ -> builtin_fail "Hashing.eq" ls

    let hash_type = tfun_typ "'A" @@ fun_typ (tvar "'A") (bystrx_typ hash_length)
    let hash_arity = 1
    let hash_elab sc ts = match ts with
      | [_]  -> elab_tfun_with_args sc ts
      | _ -> fail "Failed to elaborate"
    let sha256hash ls _ = match ls with
      | [l] ->
          let lstr = sexp_of_literal l |> Sexplib.Sexp.to_string in
          let lhash = hash lstr in
          let lhash_hex = "0x" ^ tohex lhash in 
          let lo = build_prim_literal (bystrx_typ hash_length) lhash_hex in
          (match lo with
          | Some l' -> pure @@ l'
          | None -> builtin_fail "Hashing.sha256hash: internal error, invalid sha256 hash" ls)
      | _ -> builtin_fail "Hashing.sha256hash" ls

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
           | None -> builtin_fail "Hashing.dist: Error building Uint256 from hash distance" ls
          )
      | _ -> builtin_fail "Hashing.dist" ls

    (* ByStrX -> ByStr *)
    let to_bystr_type = tfun_typ "'A" @@ fun_typ (tvar "'A") bystr_typ
    let to_bystr_arity = 1
    let to_bystr_elab sc ts = match ts with
      | [t] when is_bystrx_type t -> elab_tfun_with_args sc ts
      | _ -> fail "Failed to elaborate"
    let to_bystr ls _ = match ls with
      | [ByStrX(_, s)] -> 
        let res = build_prim_literal bystr_typ s in
        (match res with
         | Some l' -> pure l'
         | None -> builtin_fail "Hashing.to_bystr: internal error" ls)
      | _ -> builtin_fail "Hashing.to_bystr" ls

    let schnorr_gen_key_pair_type = fun_typ unit_typ (pair_typ (bystrx_typ privkey_len) (bystrx_typ pubkey_len))
    let schnorr_gen_key_pair_arity = 0  
    let schnorr_gen_key_pair ls _ =
      match ls with
      | [] ->
        let privK, pubK = genKeyPair () in
        let privK_lit_o = build_prim_literal (bystrx_typ privkey_len) privK in
        let pubK_lit_o = build_prim_literal (bystrx_typ pubkey_len) pubK in
        (match privK_lit_o, pubK_lit_o with
        | Some privK', Some pubK' -> pair_lit privK' pubK'
        | _ -> builtin_fail "schnorr_gen_key_pair: internal error, invalid private/public key(s)." ls)
      | _ -> builtin_fail "schnorr_gen_key_pair" ls

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

  end

  (***********************************************************)
  (* Maps *)
  (***********************************************************)
  module Maps = struct
    open UsefulLiterals
    open Datatypes.DataTypeDictionary

    let contains_arity = 2
    let contains_type =
      tfun_typ "'K" @@ tfun_typ "'V" @@
      (fun_typ (map_typ (tvar "'K") (tvar "'V")) @@ fun_typ (tvar "'K") bool_typ)
    let contains_elab sc ts = match ts with
      | [MapType (kt, vt); u] when kt = u  ->
          elab_tfun_with_args sc [kt; vt]
      | _ -> fail "Failed to elaborate"
    let contains ls _ = match ls with
      | [Map (_, entries); key] ->
          let res = List.exists entries ~f:(fun (k, _) -> k = key) in
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
      | _ -> fail "Failed to elaborate"
    let put ls _ = match ls with
      | [Map (tm, entries); key; value] ->
          let filtered =
            List.filter entries ~f:(fun (k, _) -> k <> key) in
          pure @@ Map (tm, ((key, value) :: filtered)) 
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
      | _ -> fail "Failed to elaborate"
    (* Notice that get passes return type *)
    let get ls rt = match ls, rt with
      | [Map (_, entries); key], ADT ("Option", [targ]) ->
          let res = List.find entries ~f:(fun (k, _) -> k = key) in
          (match res with
           | None -> pure @@ none_lit targ
           | Some (_, v) -> some_lit v)
      | _ -> builtin_fail "Map.get" ls

    let remove_arity = 2
    let remove_type =
      tfun_typ "'K" @@ tfun_typ "'V" @@
      (fun_typ (map_typ (tvar "'K") (tvar "'V")) @@
       fun_typ (tvar "'K") (map_typ (tvar "'K") (tvar "'V")))
    let remove_elab sc ts = match ts with
      | [MapType (kt, vt); u] when kt = u  ->
          elab_tfun_with_args sc [kt; vt]
      | _ -> fail "Failed to elaborate" 
    let remove ls _ = match ls with
      | [Map (tm, entries); key] ->
          let res = List.filter entries ~f:(fun (k, _) -> k <> key) in
          pure @@ Map (tm, res)
      | _ -> builtin_fail "Map.remove" ls


    let to_list_arity = 1
    let to_list_type =
      tfun_typ "'K" @@ tfun_typ "'V" @@
      (fun_typ (map_typ (tvar "'K") (tvar "'V")) @@
       (list_typ (pair_typ (tvar "'K") (tvar "'V"))))
    let to_list_elab sc ts = match ts with
      | [MapType (kt, vt)]  -> elab_tfun_with_args sc [kt; vt]
      | _ -> fail "Failed to elaborate" 
    let to_list ls _ = match ls with
      | [Map ((kt, vt), entries)] ->
          (* The type of the output list will be "Pair (kt) (vt)" *)
          let otyp = pair_typ kt vt in
          let nil = ADTValue ("Nil", (otyp::[]), []) in
          let ol = List.fold_left entries ~init:nil
              ~f: (fun accum (k, v) ->
                  let kv = ADTValue ("Pair", (kt::vt::[]), k::v::[]) in
                  let kvl = ADTValue ("Cons", (otyp::[]), (kv::accum::[]))
                  in kvl)
          in pure (ol)
      | _ -> builtin_fail "Map.to_list" ls

  end


  (* Identity elaborator *)
  let elab_id = fun t _ -> pure t

  (**********************************************************)
  (*                   Built-in  Dictionary                *)
  (**********************************************************)

  module BuiltInDictionary = struct 

    (* Elaborates the operation type based on the arguments types *)
    type elaborator = typ -> typ list -> (typ, string) result      

    (* Takes the expected type as an argument to elaborate the result *)
    type built_in_executor = literal list -> typ -> (literal, string) result

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

      (* Block numbers *)
      ("eq", BNum.eq_arity, BNum.eq_type, elab_id , BNum.eq);
      ("blt", BNum.blt_arity, BNum.blt_type, elab_id , BNum.blt);
      ("badd", BNum.badd_arity, BNum.badd_type, BNum.badd_elab , BNum.badd);

      (* Hashes *)
      ("eq", Hashing.eq_arity, Hashing.eq_type, Hashing.eq_elab, Hashing.eq);
      ("dist", Hashing.dist_arity, Hashing.dist_type, elab_id , Hashing.dist);
      ("sha256hash", Hashing.hash_arity, Hashing.hash_type,Hashing.hash_elab, Hashing.sha256hash);
      ("to_bystr", Hashing.to_bystr_arity, Hashing.to_bystr_type, Hashing.to_bystr_elab, Hashing.to_bystr);
      ("schnorr_gen_key_pair", Hashing.schnorr_gen_key_pair_arity, Hashing.schnorr_gen_key_pair_type, elab_id, Hashing.schnorr_gen_key_pair);
      ("schnorr_sign", Hashing.schnorr_sign_arity, Hashing.schnorr_sign_type, elab_id, Hashing.schnorr_sign);
      ("schnorr_verify", Hashing.schnorr_verify_arity, Hashing.schnorr_verify_type, elab_id, Hashing.schnorr_verify);

      (* Maps *)
      ("contains", Maps.contains_arity, Maps.contains_type, Maps.contains_elab, Maps.contains);
      ("put", Maps.put_arity, Maps.put_type, Maps.put_elab, Maps.put);
      ("get", Maps.get_arity, Maps.get_type, Maps.get_elab, Maps.get);
      ("remove", Maps.remove_arity, Maps.remove_type, Maps.remove_elab, Maps.remove);
      ("to_list", Maps.to_list_arity, Maps.to_list_type, Maps.to_list_elab, Maps.to_list);

      (* Integers *)
      ("eq", Int.eq_arity, Int.eq_type, Int.eq_elab, Int.eq);
      ("lt", Int.eq_arity, Int.eq_type, Int.eq_elab, Int.lt);
      ("add", Int.binop_arity, Int.binop_type, Int.binop_elab, Int.add);
      ("sub", Int.binop_arity, Int.binop_type, Int.binop_elab, Int.sub);
      ("mul", Int.binop_arity, Int.binop_type, Int.binop_elab, Int.mul);
      ("div", Int.binop_arity, Int.binop_type, Int.binop_elab, Int.div);
      ("rem", Int.binop_arity, Int.binop_type, Int.binop_elab, Int.rem);
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
          else fail @@ "Name or arity don't match") in
      let open Caml in
      let dict = Option.value ~default:[] @@ Hashtbl.find_opt built_in_hashtbl opname in
      let%bind (_, (type_elab, res_type, exec)) = tryM dict ~f:finder
          ~msg:(sprintf "[%s] Cannot find built-in with name \"%s\" and argument types %s."
                  (ER.get_loc (get_rep op) |> get_loc_str) opname (pp_typ_list argtypes))
      in pure (type_elab, res_type, exec)

  end

end
