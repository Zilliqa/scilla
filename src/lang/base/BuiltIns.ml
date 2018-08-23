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
open MonadUtil.Let_syntax
open Big_int
open Stdint
open TypeUtil
open Integer256

exception IntOverflow
exception IntUnderflow

let print_literal_list ls =
  let ps = List.map ls
      ~f:(fun l -> sexp_of_literal l |> Sexplib.Sexp.to_string) in
  String.concat ~sep:",\n " ps

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

  let to_Bool b = if b then true_lit else false_lit
end

(* Validate Int* and Uint* literals (wx, x), whether the
   string x they contain can be represented in wx bits  *)
let validate_int_literal i =
  try
    match i with
    | IntLit (wx, x) ->
      (match wx with
      | 32 -> Int32.to_string (Int32.of_string x) = x
      | 64 -> Int64.to_string (Int64.of_string x) = x
      | 128 -> Int128.to_string (Int128.of_string x) = x
      | 256 -> Int256.to_string (Int256.of_string x) = x
      | _ -> false
      )
    | UintLit (wx, x) ->
      (match wx with
      | 32 -> Uint32.to_string (Uint32.of_string x) = x
      | 64 -> Uint64.to_string (Uint64.of_string x) = x
      | 128 -> Uint128.to_string (Uint128.of_string x) = x
      | 256 -> Uint256.to_string (Uint256.of_string x) = x
      | _ -> false
      )
    | _ -> false
  with
  | _ -> false

(* Given an integer type (as string) and the value (as string),
   build IntLit or UintLit out of it. *)
let build_int t v =
  let open PrimTypes in 
  let validator_wrapper l = 
    if validate_int_literal l then Some l else None
  in
  match t with
  | x when x = int32_typ   -> validator_wrapper (IntLit(32, v))
  | x when x = int64_typ   -> validator_wrapper (IntLit(64, v))
  | x when x = int128_typ  -> validator_wrapper (IntLit(128, v))
  | x when x = int256_typ  -> validator_wrapper (IntLit(256, v))
  | x when x = uint32_typ  -> validator_wrapper (UintLit(32, v))
  | x when x = uint64_typ  -> validator_wrapper (UintLit(64, v))
  | x when x = uint128_typ -> validator_wrapper (UintLit(128, v))
  | x when x = uint256_typ -> validator_wrapper (UintLit(256, v))
  | _ -> None

let is_int_type = function
  | x when x = PrimTypes.int32_typ ||
           x = PrimTypes.int64_typ ||
           x = PrimTypes.int128_typ ||
           x = PrimTypes.int256_typ -> true
  | _ -> false

let is_uint_type = function
  | x when x = PrimTypes.uint32_typ ||
           x = PrimTypes.uint64_typ ||
           x = PrimTypes.uint128_typ ||
           x = PrimTypes.uint256_typ -> true
  | _ -> false

(* Given an address string, build an Address literal,
   or return None on invalid address. *)
let build_address v =
  let v' = String.lowercase v in
  let s, re, l =
    v', Str.regexp "0x[0-9a-f]+$", address_length+2 in
  if Str.string_match re s 0 && String.length s = l
  then
    Some (Address v')
  else
    None

(* Given a hash string, build a hash literal,
   or return None on invalid hash. *)
let build_hash v =
  let v' = String.lowercase v in
  let s, re, l =
    v', Str.regexp "0x[0-9a-f]+$", hash_length+2 in
  if Str.string_match re s 0 && String.length s = l
  then
    Some (Sha256 v')
  else
    None

(* Given an integer string, build a BNum literal,
   or return None on invalid input. *)
let build_bnum v =
  let s, re =
    v, Str.regexp "[0-9]+$" in
  if Str.string_match re s 0
  then
    Some (BNum v)
  else
    None

let build_prim_literal t v =
  let open PrimTypes in
  match t with
  | x when x = string_typ -> Some (StringLit v)
  | x when x = bnum_typ -> build_bnum v
  | x when x = address_typ -> build_address v
  | x when x = hash_typ -> build_hash v
  | x when is_int_type x || is_uint_type x -> build_int t v
  | _ -> None

(*******************************************************)
(*******************************************************)
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
  | [StringLit x; UintLit (_,s); UintLit (_,e)] ->
      pure @@ StringLit (Core.String.sub x ~pos:(int_of_string s) ~len:(int_of_string e))
  | _ -> builtin_fail "String.substr" ls
end

(*******************************************************)
(* Manipulating with arbitrary integer representations *)
(*******************************************************)
module type IntRep = sig
  type t
  val compare : t -> t -> int
  val of_string : string -> t
  val to_string : t -> string
  val add: t -> t -> t
  val sub: t -> t -> t
  val mul: t -> t -> t
  val div: t -> t -> t
  val zero: t
  val min_int: t
end

module StdIntWrapper(R: IntRep) = struct
  open R
  
  let safe_add astr bstr =
    let a = of_string astr in
    let b = of_string bstr in
    let r = add a b in
    (* if a > 0 && b > 0 && r < 0 then we have an overflow*)
    if ((compare a zero) > 0 && (compare b zero) > 0 && (compare r zero) < 0) 
      then raise IntOverflow
    (* if a < 0 && b < 0 && r > 0 then we have an underflow*)
    else if ((compare a zero) < 0 && (compare b zero) < 0 && (compare r zero) > 0) 
      then raise IntUnderflow
    else
      to_string r

  let safe_sub astr bstr =
    let a = of_string astr in
    let b = of_string bstr in
    let r = R.sub a b in
    (* if a > 0 && b < 0 && r < 0 then we have an overflow *)
    if ((compare a zero) > 0 && (compare b zero) < 0 && (compare r zero) < 0) 
      then raise IntOverflow
    (* if a < 0 && b > 0 && r > 0 then we have an underflow *)
    else if ((compare a zero) < 0 && (compare b zero) > 0 && (compare r zero) > 0) 
      then raise IntUnderflow
    else
      to_string r

  let safe_mul astr bstr  =
    let a = of_string astr in
    let b = of_string bstr in
    let r = mul a b in
    (* http://www.informit.com/articles/article.aspx?p=1959565&seqNum=13 *)
    (* if b < 0 && a = int_min OR if b != 0 && r / b != a *)
    if (compare b zero) < 0 && a = min_int then raise IntOverflow else
    if compare b zero <> 0
    then
      let d = div r b in
      if (compare d a <> 0)
        then raise IntOverflow
      else
        to_string r
    else
      to_string r

  let safe_lt astr bstr =
    let a = of_string astr in
    let b = of_string bstr in
    if (compare a b) < 0 then true else false

end

module StdUintWrapper(R: IntRep) = struct
  open R

  let safe_add astr bstr =
    let a = of_string astr in
    let b = of_string bstr in
    let r = add a b in
    (* if r < a || r < b then we have an overflow *)
    if ((compare r a) < 0 || (compare r b) < 0)
      then raise IntOverflow
    else
      to_string r

  let safe_sub astr bstr =
    let a = of_string astr in
    let b = of_string bstr in
    let r = sub a b in
    (* if a < b then we have an underflow *)
    if (compare a b) < 0
      then raise IntUnderflow
    else
      to_string r

  let safe_mul astr bstr  =
    let a = of_string astr in
    let b = of_string bstr in
    let r = mul a b in
    (* if b != 0 && r / b != a *)
    if compare b zero <> 0
    then
      let d = div r b in
      if (compare d a <> 0)
        then raise IntOverflow
      else
        to_string r
    else
      to_string r

  let safe_lt astr bstr =
    let a = of_string astr in
    let b = of_string bstr in
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
    | [IntLit (wx, x); IntLit (wy, y)] ->
      if (wx <> wy) 
      then 
        builtin_fail "Int.eq" ls
      else
        pure @@ to_Bool (x = y)
    | _ -> builtin_fail "Int.eq" ls

  let add ls _ = match ls with
    | [IntLit (wx, x); IntLit (wy, y)] ->
      if (wx <> wy) then
        builtin_fail "Int.add: type mismatch" ls
      else
        (try match wx with
         | 32 ->
             pure (IntLit (wx, Int32Wrapper.safe_add x y))
         | 64 ->
             pure (IntLit (wx, Int64Wrapper.safe_add x y))
         | 128 ->
             pure (IntLit (wx, Int128Wrapper.safe_add x y))
         | 256 ->
             pure (IntLit (wx, Int256Wrapper.safe_add x y))
         | _ -> builtin_fail "Int.add: unsupported Int type" ls
         with | IntOverflow | IntUnderflow ->
           builtin_fail "Int.add: an overflow/underflow occurred" ls
        )
    | _ -> builtin_fail "Int.add" ls

  let sub ls _  = match ls with
    | [IntLit (wx, x); IntLit (wy, y)] ->
      if (wx <> wy) then
        builtin_fail "Int.sub: type mismatch" ls
      else
        (try match wx with
         | 32 ->
             pure (IntLit (wx, Int32Wrapper.safe_sub x y))
         | 64 ->
             pure (IntLit (wx, Int64Wrapper.safe_sub x y))
         | 128 ->
             pure (IntLit (wx, Int128Wrapper.safe_sub x y))
         | 256 ->
             pure (IntLit (wx, Int256Wrapper.safe_sub x y))
         | _ -> builtin_fail "Int.sub: unsupported Int type" ls
         with | IntOverflow | IntUnderflow ->
           builtin_fail "Int.sub: an overflow/underflow occurred" ls
        )
    | _ -> builtin_fail "Int.sub" ls

  let mul ls  _ = match ls with
    | [IntLit (wx, x); IntLit (wy, y)] ->
      if (wx <> wy) then
        builtin_fail "Int.mul: type mistmatch" ls
      else 
        (try match wx with
         | 32 ->
             pure (IntLit (wx, Int32Wrapper.safe_mul x y))
         | 64 ->
             pure (IntLit (wx, Int64Wrapper.safe_mul x y ))
         | 128 ->
             pure (IntLit (wx, Int128Wrapper.safe_mul x y))
         | 256 ->
             pure (IntLit (wx, Int256Wrapper.safe_mul x y))
         | _ -> builtin_fail "Int.mul: unsupported Int type" ls
         with | IntOverflow | IntUnderflow ->
           builtin_fail "Int.mul: an overflow/underflow occurred" ls
        )
    | _ -> builtin_fail "Int.mul" ls

  let lt ls _ = match ls with
    | [IntLit (wx, x); IntLit (wy, y)] ->
      if (wx <> wy) then
        builtin_fail "Int.lt: type mismatch" ls
      else 
        (try match wx with
        | 32 -> pure (to_Bool (Int32Wrapper.safe_lt x y))
        | 64 -> pure (to_Bool (Int64Wrapper.safe_lt x y))
        | 128 -> pure (to_Bool (Int128Wrapper.safe_lt x y))
        | 256 -> pure (to_Bool (Int256Wrapper.safe_lt x y))
        | _ -> builtin_fail "Int.lt: unsupported Int type" ls
        with | IntOverflow | IntUnderflow ->
          builtin_fail "Int.lt: an overflow/underflow occurred" ls
        )
    | _ -> builtin_fail "Int.lt" ls

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
  
  let to_int_helper ls w = match ls with
    | [UintLit (_, x)] | [IntLit(_, x)] ->
        let lit = IntLit (w, x) in
        let%bind ityp = mk_int_type w in
        if validate_int_literal lit
        then pure (ADTValue ("Some", [ityp], [lit]))
        else pure (ADTValue ("None", [ityp], []))
    | _ -> builtin_fail (sprintf "Int.to_int%i" w) ls

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
    | [UintLit (wx, x); UintLit (wy, y)] ->
      if (wx <> wy) 
      then 
        builtin_fail "Uint.eq" ls
      else
        pure @@ to_Bool (x = y)
    | _ -> builtin_fail "Uint.eq" ls

  let add ls _ = match ls with
    | [UintLit (wx, x); UintLit (wy, y)] ->
      if (wx <> wy) then
        builtin_fail "Uint.add: type mismatch" ls
      else
        (try match wx with
        | 32 -> pure (UintLit (wx, Uint32Wrapper.safe_add x y))
        | 64 -> pure (UintLit (wx, Uint64Wrapper.safe_add x y))
        | 128 -> pure (UintLit (wx, Uint128Wrapper.safe_add x y))
        | 256 -> pure (UintLit (wx, Uint256Wrapper.safe_add x y))
        | _ -> builtin_fail "Uint.add: unsupported Uint type" ls
        with | IntOverflow | IntUnderflow ->
          builtin_fail "Uint.add: an overflow/underflow occurred" ls
        )
    | _ -> builtin_fail "Uint.add" ls

  let sub ls _ = match ls with
    | [UintLit (wx, x); UintLit (wy, y)] ->
      if (wx <> wy) then
        builtin_fail "Uint.sub: type mismatch" ls
      else
        (try match wx with
        | 32 -> pure (UintLit (wx, Uint32Wrapper.safe_sub x y))
        | 64 -> pure (UintLit (wx, Uint64Wrapper.safe_sub x y))
        | 128 -> pure (UintLit (wx, Uint128Wrapper.safe_sub x y))
        | 256 -> pure (UintLit (wx, Uint256Wrapper.safe_sub x y))
        | _ -> builtin_fail "Uint.sub: unsupported Uint type" ls
        with | IntOverflow | IntUnderflow ->
          builtin_fail "Uint.sub: an overflow/underflow occurred" ls
        )
    | _ -> builtin_fail "Uint.sub" ls

  let mul ls _ = match ls with
    | [UintLit (wx, x); UintLit (wy, y)] ->
      if (wx <> wy) then
        builtin_fail "Uint.mul: type mistmatch" ls
      else 
        (try match wx with
        | 32 -> pure (UintLit (wx, Uint32Wrapper.safe_mul x y))
        | 64 -> pure (UintLit (wx, Uint64Wrapper.safe_mul  x y ))
        | 128 -> pure (UintLit (wx, Uint128Wrapper.safe_mul  x y))
        | 256 -> pure (UintLit (wx, Uint256Wrapper.safe_mul  x y))
        | _ -> builtin_fail "Uint.mul: unsupported Uint type" ls
        with | IntOverflow | IntUnderflow ->
          builtin_fail "Uint.mul: an overflow/underflow occurred" ls
        )
    | _ -> builtin_fail "Uint.mul" ls  

  let lt ls _ = match ls with
    | [UintLit (wx, x); UintLit (wy, y)] -> 
      if (wx <> wy) then
        builtin_fail "Uint.lt: type mismatch" ls
      else 
        (try match wx with
        | 32 -> pure (to_Bool (Uint32Wrapper.safe_lt x y))
        | 64 -> pure (to_Bool (Uint64Wrapper.safe_lt x y))
        | 128 -> pure (to_Bool (Uint128Wrapper.safe_lt x y))
        | 256 -> pure (to_Bool (Uint256Wrapper.safe_lt x y))
        | _ -> builtin_fail "Uint.lt: unsupported Uint type" ls
        with | IntOverflow | IntUnderflow ->
          builtin_fail "Uint.lt: an overflow/underflow occurred" ls
        )
    | _ -> builtin_fail "Uint.lt" ls

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

  let to_uint_helper ls w = match ls with
    | [UintLit (_, x)] | [IntLit(_, x)] ->
        let lit = UintLit (w, x) in
        let%bind ityp = mk_uint_type w in
        if validate_int_literal lit
        then pure (ADTValue ("Some", [ityp], [lit]))
        else pure (ADTValue ("None", [ityp], []))
    | _ -> builtin_fail (sprintf "Uint.to_uint%i" w) ls

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
  | [UintLit (wx, x)] ->
    (match wx with 
    | 32 -> 
      let zero = ADTValue ("Zero", [], []) in
      let n = Uint32.of_string x in
      let rec nat_builder (i : Uint32.t) =
        if i = Uint32.zero then zero
        else
          let prev = nat_builder (Uint32.sub i Uint32.one) in
            ADTValue ("Succ", [], (prev::[]))
      in
      pure (nat_builder n)
    (* Other integer widths can be in the library, using integer conversions. *)
    | _ -> builtin_fail "Uint.to_nat only supported for Uint32" ls)
  | _ -> builtin_fail "Uint.to_nat" ls

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
    | [BNum x; UintLit (_, y)] ->
        let i1 = big_int_of_string x in
        let i2 = big_int_of_string y in
        if ge_big_int i2 (big_int_of_int 0)
        then pure @@ BNum (string_of_big_int (add_big_int i1 i2))
        else fail @@ sprintf
            "Cannot add a negative value (%s) to a block." y
    | _ -> builtin_fail "BNum.badd" ls

end

(***********************************************************)
(* Working with addresses *)
(***********************************************************)
module Address = struct
  open UsefulLiterals
  open PrimTypes
  open Datatypes.DataTypeDictionary
         
  let eq_type = fun_typ address_typ @@ fun_typ address_typ bool_typ
  let eq_arity = 2    
  let eq ls _ = match ls with
    | [Address x; Address y] ->
        pure @@ to_Bool (x = y)
    | _ -> builtin_fail "Address.eq" ls
end

(***********************************************************)
(* Hashing *)
(***********************************************************)
module Hashing = struct
  open UsefulLiterals
  open Cryptokit
  open PrimTypes
  open Datatypes.DataTypeDictionary


  (* let hex s = transform_string (Hexa.decode()) s *)
  let tohex s = transform_string (Hexa.encode()) s
  let hash s = hash_string (Hash.sha2 256) s

  let eq_type = fun_typ hash_typ @@ fun_typ hash_typ bool_typ
  let eq_arity = 2    
  let eq ls _ = match ls with
    | [Sha256 x; Sha256 y] ->
        pure @@ to_Bool (x = y)
    | _ -> builtin_fail "Hashing.eq" ls

  let hash_type = tfun_typ "'A" @@ fun_typ (tvar "'A") hash_typ
  let hash_arity = 1
  let hash_elab sc ts = match ts with
    | [_]  -> elab_tfun_with_args sc ts
    | _ -> fail "Failed to elaborate"
  let sha256hash ls _ = match ls with
    | [l] ->
        let lstr = sexp_of_literal l |> Sexplib.Sexp.to_string in
        let lhash = hash lstr in
        let lhash_hex = "0x" ^ tohex lhash in 
        pure @@ Sha256 lhash_hex
    | _ -> builtin_fail "Hashing.sha256hash" ls

  let big_int_of_hash h =
    let s = match Hex.of_string h with
      | `Hex s -> s
    in Big_int.big_int_of_string s

  let dist_type = fun_typ hash_typ @@ fun_typ hash_typ uint128_typ
  let dist_arity = 2    
  let dist ls _ = match ls with
    | [Sha256 x; Sha256 y] ->
        let i1 = big_int_of_hash x in
        let i2 = big_int_of_hash y in
        let two256 = power_big_int_positive_big_int (big_int_of_int 2) (big_int_of_int 256) in
        let i1' = mod_big_int i1 two256 in
        let i2' = mod_big_int i2 two256 in
        let dist = abs_big_int (sub_big_int i1' i2') in
        let i = build_int uint256_typ (string_of_big_int dist) in
        (match i with
        | Some ui -> pure ui
        | None -> builtin_fail "Hashing.dist: Error building Uint256 from hash distance" ls
        )
    | _ -> builtin_fail "Hashing.dist" ls
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
  type elaborator = typ -> typ list -> (typ, string) eresult      

  (* Takes the expected type as an argument to elaborate the result *)
  type built_in_executor = literal list -> typ -> (literal, string) eresult

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

    (* Addresses *)
    ("eq", Address.eq_arity, Address.eq_type, elab_id, Address.eq);
    
    (* Block numbers *)
    ("eq", BNum.eq_arity, BNum.eq_type, elab_id , BNum.eq);
    ("blt", BNum.blt_arity, BNum.blt_type, elab_id , BNum.blt);
    ("badd", BNum.badd_arity, BNum.badd_type, BNum.badd_elab , BNum.badd);

    (* Hashes *)
    ("eq", Hashing.eq_arity, Hashing.eq_type, elab_id, Hashing.eq);
    ("dist", Hashing.dist_arity, Hashing.dist_type, elab_id , Hashing.dist);
    ("sha256hash", Hashing.hash_arity, Hashing.hash_type,Hashing.hash_elab, Hashing.sha256hash);

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
    ("to_uint32", Uint.to_uint_arity, Uint.to_uint_type, Uint.to_uint_elab 32, Uint.to_uint32);
    ("to_uint64", Uint.to_uint_arity, Uint.to_uint_type, Uint.to_uint_elab 64, Uint.to_uint64);
    ("to_uint128", Uint.to_uint_arity, Uint.to_uint_type, Uint.to_uint_elab 128, Uint.to_uint128);
    ("to_uint256", Uint.to_uint_arity, Uint.to_uint_type, Uint.to_uint_elab 256, Uint.to_uint256);
    ("to_nat", Uint.to_nat_arity, Uint.to_nat_type, Uint.to_nat_elab, Uint.to_nat);
  ]


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
    let%bind (_, (type_elab, res_type, exec)) = tryM built_in_dict ~f:finder
      ~msg:(sprintf "[%s] Cannot find built-in with name \"%s\" and argument types %s."
            (get_rep op |> get_loc_str) opname (pp_typ_list argtypes))
    in pure (type_elab, res_type, exec)
  
end
