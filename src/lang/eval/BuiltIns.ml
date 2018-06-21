(*
 * Copyright (c) 2018 - present Zilliqa, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open Syntax
open Core
open Stdint
open Result.Let_syntax
open MonadUtil
open Big_int
open Stdint

exception IntOverflow
exception IntUnderflow

let print_literal_list ls =
  let ps = List.map ls
      ~f:(fun l -> sexp_of_literal l |> Sexplib.Sexp.to_string) in
  String.concat ~sep:",\n " ps

let builtin_fail name ls =
  fail @@ sprintf "Cannot apply built-in %s to a list of arguments:%s."
    name (print_literal_list ls)

module StdIntWrapper = struct
type 'I gi = { compare : 'I -> 'I -> int;
               of_string : string ->'I;
               to_string : 'I -> string }
let int32gi = { compare = Int32.compare;
                of_string = Int32.of_string;
                to_string = Int32.to_string }
let int64gi = { compare = Int64.compare;
                of_string = Int64.of_string;
                to_string = Int64.to_string } 
let int128gi = { compare = Int128.compare;
                 of_string = Int128.of_string;
                 to_string = Int128.to_string }
let uint32gi = { compare = Uint32.compare;
                 of_string = Uint32.of_string;
                 to_string = Uint32.to_string }
let uint64gi = { compare = Uint64.compare;
                 of_string = Uint64.of_string;
                 to_string = Uint64.to_string } 
let uint128gi = { compare = Uint128.compare;
                  of_string = Uint128.of_string;
                  to_string = Uint128.to_string } 
end

module UsefulLiterals = struct
  let true_lit = ADTValue ("True", [], [])
  let false_lit = ADTValue ("False", [], [])

  (* TODO: implement typing! *)
  let some_lit l = ADTValue ("Some", [], [l])
  let none_lit = ADTValue ("None", [], [])

  let to_Bool b = if b then true_lit else false_lit
end

(* String operations *)
module String = struct
  open UsefulLiterals

  let eq ls = match ls with
  | [StringLit x; StringLit y] ->
    pure @@ to_Bool (x = y)
  | _ -> builtin_fail "String.eq" ls

  let concat ls = match ls with
  | [StringLit x; StringLit y] ->
    pure @@ StringLit (x ^ y)
  | _ -> builtin_fail "String.concat" ls

  let substr ls = match ls with
  | [StringLit x; UintLit (_,s); UintLit (_,e)] ->
      pure @@ StringLit (Core.String.sub x (int_of_string s) (int_of_string e))
  | _ -> builtin_fail "String.substr" ls
end

(* Integer operation *)
module Int = struct
  open UsefulLiterals

  open StdIntWrapper

  let safe_add (f : 'I gi) (add : 'I -> 'I -> 'I) (zero : 'I) astr bstr =
    let a = f.of_string astr in
    let b = f.of_string bstr in
    let r = add a b in
    (* if a > 0 && b > 0 && r < 0 then we have an overflow*)
    if ((compare a zero) > 0 && (compare b zero) > 0 && (compare r zero) < 0) 
      then raise IntOverflow
    (* if a < 0 && b < 0 && r > 0 then we have an underflow*)
    else if ((compare a zero) < 0 && (compare b zero) < 0 && (compare r zero) > 0) 
      then raise IntUnderflow
    else
      f.to_string r

  let safe_sub (f : 'I gi) (sub : 'I -> 'I -> 'I) (zero : 'I) astr bstr =
    let a = f.of_string astr in
    let b = f.of_string bstr in
    let r = sub a b in
    (* if a > 0 && b < 0 && r < 0 then we have an overflow *)
    if ((compare a zero) > 0 && (compare b zero) < 0 && (compare r zero) < 0) 
      then raise IntOverflow
    (* if a < 0 && b > 0 && r > 0 then we have an underflow *)
    else if ((compare a zero) < 0 && (compare b zero) > 0 && (compare r zero) > 0) 
      then raise IntUnderflow
    else
      f.to_string r

  let safe_mul (f : 'I gi) (mul : 'I -> 'I -> 'I) (abs : 'I -> 'I) astr bstr  =
    let a = f.of_string astr in
    let b = f.of_string bstr in
    let r = mul a b in
    (* if abr(r) < abs(a) || abs(r) < abs(b) then we have an overflow *)
    if ((compare (abs r) (abs a)) < 0 || (compare (abs r) (abs b)) < 0)
      then raise IntOverflow
    else
      f.to_string r

  let safe_lt (f : 'I gi) astr bstr =
    let a = f.of_string astr in
    let b = f.of_string bstr in
    if (compare a b) < 0 then true else false

  let eq ls = match ls with
    | [IntLit (wx, x); IntLit (wy, y)] ->
      if (wx <> wy) 
      then 
        builtin_fail "Int.eq" ls
      else
        pure @@ to_Bool (x = y)
    | _ -> builtin_fail "Int.eq" ls

  let add ls = match ls with
    | [IntLit (wx, x); IntLit (wy, y)] -> 
      if (wx <> wy) then
        builtin_fail "Int.add: type mismatch" ls
      else
        (match wx with
        | 32 -> pure (IntLit (wx, safe_add int32gi Int32.add Int32.zero x y))
        | 64 -> pure (IntLit (wx, safe_add int64gi Int64.add Int64.zero x y))
        | 128 -> pure (IntLit (wx, safe_add int128gi Int128.add Int128.zero x y))
        | _ -> builtin_fail "Int.add: unsupported Int type" ls
        )
    | _ -> builtin_fail "Int.add" ls

  let sub ls = match ls with
    | [IntLit (wx, x); IntLit (wy, y)] -> 
      if (wx <> wy) then
        builtin_fail "Int.sub: type mismatch" ls
      else
        (match wx with
        | 32 -> pure (IntLit (wx, safe_sub int32gi Int32.sub Int32.zero x y))
        | 64 -> pure (IntLit (wx, safe_sub int64gi Int64.sub Int64.zero x y))
        | 128 -> pure (IntLit (wx, safe_sub int128gi Int128.sub Int128.zero x y))
        | _ -> builtin_fail "Int.sub: unsupported Int type" ls
        )
    | _ -> builtin_fail "Int.sub" ls

  let mul ls = match ls with
    | [IntLit (wx, x); IntLit (wy, y)] -> 
      if (wx <> wy) then
        builtin_fail "Int.mul: type mistmatch" ls
      else 
        (match wx with
        | 32 -> pure (IntLit (wx, safe_mul int32gi Int32.mul Int32.abs x y))
        | 64 -> pure (IntLit (wx, safe_mul int64gi Int64.mul Int64.abs x y ))
        | 128 -> pure (IntLit (wx, safe_mul int128gi Int128.mul Int128.abs x y))
        | _ -> builtin_fail "Int.mul: unsupported Int type" ls
        )
    | _ -> builtin_fail "Int.mul" ls  

  let lt ls = match ls with
    | [IntLit (wx, x); IntLit (wy, y)] -> 
      if (wx <> wy) then
        builtin_fail "Int.lt: type mismatch" ls
      else 
        (match wx with
        | 32 -> pure (to_Bool (safe_lt int32gi x y))
        | 64 -> pure (to_Bool (safe_lt int64gi x y))
        | 128 -> pure (to_Bool (safe_lt int128gi x y))
        | _ -> builtin_fail "Int.lt: unsupported Int type" ls
        )
    | _ -> builtin_fail "Int.lt" ls  

end

(* Unsigned integer operation *)
module Uint = struct
  open UsefulLiterals
  open StdIntWrapper

  let safe_add (f : 'I gi) (add : 'I -> 'I -> 'I) astr bstr =
    let a = f.of_string astr in
    let b = f.of_string bstr in
    let r = add a b in
    (* if r < a || r < b then we have an overflow *)
    if ((compare r a) < 0 || (compare r b) < 0)
      then raise IntOverflow
    else
      f.to_string r

  let safe_sub (f : 'I gi) (sub : 'I -> 'I -> 'I) astr bstr =
    let a = f.of_string astr in
    let b = f.of_string bstr in
    let r = sub a b in
    (* if a < b then we have an underflow *)
    if (compare a b) < 0
      then raise IntUnderflow
    else
      f.to_string r

  let safe_mul (f : 'I gi) (mul : 'I -> 'I -> 'I) astr bstr  =
    let a = f.of_string astr in
    let b = f.of_string bstr in
    let r = mul a b in
    (* if r < a || r < b then we have an overflow *)
    if ((compare r a) < 0 || (compare r b) < 0)
      then raise IntOverflow
    else
      f.to_string r

  let safe_lt (f : 'I gi) astr bstr =
    let a = f.of_string astr in
    let b = f.of_string bstr in
    if (compare a b) < 0 then true else false

  let eq ls = match ls with
    | [UintLit (wx, x); UintLit (wy, y)] ->
      if (wx <> wy) 
      then 
        builtin_fail "Uint.eq" ls
      else
        pure @@ to_Bool (x = y)
    | _ -> builtin_fail "Uint.eq" ls

  let add ls = match ls with
    | [UintLit (wx, x); UintLit (wy, y)] -> 
      if (wx <> wy) then
        builtin_fail "Uint.add: type mismatch" ls
      else
        (match wx with
        | 32 -> pure (UintLit (wx, safe_add uint32gi Uint32.add x y))
        | 64 -> pure (UintLit (wx, safe_add uint64gi Uint64.add x y))
        | 128 -> pure (UintLit (wx, safe_add uint128gi Uint128.add x y))
        | _ -> builtin_fail "Uint.add: unsupported Uint type" ls
        )
    | _ -> builtin_fail "Uint.add" ls

  let sub ls = match ls with
    | [UintLit (wx, x); UintLit (wy, y)] -> 
      if (wx <> wy) then
        builtin_fail "Uint.sub: type mismatch" ls
      else
        (match wx with
        | 32 -> pure (UintLit (wx, safe_sub uint32gi Uint32.sub x y))
        | 64 -> pure (UintLit (wx, safe_sub uint64gi Uint64.sub x y))
        | 128 -> pure (UintLit (wx, safe_sub uint128gi Uint128.sub x y))
        | _ -> builtin_fail "Uint.sub: unsupported Uint type" ls
        )
    | _ -> builtin_fail "Uint.sub" ls

  let mul ls = match ls with
    | [UintLit (wx, x); UintLit (wy, y)] -> 
      if (wx <> wy) then
        builtin_fail "Uint.mul: type mistmatch" ls
      else 
        (match wx with
        | 32 -> pure (UintLit (wx, safe_mul uint32gi Uint32.mul x y))
        | 64 -> pure (UintLit (wx, safe_mul uint64gi Uint64.mul x y ))
        | 128 -> pure (UintLit (wx, safe_mul uint128gi Uint128.mul x y))
        | _ -> builtin_fail "Uint.mul: unsupported Uint type" ls
        )
    | _ -> builtin_fail "Uint.mul" ls  

  let lt ls = match ls with
    | [UintLit (wx, x); UintLit (wy, y)] -> 
      if (wx <> wy) then
        builtin_fail "Uint.lt: type mismatch" ls
      else 
        (match wx with
        | 32 -> pure (to_Bool (safe_lt uint32gi x y))
        | 64 -> pure (to_Bool (safe_lt uint64gi x y))
        | 128 -> pure (to_Bool (safe_lt uint128gi x y))
        | _ -> builtin_fail "Uint.lt: unsupported Uint type" ls
        )
    | _ -> builtin_fail "Uint.lt" ls  
end

(* Maps *)
module Maps = struct
  open UsefulLiterals
  
  let contains ls = match ls with
    | [Map (_, entries); key] ->
        let res = List.exists entries ~f:(fun (k, v) -> k = key) in
        pure @@ to_Bool res
    | _ -> builtin_fail "Map.contains" ls

  (* FIXME: Runtime type checking *)
  let put ls = match ls with
    | [Map (tm, entries); key; value] ->
        let filtered =
          List.filter entries ~f:(fun (k, v) -> k <> key) in
        pure @@ Map (tm, ((key, value) :: filtered)) 
    | _ -> builtin_fail "Map.put" ls

  let remove ls = match ls with
    | [Map (tm, entries); key] ->
        let res = List.filter entries ~f:(fun (k, v) -> k <> key) in
        pure @@ Map (tm, res)
    | _ -> builtin_fail "Map.remove" ls

  let get ls = match ls with
    | [Map (tm, entries); key] ->
        let res = List.find entries ~f:(fun (k, v) -> k = key) in
        pure (match res with
            | None -> none_lit
            | Some (_, v) -> some_lit v)
    | _ -> builtin_fail "Map.get" ls

end

(* Working with block numbers *)
module BNum = struct
  open UsefulLiterals
  
  let eq ls = match ls with
    | [BNum x; BNum y] ->
        pure @@ to_Bool (x = y)
    | _ -> builtin_fail "BNum.eq" ls

  let blt ls = match ls with
    | [BNum x; BNum y] -> pure (
        let i1 = big_int_of_string x in
        let i2 = big_int_of_string y in
        to_Bool (lt_big_int i1 i2))
    | _ -> builtin_fail "BNum.blt" ls

  let badd ls = match ls with
    | [BNum x; IntLit (wy, y)] ->
        let i1 = big_int_of_string x in
        let i2 = big_int_of_string y in
        if ge_big_int i2 (big_int_of_int 0)
        then pure @@ BNum (string_of_big_int (add_big_int i1 i2))
        else fail @@ sprintf
            "Cannot add a negative value (%s) to a block." y
    | _ -> builtin_fail "BNum.badd" ls

  
end

(* Working with addresses *)
module Address = struct
  open UsefulLiterals

  let eq ls = match ls with
    | [Address x; Address y] ->
        pure @@ to_Bool (x = y)
    | _ -> builtin_fail "Address.eq" ls
end

(* Hashing *)
module Hashing = struct
  open UsefulLiterals
  open Cryptokit

  let hex s = transform_string (Hexa.decode()) s
  let tohex s = transform_string (Hexa.encode()) s
  let hash s = hash_string (Hash.sha2 256) s

  let eq ls = match ls with
    | [Sha256 x; Sha256 y] ->
        pure @@ to_Bool (x = y)
    | _ -> builtin_fail "Hashing.eq" ls

  let sha256hash ls = match ls with
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
  
  let dist ls = match ls with
    | [Sha256 x; Sha256 y] ->
        let i1 = big_int_of_hash x in
        let i2 = big_int_of_hash y in
        let dist = abs_big_int (sub_big_int i1 i2) in
        (try
           let i = IntLit (128,(string_of_big_int dist)) in
           pure i
         with
         | Failure _ -> fail @@
             sprintf "Could not convert big int %s to int."
               (string_of_big_int dist))
    | _ -> builtin_fail "Hashing.dist" ls
  
end

(***************************************)
(*        Built-in  Dictionariy        *)
(***************************************)

module BuiltInDictionary = struct 
  type built_in_op_type = literal list -> (literal, string) result

  (* All built-in functions *)
      
  let built_in_dict = [
    (* Strings *)
    ("eq", ["String"; "String"], String.eq);
    ("concat", ["String"; "String"], String.concat);
    ("substr", ["String"; "Int"; "Int"], String.substr);

    (* Integers *)
    ("eq",  ["Int"; "Int"], Int.eq);
    ("add", ["Int"; "Int"], Int.add);
    ("sub", ["Int"; "Int"], Int.sub);
    ("mul", ["Int"; "Int"], Int.mul);
    ("lt",  ["Int"; "Int"], Int.lt);

    (* Unsigned integers *)
    ("eq",  ["Uint"; "Uint"], Uint.eq);
    ("add", ["Uint"; "Uint"], Uint.add);
    ("sub", ["Uint"; "Uint"], Uint.sub);
    ("mul", ["Uint"; "Uint"], Uint.mul);
    ("lt",  ["Uint"; "Uint"], Uint.lt);

    (* Block numbers *)
    ("eq",  ["BNum"; "BNum"], BNum.eq);
    ("blt", ["BNum"; "BNum"], BNum.blt);
    ("badd", ["BNum"; "Int"],  BNum.badd);
    
    (* Addresses *)
    ("eq",  ["Address"; "Address"], Address.eq);

    (* Hashes *)
    ("eq", ["Hash"; "Hash"], Hashing.eq);
    ("dist", ["Hash"; "Hash"], Hashing.dist);
    ("sha256hash", ["Any"], Hashing.sha256hash);

    (* Maps *)
    ("contains", ["Map"; "Any"], Maps.contains);
    ("put", ["Map"; "Any"; "Any"], Maps.put);
    ("get", ["Map"; "Any"], Maps.get);
    ("remove", ["Map"; "Any"], Maps.remove);
  ]

  let rec tags_match expected argtypes =
    match expected, argtypes with
    | e::es, a::args ->
      if e = a || e = "Any" || 
         is_int_type a && e = "Int" || 
         is_uint_type a && e = "Uint"
      then
        tags_match es args
      else 
        false
    | [], [] -> true
    | _ -> false
  
  (* Dictionary lookup *)
  let find_builtin_op opname argtypes =
    match List.find built_in_dict
            ~f:(fun (n, expected, _) ->
                n = opname &&
                tags_match expected argtypes) with
    | None ->
        fail @@
        sprintf "Cannot find built-in with name \"%s\" and arguments %s."
          opname ("(" ^ (Core.String.concat ~sep:", " argtypes) ^ ")")
    | Some (_, _, op) -> pure op
end
