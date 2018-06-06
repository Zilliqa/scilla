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
open Result.Let_syntax
open MonadUtil
open Big_int

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
end

(* Integer operation *)
module Int = struct
  open UsefulLiterals
  
  let eq ls = match ls with
    | [IntLit x; IntLit y] ->
        pure @@ to_Bool (x = y)
    | _ -> builtin_fail "Int.eq" ls

  (* Check for overflows! *)
  let add ls = match ls with
    | [IntLit x; IntLit y] -> pure (
        let i1 = big_int_of_string x in
        let i2 = big_int_of_string y in
        IntLit (string_of_big_int (add_big_int i1 i2)))
    | _ -> builtin_fail "Int.add" ls

  let sub ls = match ls with
    | [IntLit x; IntLit y] -> pure (
        let i1 = big_int_of_string x in
        let i2 = big_int_of_string y in
        IntLit (string_of_big_int (sub_big_int i1 i2)))
    | _ -> builtin_fail "Int.sub" ls

  let mul ls = match ls with
    | [IntLit x; IntLit y] -> pure (
        let i1 = big_int_of_string x in
        let i2 = big_int_of_string y in
        IntLit (string_of_big_int (mult_big_int i1 i2)))
    | _ -> builtin_fail "Int.mul" ls  

  let lt ls = match ls with
    | [IntLit x; IntLit y] -> pure (
        let i1 = big_int_of_string x in
        let i2 = big_int_of_string y in
        to_Bool (lt_big_int i1 i2))
    | _ -> builtin_fail "Int.lt" ls  

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
    | [BNum x; IntLit y] ->
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
           let i = IntLit (string_of_big_int dist) in
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

    (* Integers *)
    ("eq",  ["Int"; "Int"], Int.eq);
    ("add", ["Int"; "Int"], Int.add);
    ("sub", ["Int"; "Int"], Int.sub);
    ("mul", ["Int"; "Int"], Int.mul);
    ("lt",  ["Int"; "Int"], Int.lt);

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
    | e::es, a::args
      when e = a || e = "Any" -> tags_match es args
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
