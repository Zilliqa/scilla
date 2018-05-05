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


(* Integer operation *)
module Int = struct
  open UsefulLiterals
  
  let eq ls = match ls with
    | [IntLit x; IntLit y] ->
        pure @@ to_Bool (x = y)
    | _ -> builtin_fail "Int.eq" ls

  let add ls = match ls with
    | [IntLit x; IntLit y] -> pure @@ IntLit (x + y)
    | _ -> builtin_fail "Int.add" ls

  let sub ls = match ls with
    | [IntLit x; IntLit y] -> pure @@ IntLit (x - y)
    | _ -> builtin_fail "Int.sub" ls

  let mul ls = match ls with
    | [IntLit x; IntLit y] -> pure @@ IntLit (x * y)
    | _ -> builtin_fail "Int.mul" ls  

  let lt ls = match ls with
    | [IntLit x; IntLit y] -> pure @@ to_Bool (x < y)
    | _ -> builtin_fail "Int.lt" ls  

end

(* Maps *)
module Maps = struct
  open UsefulLiterals
  
  let contains ls = match ls with
    | [Map entries; key] ->
        let res = List.exists entries ~f:(fun (k, v) -> k = key) in
        pure @@ to_Bool res
    | _ -> builtin_fail "Map.contains" ls

  let put ls = match ls with
    | [Map entries; key; value] ->
        let filtered =
          List.filter entries ~f:(fun (k, v) -> k <> key) in
        pure @@ Map ((key, value) :: filtered) 
    | _ -> builtin_fail "Map.put" ls

  let remove ls = match ls with
    | [Map entries; key] ->
        let res = List.filter entries ~f:(fun (k, v) -> k <> key) in
        pure @@ Map res
    | _ -> builtin_fail "Map.remove" ls

  let get ls = match ls with
    | [Map entries; key] ->
        let res = List.find entries ~f:(fun (k, v) -> k = key) in
        pure (match res with
            | None -> none_lit
            | Some (_, v) -> some_lit v)
    | _ -> builtin_fail "Map.contains" ls

end

(* Working with block numbers *)
module BNum = struct
  open UsefulLiterals
  
  let eq ls = match ls with
    | [BNum x; BNum y] ->
        pure @@ to_Bool (x = y)
    | _ -> builtin_fail "BNum.eq" ls

  let blt ls = match ls with
    | [BNum x; BNum y] ->
        pure @@ to_Bool (x < y)
    | _ -> builtin_fail "BNum.blt" ls

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
  (* TODO *)
end

(* Dictionaries *)
module BuiltInDictionary = struct 
  type built_in_op_type = literal list -> (literal, string) result

  (* All built-in functions *)
      
  let built_in_dict = [
    (* Integers *)
    ("eq",  ["Int"; "Int"], Int.eq);
    ("add", ["Int"; "Int"], Int.add);
    ("sub", ["Int"; "Int"], Int.sub);
    ("mul", ["Int"; "Int"], Int.mul);
    ("lt",  ["Int"; "Int"], Int.lt);

    (* Block numbers *)
    ("eq",  ["BNum"; "BNum"], BNum.eq);
    ("blt", ["BNum"; "BNum"], BNum.blt);

    (* Addresses *)
    ("eq",  ["Address"; "Address"], BNum.eq);

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
          opname ("(" ^ (String.concat ~sep:", " argtypes) ^ ")")
    | Some (_, _, op) -> pure op
end
