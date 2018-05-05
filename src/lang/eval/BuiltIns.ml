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
  let ttrue = ADTValue ("True", [], [])
  let ffalse = ADTValue ("False", [], [])

  let to_Bool b = if b then ttrue else ffalse
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
end

(* Maps *)
module Maps = struct
  open UsefulLiterals
  
  let contains ls = match ls with
    | [Map entries; key] ->
        let res = List.exists entries ~f:(fun (k, v) -> k = key) in
        pure @@ to_Bool res
    | _ -> builtin_fail "Map.contains" ls

  (* TODO: implement put *)

  (* TODO: implement get *)

  
end


(* Working with addresses *)
module Address = struct
    (* TODO *)
end

(* Hashing *)
module Hashing = struct
  (* TODO *)
end

(* Dictionaries *)
module BuiltInDictionary = struct 
  type built_in_op_type = literal list -> (literal, string) result

  

  let built_in_dict = [
    ("eq", ["Int"; "Int"], Int.eq);
    ("add", ["Int"; "Int"], Int.add);
    ("sub", ["Int"; "Int"], Int.sub);
    ("mul", ["Int"; "Int"], Int.mul);

    (* TODO: generalize for any type! *)
    ("contains", ["Map"; "Int"], Maps.contains);
    (* TODO: add other built-ins *)
  ]

  let tags_match expected argtypes =
    (* TODO: Generalise me! *)
    expected = argtypes
  
  (* Dictionary lookup *)
  let find_builtin_op opname argtypes =
    match List.find built_in_dict
            ~f:(fun (n, args, _) ->
                n = opname &&
                tags_match args argtypes) with
    | None ->
        fail @@ sprintf "Cannot find built-in with name \"%s\" and arguments %s."
          opname ("(" ^ (String.concat ~sep:", " argtypes) ^ ")")
    | Some (_, _, op) -> pure op
end
