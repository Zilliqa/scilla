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

(* Monadic evaluation results *)
let fail s = Error s
let pure e = return e


(* Environments *)
module Env = struct
  type ident = string

  type 'rep t =
    (string * 'rep value) list
  and
  'rep value =
    | ValLit of literal
    | ValClosure of 'rep Syntax.ident * typ * 'rep expr * 'rep t
  [@@deriving sexp]

  (* Pretty-printing *)
  let rec pp e =
    let ps = List.map e
        ~f:(fun (k, v) -> " [" ^ k ^ " -> " ^ (pp_value v) ^ "]") in
    let cs = String.concat ~sep:",\n " ps in
    "{" ^ cs ^ " }"
  and
    pp_value v = match v with
    | ValLit l -> sexp_of_literal l |> Sexplib.Sexp.to_string
    | ValClosure (f, t, e, env) ->
        (sexp_of_expr sexp_of_loc (Fun (f, t, e)) |> Sexplib.Sexp.to_string)
        ^ ", " ^ (pp env)  

  let empty = []
              
  let bind e k v =
    (k, v) :: List.filter ~f:(fun z -> fst z <> k) e
                                                                
  let lookup e k =
    match List.find ~f:(fun z -> fst z = k) e with 
    | Some x -> pure @@ snd x
    | None -> fail @@ "Indentifier \"" ^ k ^
                      "\" is unbound in environment:\n" ^ (pp e)
                                                        
end

(* Printing result *)
let pp_result r = match r with
  | Error s -> s
  | Ok (e, env) ->
      (Env.sexp_of_value sexp_of_loc e |> Sexplib.Sexp.to_string)
      ^ ",\n" ^
      (Env.pp env)

