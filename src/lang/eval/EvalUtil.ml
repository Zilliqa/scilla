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

(* Environments *)
module Env = struct
  type ident = string

  (* Environment *)
  type 'rep t =
    (string * 'rep value) list
  and
  (* Fully reduced value *)
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
    let i = get_id k in
    match List.find ~f:(fun z -> fst z = i) e with 
    | Some x -> pure @@ snd x
    | None -> fail @@ sprintf
        "Indentifier \"%s\" at %s is not bound in environment:\n%s"
        i (get_loc_str (get_loc k)) (pp e)
end

module State = struct

    (* Mutable state and operations with it *)
  type 'rep t = {
    (* Immutable variables *)
    env : 'rep Env.t;
    (* Contract fields *)
    fields : (string * literal) list;
    (* Contract balance *)
    balance : Big_int.big_int;
    (* Blockchain state *)
    blockchain_state : (string * literal) list;
    (* Emitted messages *)
    msgs : literal list;
    (* Emitted events *)
    events : (string * string) list
  }

  let pp_fields s =
    let ps = List.map s
        ~f:(fun (k, v) -> sprintf " [%s -> %s]" k
               (sexp_of_literal v |> Sexplib.Sexp.to_string)) in
    let cs = String.concat ~sep:",\n " ps in
    sprintf "{%s }" cs
  
  (* TODO: Implement state pretty-printer *)
  
  let store st k l =
    let s = st.fields in
    match List.find s ~f:(fun (z, _) -> z = k) with
    | Some (_, _) -> pure @@
        {st with
         fields = (k, l) :: List.filter ~f:(fun z -> fst z <> k) s}
    | None -> fail @@ sprintf
          "No field \"%s\" in fields:\n%s" k (pp_fields s)

  let load st k =
    let s = st.fields in
    let i = get_id k in
    match List.find ~f:(fun z -> fst z = i) s with 
    | Some x -> pure @@ snd x
    | None -> fail @@ sprintf
        "No field \"%s\" in state:\n%s" i (pp_fields s)

  let bind st k v =
    let e = st.env in
    {st with env = (k, v) :: List.filter ~f:(fun z -> fst z <> k) e}

  

end

(* Printing result *)
let pp_result r = match r with
  | Error s -> s
  | Ok (e, env) ->
      (Env.sexp_of_value sexp_of_loc e |> Sexplib.Sexp.to_string)
      ^ ",\n" ^
      (Env.pp env)
