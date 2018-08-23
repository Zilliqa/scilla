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

open Core
open Result.Let_syntax

(****************************************************************)
(*               Utilities for the result monad                 *)
(****************************************************************)

(* Monadic evaluation results *)
let fail s = Error s
let pure e = return e

(* Monadic fold-left for error *)
let rec foldM ~f ~init ls = match ls with
  | x :: ls' ->
      let%bind res = f init x in
      foldM ~f:f ~init:res ls'
  | [] -> pure init

(* Monadic fold-right for error *)
let rec foldrM ~f ~init ls = match ls with
  | x :: ls' ->
      let%bind rest = foldrM ~f:f ~init:init ls' in
      f rest x
  | [] -> pure init

(* Monadic map for error *)
let rec mapM ~f ls = match ls with
  | x :: ls' ->
      (match f x, mapM ~f:f ls' with
       | Ok z, Ok zs -> Ok (z :: zs)
       | Error _ as err, _ -> err
       | _, (Error _ as err) -> err)
  | [] -> Ok []

(* Try all variants in the list, pick the first successful one *)
let rec tryM ~f ls ~msg = match ls with
  | x :: ls' ->
      (match f x  with
       | Ok z -> Ok (x, z)
       | Error _ -> tryM ~f:f ls' ~msg)
  | [] -> Error msg

let liftPair2 x m = match m with
  | Ok z -> Ok (x, z)
  | Error _ as err -> err

let liftPair1 m x = match m with
  | Ok z -> Ok (z, x)
  | Error _ as err -> err

(****************************************************************)
(*           A monad for `Eval` and related utilites            *)
(****************************************************************)

module EvalMonad = struct

type eval_state = 
  { 
    gas_used : int;
    unused : int;
  }

let init_eval_state =
  { gas_used = 0; unused = 0 }

(* Extended result type, to track eval_state. *)
type ('a, 'b) eresult =
  | Ok of 'a * eval_state
  | Error of 'b * eval_state

let add_gas x g = match x with
  | Error (e, es) -> 
    let es' = { es with gas_used = es.gas_used + g } in
      Error (e, es')
  | Ok (x, es) ->
    let es' = { es with gas_used = es.gas_used + g } in
    Ok (x, es')

include Monad.Make2 (struct

  type nonrec ('a, 'b) t = ('a,'b) eresult

  let bind x ~f = match x with
    | Error _ as x  -> x
    | Ok (x, es) ->
      let res = f x in
        add_gas res es.gas_used

    let map x ~f = match x with
    | Error _ as x -> x
    | Ok (x, g) -> Ok ((f x), g)

  let map = `Custom map

  let return x = Ok (x, init_eval_state)

end)

(* Monadic evaluation results *)
let fail s = Error (s, init_eval_state)
let pure e = Ok (e, init_eval_state)
let pure_gas e g = Ok (e, { init_eval_state with gas_used = g } )
let from_result r =
  match r with
  | Core.Error s -> fail s
  | Core.Ok a -> pure a

open Let_syntax

(* Monadic fold-left for error *)
let rec foldM ~f ~init ls = match ls with
  | x :: ls' ->
      let%bind res = f init x in
      foldM ~f:f ~init:res ls'
  | [] -> pure init

(* Monadic fold-right for error *)
let rec foldrM ~f ~init ls = match ls with
  | x :: ls' ->
      let%bind rest = foldrM ~f:f ~init:init ls' in
      f rest x
  | [] -> pure init

(* Monadic map for error *)
let rec mapM ~f ls = match ls with
  | x :: ls' ->
      (match f x, mapM ~f:f ls' with
       | Ok (z, es1), Ok (zs, es2) -> 
          pure_gas (z :: zs) (es1.gas_used + es2.gas_used)
       | Error _ as err, _ -> err
       | _, (Error _ as err) -> err)
  | [] -> pure []

(* Try all variants in the list, pick the first successful one *)
let rec tryM ~f ls ~msg = match ls with
  | x :: ls' ->
      (match f x  with
       | Ok (z, es) -> Ok ((x, z), es)
       | Error _ -> tryM ~f:f ls' ~msg)
  | [] -> fail msg

let liftPair2 x m = match m with
  | Ok (z, es) -> Ok ((x, z), es)
  | Error _ as err -> err

let liftPair1 m x = match m with
  | Ok (z, es) -> Ok ((z, x), es)
  | Error _ as err -> err

end (* module EvalMonad *)
