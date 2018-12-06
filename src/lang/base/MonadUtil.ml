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
open ErrorUtils

(* Monadic evaluation results *)
let fail (s : scilla_error list) = Error s
let pure e = return e

(* fail with just a message, no location info. *)
let fail0 (msg : string) = fail @@ mk_error0 msg
(* fail with a message and start location. *)
let fail1 msg sloc = fail @@ mk_error1 msg sloc
(* fail with a message and both start and end locations. *)
let fail2 msg sloc eloc = fail @@ mk_error2 msg sloc eloc

(****************************************************************)
(*               Utilities for the result monad                 *)
(****************************************************************)

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
      let%bind z = f x in
      let%bind zs = mapM ~f:f ls' in
      pure (z :: zs)
  | [] -> pure []

let liftPair1 m x = 
  let%bind z = m in
  pure (z, x)

let liftPair2 x m = 
  let%bind z = m in
  pure (x, z)

(* Return the first error applying f to elements of ls.
 * Returns true if all elements satisfy f. *)
let rec forallM ~f ls = match ls with
  | x :: ls' ->
      let%bind _ = f x in 
      forallM ~f:f ls'
  | [] -> pure true

(* Try all variants in the list, pick the first successful one *)
let rec tryM ~f ls ~msg = match ls with
  | x :: ls' ->
      (match f x  with
       | Ok z -> Ok (x, z)
       | Error _ -> tryM ~f:f ls' ~msg)
  | [] -> Error (msg ())

(****************************************************************)
(*           A monad for `Eval` and related utilites            *)
(****************************************************************)

module EvalMonad = struct

  (* Extended result type, to track remaining gas. *)
  type ('a, 'b) eresult =
    | Ok of 'a * int
    | Error of 'b * int

  (* Each eval operation that passes through this monad 
   * returns a function "(int -> ('a, 'b) eresult", which
   * when executed with "remaining_gas" as the argument
   * produces eresult (with a new value for remaining_gas). *)
  module CPSMonad = struct

      type nonrec ('a, 'b, 'c) t = (('a, 'b) eresult -> 'c) -> int -> 'c

      (* This does charge any gas. *)
      let return x = (fun k remaining_gas-> k @@ Ok (x, remaining_gas))

      let bind x ~f =
        (fun k remaining_gas ->
           let k' r = (match r with
             | Error _ as x'  -> k x'
             | Ok (z, remaining_gas') -> (f z) k remaining_gas') in
           x k' remaining_gas
        )

      let map x ~f = 
        (fun k remaining_gas->
           let k' r = (match r with
           | Error _ as x' -> k x'
           | Ok (z, remaining_gas') -> k @@ Ok ((f z), remaining_gas')) in
           x  k' remaining_gas
        )

      let map = `Custom map

    end             

  include Monad.Make3 (CPSMonad)

  (* Monadic evaluation results *)
  let fail (s : scilla_error list) = 
    (fun k remaining_gas -> k @@ Error (s, remaining_gas))
  let pure e = return e

  (* fail with just a message, no location info. *)
  let fail0 (msg : string) = fail @@ mk_error0 msg
  (* fail with a message and start location. *)
  let fail1 msg sloc = fail @@ mk_error1 msg sloc
  (* fail with a message and both start and end locations. *)
  let fail2 msg sloc eloc = fail @@ mk_error2 msg sloc eloc

  let fromR r =
    match r with
    | Core.Error s -> fail s
    | Core.Ok a -> pure a

  let mapR r remaining_gas =
    match r with
    | Core.Error s -> Error (s, remaining_gas)
    | Core.Ok a -> Ok (a, remaining_gas)

  (* Wrap an op with cost check when op returns "result". *)
  let checkwrap_opR op_thunk cost =
    (fun k remaining_gas ->
       if (remaining_gas >= cost)
       then 
         let res = op_thunk () in
         k @@ mapR res (remaining_gas - cost)
       else 
         k @@ Error (mk_error0 "Ran out of gas", remaining_gas))

  (* Wrap an op with cost check when op returns "eresult". *)
  let checkwrap_op op_thunk cost emsg =
    (fun k remaining_gas ->
       if remaining_gas >= cost then
          op_thunk () k (remaining_gas - cost)
       else
         k @@ Error (emsg, remaining_gas))

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
        let%bind z = f x in
        let%bind zs = mapM ~f:f ls' in
        pure (z :: zs)
    | [] -> pure []

  let liftPair1 m x = 
    let%bind z = m in
    pure (z, x)
      
  let liftPair2 x m = 
    let%bind z = m in
    pure (x, z)

(* Return the first error applying f to elements of ls.
 * Returns true if all elements satisfy f. *)
  let rec forallM ~f ls = match ls with
    | x :: ls' ->
        let%bind _ = f x in 
        forallM ~f:f ls'
    | [] -> pure true

  (* Try all variants in the list, pick the first successful one *)
  let tryM ~f ls ~msg =
    let rec doTry ls k remaining_cost =
      match ls with
      | x :: ls' ->
          let k' r = (
            match r with
            | Ok (z, remaining_cost') -> k @@ Ok ((x, z), remaining_cost')
            | Error (_, remaining_cost') -> doTry ls' k remaining_cost') in
          (f x) k' remaining_cost
      | _ -> k @@ Error (msg (), remaining_cost)
    in
    (fun k remaining_cost -> doTry ls k remaining_cost)

end (* module EvalMonad *)
