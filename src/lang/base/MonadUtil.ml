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
open Stdint

(* [Evaluation in CPS]

   The following module implements the standard CPS, together with the
   posible failure result. The `CPSMonad` is later specialised to
   implement state-passing (for gas accounting) by means of
   insantiating it suitably in `EvalMonad` to take an additional
   parameter of type `int` for gas accounting.

*)

module CPSMonad = struct
  
  type nonrec ('a, 'b, 'c) t = (('a, 'b) result -> 'c) -> 'c
    
  let return x = (fun k -> k @@ Ok x)
                 
  let bind x ~f =
    fun k ->
      let k' r = (match r with
          | Ok z -> (f z) k
          | Error _ as x'  -> k x'
            ) in
      x k'
          
  let map x ~f = 
    fun k->
      let k' r = (match r with
          | Error _ as x' -> k x'
          | Ok z -> k @@ Ok (f z)
        ) in
      x  k'
        
  let map = `Custom map
      
end             

(****************************************************************)
(*               Result monad and its utilitis                  *)
(****************************************************************)

(* Monadic evaluation results *)
let fail (s : scilla_error list) = Error s
let pure e = return e

(* fail with just a message, no location info. *)
let fail0 (msg : string) = fail @@ mk_error0 msg
(* fail with a message and start location. *)
let fail1 msg sloc = fail @@ mk_error1 msg sloc
(* fail with a message and both start and end locations. *)
let fail2 msg sloc eloc = fail @@ mk_error2 msg sloc eloc

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
(*           A gas-aware monad for `Eval` and related utilites  *)
(****************************************************************)

module EvalMonad = struct

  include Monad.Make3 (CPSMonad)

  (* Monadic evaluation results *)
  let fail (s : scilla_error list) = 
    (fun k remaining_gas -> k (Error s) remaining_gas)
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

  (* [Wrappers for Gas Accounting]  *)
  let checkwrap_opR op_thunk cost =
    (fun k remaining_gas ->
       if (Uint64.compare remaining_gas cost) >= 0
       then 
         let res = op_thunk () in
         k res (Uint64.sub remaining_gas cost)
       else 
         k (Error (mk_error0 "Ran out of gas")) remaining_gas)

  let checkwrap_op op_thunk cost emsg =
    (fun k remaining_gas ->
       if (Uint64.compare remaining_gas cost) >= 0 then
          op_thunk () k (Uint64.sub remaining_gas cost)
       else
         k (Error emsg) remaining_gas)

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
          let k' r remaining_cost' = (
            match r with
            | Ok z -> k (Ok (x, z)) remaining_cost'
            | Error _ -> doTry ls' k remaining_cost') in
          (f x) k' remaining_cost
      | _ -> k (Error (msg ())) remaining_cost
    in
    (fun k remaining_cost -> doTry ls k remaining_cost)

end (* module EvalMonad *)
