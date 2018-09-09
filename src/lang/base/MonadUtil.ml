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

  (* Extended result type, to track remaining gas. *)
  type ('a, 'b) eresult =
    | Ok of 'a * int
    | Error of 'b * int

  (* Each eval operation that passes through this monad 
   * returns a function "(int -> ('a, 'b) eresult", which
   * when executed with "remaining_gas" as the argument
   * produces eresult (with a new value for remaining_gas). *)
  include Monad.Make2 (struct

      type nonrec ('a, 'b) t = int -> ('a, 'b) eresult

      let bind x ~f =
        (fun remaining_gas ->
           let r = x remaining_gas in
           match r with
           | Error _ as x'  -> x'
           | Ok (z, remaining_gas') -> (f z) remaining_gas'
        )

      let map x ~f =
        (fun remaining_gas ->
           let r = x remaining_gas in
           match r with
           | Error _ as x' -> x'
           | Ok (z, remaining_gas') -> Ok ((f z), remaining_gas'))

      let map = `Custom map

      (* This does charge any gas. *)
      let return x = (fun remaining_gas -> Ok (x, remaining_gas))

    end)

  (* Monadic evaluation results *)
  let fail s = (fun remaining_gas -> Error (s, remaining_gas))
  let pure e = return e

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
    (fun remaining_gas ->
       if (remaining_gas >= cost)
       then 
         let res = op_thunk() in
         mapR res (remaining_gas - cost)
       else 
         Error ("Ran out of gas", remaining_gas))

  (* Wrap an op with cost check when op returns "eresult". *)
  let checkwrap_op op_thunk cost emsg =
    (fun remaining_gas ->
       if remaining_gas >= cost then
         op_thunk() (remaining_gas - cost)
       else
         Error (emsg, remaining_gas))

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
  let mapM ~f ls = 
    let rec doMap ls remaining_cost =
      match ls with
      | x :: ls' ->
          let r1 = f x in
          (match r1 remaining_cost with
           | Ok(z, remaining_cost') ->
               let z' = (doMap ls' remaining_cost') in
               (match z' with
                | Ok (z'', remaining_cost'') -> Ok (z :: z'', remaining_cost'')
                | Error _ as x' -> x')
           | Error _ as x' -> x')

      | [] -> Ok ([], remaining_cost)
    in
    (fun remaining_cost -> doMap ls remaining_cost)

  (* Try all variants in the list, pick the first successful one *)
  let tryM ~f ls ~msg =
    let rec doTry ls remaining_cost =
      match ls with
      | x :: ls' ->
          (match (f x) remaining_cost  with
           | Ok (z, remaining_cost') -> Ok ((x, z), remaining_cost')
           | Error (_, remaining_cost') -> doTry ls' remaining_cost')
      | [] -> Error (msg, remaining_cost)
    in
    (fun remaining_cost -> doTry ls remaining_cost)

  let liftPair2 x m = 
    (fun remaining_gas ->
       match m remaining_gas with
       | Ok (z, es) -> Ok ((x, z), es)
       | Error _ as err -> err)

  let liftPair1 m x =
    (fun remaining_gas ->
       match m remaining_gas with
       | Ok (z, es) -> Ok ((z, x), es)
       | Error _ as err -> err)

end (* module EvalMonad *)
