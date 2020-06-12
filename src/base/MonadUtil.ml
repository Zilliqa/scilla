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

open Core_kernel
open! Int.Replace_polymorphic_compare
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

  let return x k = k @@ Ok x

  let bind x ~f k =
    let k' r = match r with Ok z -> (f z) k | Error _ as x' -> k x' in
    x k'

  let map x ~f k =
    let k' r = match r with Error _ as x' -> k x' | Ok z -> k @@ Ok (f z) in
    x k'

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
let rec foldM ~f ~init ls =
  match ls with
  | x :: ls' ->
      let%bind res = f init x in
      foldM ~f ~init:res ls'
  | [] -> pure init

(* Monad version of fold2 *)
let rec fold2M ~f ~init ls ms ~msg =
  match (ls, ms) with
  | x :: ls', y :: ms' ->
      let%bind res = f init x y in
      fold2M ~f ~init:res ls' ms' ~msg
  | [], [] -> pure init
  | _ -> fail @@ msg ()

(* Monadic fold-right for error *)
let rec foldrM ~f ~init ls =
  match ls with
  | x :: ls' ->
      let%bind rest = foldrM ~f ~init ls' in
      f rest x
  | [] -> pure init

(* Monadic map for error *)
let rec mapM ~f ls =
  match ls with
  | x :: ls' ->
      let%bind z = f x in
      let%bind zs = mapM ~f ls' in
      pure (z :: zs)
  | [] -> pure []

(* Monadic map2 *)
let rec map2M ~f ls ms ~msg =
  match (ls, ms) with
  | x :: ls', y :: ms' ->
      let%bind z = f x y in
      let%bind zs = map2M ~f ls' ms' ~msg in
      pure (z :: zs)
  | [], [] -> pure []
  | _ -> fail @@ msg ()

let liftPair1 m x =
  let%bind z = m in
  pure (z, x)

let liftPair2 x m =
  let%bind z = m in
  pure (x, z)

(* Return the first error applying f to elements of ls.
 * Returns () if all elements satisfy f. *)
let rec forallM ~f ls =
  match ls with
  | x :: ls' ->
      let%bind () = f x in
      forallM ~f ls'
  | [] -> pure ()

(* Try all variants in the list, pick the first successful one *)
let rec tryM ~f ls ~msg =
  match ls with
  | x :: ls' -> (
      match f x with Ok z -> Ok (x, z) | Error _ -> tryM ~f ls' ~msg )
  | [] -> Error (msg ())

(* Monadic Option.map for error *)
let option_mapM ~f opt_val =
  match opt_val with
  | None -> pure None
  | Some v ->
      let%bind z = f v in
      pure @@ Some z

(* Monadic version of List.fold_map *)
let fold_mapM ~f ~init l =
  let%bind acc, l'_rev =
    foldM ~init:(init, [])
      ~f:(fun (accacc, lrevacc) lel ->
        let%bind accacc', lel' = f accacc lel in
        pure (accacc', lel' :: lrevacc))
      l
  in
  pure (acc, List.rev l'_rev)

(* Monadic wrapper around any container's fold (Set, Map etc). *)
(* folder : 'a t -> init:'accum -> f:('accum -> 'a -> 'accum) -> 'accum *)
let wrapM_folder ~folder ~f ~init l =
  let f' acc e = match acc with Error _ -> acc | Ok acc' -> f acc' e in
  folder l ~init:(Ok init) ~f:f'

(****************************************************************)
(*           A gas-aware monad for `Eval` and related utilites  *)
(****************************************************************)

module EvalMonad = struct
  include Monad.Make3 (CPSMonad)

  (* Monadic evaluation results *)
  let fail s k remaining_gas = k (Error s) remaining_gas

  let pure e = return e

  (* fail with just a message, no location info. *)
  let fail0 (msg : string) = fail @@ mk_error0 msg

  (* fail with a message and start location. *)
  let fail1 msg sloc = fail @@ mk_error1 msg sloc

  (* fail with a message and both start and end locations. *)
  let fail2 msg sloc eloc = fail @@ mk_error2 msg sloc eloc

  let fromR r =
    match r with Core_kernel.Error s -> fail s | Core_kernel.Ok a -> pure a

  let out_of_gas_err = mk_error0 "Ran out of gas"

  (* [Wrappers for Gas Accounting]  *)
  let checkwrap_opR op_thunk cost k remaining_gas =
    if Uint64.compare remaining_gas cost >= 0 then
      let res = op_thunk () in
      k res (Uint64.sub remaining_gas cost)
    else k (Error out_of_gas_err) remaining_gas

  let checkwrap_op op_thunk cost emsg k remaining_gas =
    if Uint64.compare remaining_gas cost >= 0 then
      op_thunk () k (Uint64.sub remaining_gas cost)
    else k (Error emsg) remaining_gas

  open Let_syntax

  (* Monadic fold-left for error *)
  let rec foldM ~f ~init ls =
    match ls with
    | x :: ls' ->
        let%bind res = f init x in
        foldM ~f ~init:res ls'
    | [] -> pure init

  (* Monadic fold-right for error *)
  let rec foldrM ~f ~init ls =
    match ls with
    | x :: ls' ->
        let%bind rest = foldrM ~f ~init ls' in
        f rest x
    | [] -> pure init

  (* Monadic map for error *)
  let rec mapM ~f ls =
    match ls with
    | x :: ls' ->
        let%bind z = f x in
        let%bind zs = mapM ~f ls' in
        pure (z :: zs)
    | [] -> pure []

  (* Monadic map2 *)
  let rec map2M ~f ls ms ~msg =
    match (ls, ms) with
    | x :: ls', y :: ms' ->
        let%bind z = f x y in
        let%bind zs = map2M ~f ls' ms' ~msg in
        pure (z :: zs)
    | [], [] -> pure []
    | _ -> fail @@ msg ()

  let liftPair1 m x =
    let%bind z = m in
    pure (z, x)

  let liftPair2 x m =
    let%bind z = m in
    pure (x, z)

  (* Return the first error applying f to elements of ls.
   * Returns () if all elements satisfy f. *)
  let rec forallM ~f ls =
    match ls with
    | x :: ls' ->
        let%bind () = f x in
        forallM ~f ls'
    | [] -> pure ()

  (* Try all variants in the list, pick the first successful one *)
  let tryM ~f ls ~msg =
    let rec doTry ls k remaining_cost =
      match ls with
      | x :: ls' ->
          let k' r remaining_cost' =
            match r with
            | Ok z -> k (Ok (x, z)) remaining_cost'
            | Error _ -> doTry ls' k remaining_cost'
          in
          (f x) k' remaining_cost
      | _ -> k (Error (msg ())) remaining_cost
    in
    fun k remaining_cost -> doTry ls k remaining_cost
end

(* module EvalMonad *)
