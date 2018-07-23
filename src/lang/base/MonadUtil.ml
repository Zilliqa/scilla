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

(* Monadic map for error *)
let rec mapM ~f ls = match ls with
  | x :: ls' ->
      (match f x, mapM ~f:f ls' with
       | Ok z, Ok zs -> Ok (z :: zs)
       | Error z as err, _ -> err
       | _, (Error _ as err) -> err)
  | [] -> Ok []

let rec map2M ~f ls1 ls2 = match (ls1, ls2) with
  | (x :: ls1', y :: ls2') ->
      (match f x y, map2M ~f:f ls1' ls2' with
       | Ok z, Ok zs -> Ok (z :: zs)
       | Error z as err, _ -> err
       | _, (Error _ as err) -> err)
  | ([], []) -> Ok []
  | _ -> Error "Internal error: map2M given lists of different lengths"

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

(* Map [[x1; x2; ...; xn]; [y1; y2; ...; yn]; ...]
   to [[...; y1; x1]; [...; y2; x2]; ...; [...; yn; xn]] *)
let rec unzipN_rev n ls ~msg =
  let rec build_empties n =
    match n with
    | 0 -> []
    | m -> [] :: (build_empties (m-1)) in
  let rec unzip_level n l acc =
    match (n, l, acc) with
    | (0, [], []) -> pure @@ []
    | (m, v :: vs, r :: rs) ->
        let%bind unzip_res = unzip_level (m-1) vs rs in
        pure @@ (v :: r) :: unzip_res
    | _ -> fail @@ msg in
  let rec unzip_rev n ls acc =
    match ls with
    | [] -> pure @@ acc
    | l :: ls' ->
        let%bind new_acc = (unzip_level n l acc) in
        unzip_rev n ls' new_acc in
  unzip_rev n ls (build_empties n)
