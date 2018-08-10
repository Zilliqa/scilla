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

(* Monadic evaluation results *)
let fail s = Error s
let pure e = return e

(* Monadic fold-left for error *)
let rec foldM ~f ~init ls = match ls with
  | x :: ls' ->
      let%bind res = f init x in
      foldM ~f:f ~init:res ls'
  | [] -> Ok init

(* Monadic fold-right for error *)
let rec foldrM ~f ~init ls = match ls with
  | x :: ls' ->
      let%bind rest = foldrM ~f:f ~init:init ls' in
      f rest x
  | [] -> Ok init

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

