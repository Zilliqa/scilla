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
open Big_int
open EvalUtil

module NatRec = struct 

(* Recursion principles for built-in ADTs *)
let nat_type = ADT ("nat", [])

let n1_id = (Ident ("n1", dummy_loc))
let res_id = (Ident ("res", dummy_loc))

let f0_formal = Ident ("f0", dummy_loc)
let fn_formal = Ident ("fn", dummy_loc)
let n_formal = Ident ("n", dummy_loc)


(* Adopted one, as flod_left and fold_right are equivalent for
 * natural numbers *)
module Foldl = struct
let fold = Ident ("foldl_g", dummy_loc)
let fold_fix = Env.ValFix (
    fold,
    nat_type, (* TODO: Generalise! *)
    Fun (fn_formal,
         (FunType (nat_type, FunType (nat_type, nat_type))),
          Fun (f0_formal,
               nat_type,
               Fun(n_formal,
                   nat_type,
                   MatchExpr (n_formal, [
                       (Constructor ("Zero", []), Var f0_formal);
                       (Constructor ("Succ", [Binder n1_id]), 
                        Let (res_id, None, App (fn_formal, [f0_formal; n1_id]),
                             App (fold, [fn_formal; res_id; n1_id])))])                   
                  ))),
  Env.empty)            

let id = "nat_fold"
end

module Foldr = struct

let fold = Ident ("foldr_g", dummy_loc)
let fold_fix = Env.ValFix (
    fold,
    nat_type, (* TODO: Generalise! *)
    Fun (fn_formal,
         (FunType (nat_type, FunType (nat_type, nat_type))),
          Fun (f0_formal,
               nat_type,
               Fun(n_formal,
                   nat_type,
                   MatchExpr (n_formal, [
                       (Constructor ("Zero", []), Var f0_formal);
                       (Constructor ("Succ", [Binder n1_id]), 
                        Let (res_id, None, App (fold, [fn_formal; f0_formal; n1_id]),
                             App (fn_formal, [n1_id; res_id])))])                   
                  ))),
  Env.empty)            

let id = "nat_foldr"
end


end

let recursion_principles = 
 [
   (NatRec.Foldl.id, NatRec.Foldl.fold_fix)
 ]

