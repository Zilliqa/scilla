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

let tvar = Ident("T", dummy_loc)
let tid = get_id tvar

(* Adopted one, as flod_left and fold_right are equivalent for
 * natural numbers *)
module Foldl = struct
let g = Ident ("foldl_g", dummy_loc)
let fold_fix = Fixpoint (
    g,
    FunType((FunType (TypeVar tid, FunType (nat_type, TypeVar tid))),
            FunType(TypeVar tid,
                    FunType(nat_type, TypeVar tid))),
    Fun (fn_formal,
         FunType (TypeVar tid, FunType (nat_type, TypeVar tid)),
         Fun (f0_formal,
              TypeVar tid,
              Fun(n_formal,
                  nat_type,
                  MatchExpr (n_formal, [
                      (Constructor ("Zero", []), Var f0_formal);
                      (Constructor ("Succ", [Binder n1_id]), 
                        Let (res_id, None, App (fn_formal, [f0_formal; n1_id]),
                             App (g, [fn_formal; res_id; n1_id])))])                   
                 ))))            

let id = "nat_fold"

let fold = Env.ValTypeClosure(tvar, fold_fix, Env.empty)

end

module Foldr = struct

let g = Ident ("foldr_g", dummy_loc)
let fold_fix = Env.ValFix (
    g,
    FunType((FunType (nat_type, FunType (TypeVar tid, TypeVar tid))),
            FunType(TypeVar tid,
                    FunType(nat_type, TypeVar tid))),
    Fun (fn_formal,
         FunType (nat_type, FunType (TypeVar tid, TypeVar tid)),
         Fun (f0_formal,
              nat_type,
              Fun(n_formal,
                  nat_type,
                  MatchExpr (n_formal, [
                      (Constructor ("Zero", []), Var f0_formal);
                      (Constructor ("Succ", [Binder n1_id]), 
                       Let (res_id, None, App (g, [fn_formal; f0_formal; n1_id]),
                            App (fn_formal, [n1_id; res_id])))])                   
                 ))),
  Env.empty)            

let id = "nat_foldr"
end


end

let recursion_principles = 
 [
   (NatRec.Foldl.id, NatRec.Foldl.fold)
 ]

