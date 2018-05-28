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

let f0_id = Ident ("f0", dummy_loc)
let fn_id = Ident ("fn", dummy_loc)
let n1_id = (Ident ("n1", dummy_loc))
let res_id = (Ident ("res", dummy_loc))

let nat_fix_id = Ident ("nat_fix", dummy_loc)
let nat_fix_formal = Ident ("n", dummy_loc)
let nat_fix = Fixpoint (
    nat_fix_id,
    nat_fix_formal,
    nat_type,
    MatchExpr (nat_fix_formal, [
    (Constructor ("Zero", []), Var f0_id);
    (Constructor ("Succ", [Binder n1_id]),
     Let (res_id, None, App (nat_fix_id, [n1_id]),
          App (fn_id, [n1_id; res_id])))]))

(* So far, only for natural numbers *)
(* let nat_rec_typ =
  FunType(nat_type,
          FunType(FunType(nat_type, nat_type),
                 FunType(nat_type, nat_type))) *)

let nat_rec =
  Env.ValClosure (f0_id, nat_type,
       Fun (fn_id, FunType (nat_type, nat_type),
           nat_fix), Env.empty)

let nat_rec_id = "nat_rec"

end

let recursion_principles =
 [
   (NatRec.nat_rec_id, NatRec.nat_rec)
 ]
