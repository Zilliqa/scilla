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
open TypeUtil
open FrontEndParser

(***********************************************************)
(*    Recursion principles for built-in ADTs               *)
(***********************************************************)

(* Folding over natural numbers *)
module NatRec = struct
  let g = mk_ident "g"
  let tvar = mk_ident "'T"
      
  (* Adopted one, as flod_left and fold_right are equivalent for
   * natural numbers *)
  module Foldl = struct
    (* The type of the fixpoint argument *)
    let fix_type = parse_type "('T -> Nat -> 'T) -> 'T -> Nat -> 'T"
    (* The type of the entire recursion primitive *)
    let full_type = tfun_typ "'T" fix_type
    let fix_arg = parse_expr ( 
        "fun (fn : 'T -> Nat -> 'T) => fun (f0 : 'T) => fun (n: Nat) => " ^
        "match n with " ^
        " | Zero => f0 " ^
        " | Succ n1 => let res = fn f0 n1 in " ^
        "   g fn res n1 " ^
        "end"
      )
    let id = mk_ident "nat_fold"
    let fold_fix = Fixpoint (g, fix_type, fix_arg)                    
    let fold = TFun(tvar, fold_fix)    
    let entry = ({lname = id; lexp = fold}, full_type)
  end

  module Foldr = struct

    let fix_type = parse_type "(Nat -> 'T -> 'T) -> 'T -> Nat -> 'T"
    let fix_arg = parse_expr ( 
        "fun (fn : Nat -> 'T -> 'T) => fun (f0 : 'T) => fun (n: Nat) => " ^
        "match n with " ^
        " | Zero => f0 " ^
        " | Succ n1 => let res = g fn res n1 in " ^
        "   fn n1 f0 " ^
        "end"
      )
    let id = mk_ident "nat_foldr"
  end
end

(* Folding over lists *)
module ListRec = struct
  let g = mk_ident "g"
  let avar = mk_ident "'A"
  let bvar = mk_ident "'B"

  module Foldl = struct
    (* The type of the fixpoint argument *)
    let fix_type = parse_type "('B -> 'A -> 'B) -> 'B -> (List 'A) -> 'B"
    (* The type of the primitive *)
    let full_type = tfun_typ "'A" (tfun_typ "'B" fix_type)
    let fix_arg = parse_expr ( 
        "fun (f : 'B -> 'A -> 'B) => fun (z : 'B) => fun (l: List 'A) => " ^
        "match l with " ^
        " | Nil => z " ^
        " | Cons h t => let res = f z h in " ^
        "   g f res t " ^
        "end"
      )
    let id = mk_ident "list_foldl"      
    let fold_fix = Fixpoint (g, fix_type, fix_arg)
    let fold = TFun(avar, TFun (bvar, fold_fix))
    let entry = ({lname = id; lexp = fold}, full_type)
  end
  
  module Foldr = struct
    (* The type of the fixpoint argument *)
    let fix_type = parse_type "('A -> 'B -> 'B) -> 'B -> (List 'A) -> 'B"
    (* The type of the primitive *)
    let full_type = tfun_typ "'A" (tfun_typ "'B" fix_type)        
    let fix_arg = parse_expr ( 
        "fun (f : 'A -> 'B -> 'B) => fun (z : 'B) => fun (l: List 'A) => " ^
        "match l with " ^
        " | Nil => z " ^
        " | Cons h t => let res = g f z t in " ^
        "   f h res " ^
        "end"
      )
    let id = mk_ident "list_foldr"
    let fold_fix = Fixpoint (g, fix_type, fix_arg)
    let fold = TFun(avar, TFun (bvar, fold_fix))
    let entry = ({lname = id; lexp = fold}, full_type)
  end
  
end

let recursion_principles = 
  [
    NatRec.Foldl.entry;
    ListRec.Foldl.entry;
    ListRec.Foldr.entry;
  ]

