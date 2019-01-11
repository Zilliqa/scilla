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

open Syntax
open Core
open ParserUtil
open FrontEndParser

(***********************************************************)
(*    Recursion principles for built-in ADTs               *)
(***********************************************************)

open ParsedSyntax

(* Folding over natural numbers *)
module NatRec = struct
  let g = mk_ident "g"
  let tvar = mk_ident "'T"

    (* Adopted one, as flod_left and fold_right are equivalent for
     * natural numbers *)
    module Foldl = struct
      (* The type of the fixpoint argument *)
      let fix_type = parse_type "('T -> Nat -> 'T) -> 'T -> Nat -> 'T"
      let (_, loc) as fix_arg = parse_expr ( 
          "fun (fn : 'T -> Nat -> 'T) => fun (f0 : 'T) => fun (n: Nat) => " ^
          "match n with " ^
          " | Zero => f0 " ^
          " | Succ n1 => let res = fn f0 n1 in " ^
          "   g fn res n1 " ^
          "end"
        )
      let id = mk_ident "nat_fold"
      let fold_fix = (Fixpoint (g, fix_type, fix_arg), loc)
      let fold = (TFun(tvar, fold_fix), loc)
      let entry = LibVar (id, fold)
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
      let (_, loc) as fix_arg = parse_expr ( 
          "fun (f : 'B -> 'A -> 'B) => fun (z : 'B) => fun (l: List 'A) => " ^
          "match l with " ^
          " | Cons h t => let res = f z h in " ^
          "   g f res t " ^
          " | Nil => z " ^
          "end"
        )
      let id = mk_ident "list_foldl"      
      let fold_fix = (Fixpoint (g, fix_type, fix_arg), loc)
      let fold = (TFun(avar, (TFun (bvar, fold_fix), loc)), loc)
      let entry = LibVar (id, fold)
    end

    module Foldr = struct
      (* The type of the fixpoint argument *)
      let fix_type = parse_type "('A -> 'B -> 'B) -> 'B -> (List 'A) -> 'B"
      let (_, loc) as fix_arg = parse_expr ( 
          "fun (f : 'A -> 'B -> 'B) => fun (z : 'B) => fun (l: List 'A) => " ^
          "match l with " ^
          " | Cons h t => let res = g f z t in " ^
          "   f h res " ^
          " | Nil => z " ^
          "end"
        )
      let id = mk_ident "list_foldr"
      let fold_fix = (Fixpoint (g, fix_type, fix_arg), loc)
      let fold = (TFun(avar, (TFun (bvar, fold_fix), loc)), loc)
      let entry = LibVar (id, fold)
    end

  end

let recursion_principles = 
  [
    NatRec.Foldl.entry;
    ListRec.Foldl.entry;
    ListRec.Foldr.entry;
  ]

