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

open Identifier
open Type
open Syntax
open Core_kernel
open! Int.Replace_polymorphic_compare
open FrontEndParser

(***********************************************************)
(*    Recursion principles for built-in ADTs               *)
(***********************************************************)

open ParsedSyntax

let parse_expr_wrapper expr =
  match parse_expr expr with
  | Error s -> PrettyPrinters.fatal_error s
  | Ok e -> e

let parse_type_wrapper expr =
  match parse_type expr with
  | Error s -> PrettyPrinters.fatal_error s
  | Ok e -> e

(* Folding over natural numbers *)
module NatRec = struct
  let g = asId "g"

  let fn = asId "fn"

  let tvar = asId "'T"

  (* Adopted one, as fold_left and fold_right are equivalent for
   * natural numbers *)
  module Foldl = struct
    (* The type of the fixpoint argument *)
    let fn_type = parse_type_wrapper "'T -> Nat -> 'T"

    let fix_type = fn_type

    let fold_type = parse_type_wrapper "('T -> Nat -> 'T) -> 'T -> Nat -> 'T"

    let fold_type_opt = Some (PolyFun (get_id tvar, fold_type))

    [@@@ocamlformat "disable"]

    let (_, loc) as fix_arg = parse_expr_wrapper ( 
        "fun (f0 : 'T) => fun (n: Nat) => " ^
        "match n with " ^
        " | Succ n1 => let res = fn f0 n1 in " ^
        "   g res n1 " ^
        " | Zero => f0 " ^
        "end"
      )

    [@@@ocamlformat "enable"]

    let id = asId "nat_fold"

    let fold_fix = (Fixpoint (g, fix_type, fix_arg), loc)

    let fold_fixed = (Fun (fn, fn_type, fold_fix), loc)

    let fold = (TFun (tvar, fold_fixed), loc)

    let entry = LibVar (id, fold_type_opt, fold)
  end

  module Foldk = struct
    (* The type of the fixpoint argument *)
    let fix_type = parse_type_wrapper "'T -> Nat -> 'T"

    let comb_type = parse_type_wrapper "'T -> Nat -> ('T -> 'T) -> 'T"

    let fold_type =
      parse_type_wrapper "('T -> Nat -> ('T -> 'T) -> 'T) -> 'T -> Nat -> 'T"

    let fold_type_opt = Some (PolyFun (get_id tvar, fold_type))

    [@@@ocamlformat "disable"]

    let (_, loc) as fix_arg = parse_expr_wrapper (
        "fun (f0 : 'T) => fun (n: Nat) => " ^
        "match n with " ^
        " | Succ n1 => let partial = fun (k : 'T) => g k n1 in " ^
        " fn f0 n partial " ^
        " | Zero => f0 " ^
        "end" )

    [@@@ocamlformat "enable"]

    let id = asId "nat_foldk"

    let fold_fix = (Fixpoint (g, fix_type, fix_arg), loc)

    let fold_fixed = (Fun (fn, comb_type, fold_fix), loc)

    let fold = (TFun (tvar, fold_fixed), loc)

    let entry = LibVar (id, fold_type_opt, fold)
  end
end

(* Folding over lists *)
module ListRec = struct
  let f = asId "f"

  let g = asId "g"

  let avar = asId "'A"

  let bvar = asId "'B"

  module Foldl = struct
    let f_type = parse_type_wrapper "'B -> 'A -> 'B"

    let fold_type =
      parse_type_wrapper "('B -> 'A -> 'B) -> 'B -> (List 'A) -> 'B"

    let fold_type_opt =
      Some (PolyFun (get_id avar, PolyFun (get_id bvar, fold_type)))

    (* The type of the fixpoint argument *)
    let fix_type = parse_type_wrapper "'B -> (List 'A) -> 'B"

    [@@@ocamlformat "disable"]

    let (_, loc) as fix_arg = parse_expr_wrapper (
        "fun (z : 'B) => fun (l: List 'A) => " ^
        "match l with " ^
        "| Cons h t => let res = f z h in g res t " ^
        "| Nil => z " ^
        "end"
    )

    [@@@ocamlformat "enable"]

    let id = asId "list_foldl"

    let fold_fix = (Fixpoint (g, fix_type, fix_arg), loc)

    let fold_fixed = (Fun (f, f_type, fold_fix), loc)

    let fold = (TFun (avar, (TFun (bvar, fold_fixed), loc)), loc)

    let entry = LibVar (id, fold_type_opt, fold)
  end

  module Foldr = struct
    let f_type = parse_type_wrapper "'A -> 'B -> 'B"

    let fold_type =
      parse_type_wrapper "('A -> 'B -> 'B) -> 'B -> (List 'A) -> 'B"

    let fold_type_opt =
      Some (PolyFun (get_id avar, PolyFun (get_id bvar, fold_type)))

    (* The type of the fixpoint argument *)
    let fix_type = parse_type_wrapper "'B -> (List 'A) -> 'B"

    [@@@ocamlformat "disable"]

    let (_, loc) as fix_arg = parse_expr_wrapper (
        "fun (z : 'B) => fun (l: List 'A) => " ^
        "match l with " ^
        "| Cons h t => let res = g z t in f h res " ^
        "| Nil => z " ^
        "end"
    )

    [@@@ocamlformat "enable"]

    let id = asId "list_foldr"

    let fold_fix = (Fixpoint (g, fix_type, fix_arg), loc)

    let fold_fixed = (Fun (f, f_type, fold_fix), loc)

    let fold = (TFun (avar, (TFun (bvar, fold_fixed), loc)), loc)

    let entry = LibVar (id, fold_type_opt, fold)
  end

  module Foldk = struct
    let comb_type = parse_type_wrapper "'B -> 'A -> ('B -> 'B) -> 'B"

    let fold_type =
      parse_type_wrapper
        "('B -> 'A -> ('B -> 'B) -> 'B) -> 'B -> (List 'A) -> 'B"

    let fold_type_opt =
      Some (PolyFun (get_id avar, PolyFun (get_id bvar, fold_type)))

    (* The type of the fixpoint argument *)
    let fix_type = parse_type_wrapper "'B -> (List 'A) -> 'B"

    [@@@ocamlformat "disable"]

    let (_, loc) as fix_arg = parse_expr_wrapper (
        "fun (z : 'B) => fun (l: List 'A) => " ^
        "match l with " ^
        "| Cons h t => let partial = fun (k : 'B) => g k t in " ^
        "   f z h partial " ^
        "| Nil => z " ^
        "end"
      )

    [@@@ocamlformat "enable"]

    let id = asId "list_foldk"

    let fold_fix = (Fixpoint (g, fix_type, fix_arg), loc)

    let fold_fixed = (Fun (f, comb_type, fold_fix), loc)

    let fold = (TFun (avar, (TFun (bvar, fold_fixed), loc)), loc)

    let entry = LibVar (id, fold_type_opt, fold)
  end
end

let recursion_principles =
  [
    NatRec.Foldl.entry;
    NatRec.Foldk.entry;
    ListRec.Foldk.entry;
    ListRec.Foldl.entry;
    ListRec.Foldr.entry;
  ]
