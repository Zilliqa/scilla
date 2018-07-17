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

module BuiltInDictionary : sig
  type built_in_executor =
    literal list -> typ -> (literal, string) result

  (*  
   The return result is a triple:
   * The full elaborated type of the operation, e.g., string -> Bool
   * Its result type for given argument types, e.g., Bool
   * Executor for evaluating the operation      
   *)
  val find_builtin_op :
    string -> typ list -> ((typ * typ * built_in_executor), string) result
end

(* The first parameter is a string type *)
val build_int : typ -> string -> literal option
val validate_int_literal : literal -> bool
val is_int_type : typ -> bool
val is_uint_type : typ -> bool

(* Elaborator for the built-in typ *)
val elab_id : typ -> typ list -> (typ, string) result
