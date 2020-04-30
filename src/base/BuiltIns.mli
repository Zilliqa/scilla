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
open ErrorUtils
open Core_kernel
open Literal

(* TODO: Change this to CanonicalLiteral = Literals based on canonical names. *)
module BILiteral = FlattenedLiteral
module BIType = BILiteral.LType

module UsefulLiterals : sig
  val true_lit : BILiteral.t

  val false_lit : BILiteral.t

  val to_Bool : bool -> BILiteral.t

  val some_lit :
    BILiteral.t -> (BILiteral.t, ErrorUtils.scilla_error list) result

  val none_lit : BIType.t -> BILiteral.t

  val pair_lit :
    BILiteral.t ->
    BILiteral.t ->
    (BILiteral.t, ErrorUtils.scilla_error list) result
end

module ScillaBuiltIns (SR : Rep) (ER : Rep) : sig
  module BuiltInDictionary : sig
    type built_in_executor =
      BILiteral.t list -> BIType.t -> (BILiteral.t, scilla_error list) result

    (* The return result is a triple:
     * The full elaborated type of the operation, e.g., string -> Bool
     * Its result type for given argument types, e.g., Bool
     * Executor for evaluating the operation      
     *)
    val find_builtin_op :
      ER.rep builtin_annot ->
      BIType.t list ->
      (BIType.t * BIType.t * built_in_executor, scilla_error list) result
  end

  (* Elaborator for the built-in typ *)
  val elab_id :
    BIType.t -> BIType.t list -> (BIType.t, scilla_error list) result
end
