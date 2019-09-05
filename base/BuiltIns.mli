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
open Core

module UsefulLiterals : sig
  val true_lit : literal
  val false_lit : literal
  val to_Bool : bool -> literal

  val some_lit : literal -> (literal, ErrorUtils.scilla_error list) result
  val none_lit : typ -> literal

  val pair_lit : literal -> literal -> (literal, ErrorUtils.scilla_error list) result
end

module ScillaBuiltIns
    (SR : Rep)
    (ER : Rep) : sig

  module BuiltInDictionary : sig
    type built_in_executor =
      literal list -> typ -> (literal, scilla_error list) result

    (* The return result is a triple:
     * The full elaborated type of the operation, e.g., string -> Bool
     * Its result type for given argument types, e.g., Bool
     * Executor for evaluating the operation      
    *)
    val find_builtin_op :
      ER.rep builtin_annot -> typ list -> ((typ * typ * built_in_executor), scilla_error list) result
  end

  (* Elaborator for the built-in typ *)
  val elab_id : typ -> typ list -> (typ, scilla_error list) result

end
