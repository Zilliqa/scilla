(*
  This file is part of scilla.

  Copyright (c) 2021 - present Zilliqa Research Pvt. Ltd.

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

open Scilla_base
open Syntax
open ErrorUtils
open BuiltIns
open MonadUtil

module ScillaEvalBuiltIns (SR : Rep) (ER : Rep) : sig
  module EvalBuiltInDictionary : sig
    (* Takes the expected type as an argument to elaborate the result *)
    type ('a, 'b) built_in_executor =
      BIType.t list ->
      (* type arguments *)
      BILiteral.t list ->
      (* value arguments *)
      BIType.t ->
      (* result type *)
      (BILiteral.t, scilla_error list, 'a -> 'b) CPSMonad.t

    (* Returns a pair:
     * - The result type for given argument types, e.g., Bool
     * - The executor for evaluating the operation      
     *)
    val find_builtin_op :
      ER.rep builtin_annot ->
      targtypes:BIType.t list ->
      (* type arguments *)
      vargtypes:BIType.t list ->
      (* types of value arguments *)
      ( BIType.t * ('a, 'b) built_in_executor,
        scilla_error list,
        'a -> 'b )
      CPSMonad.t
  end
end
