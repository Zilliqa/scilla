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

open Core_kernel
open ErrorUtils
open Literal
open Syntax
module TULiteral = GlobalLiteral
module TUType = TULiteral.LType
module TUIdentifier = TUType.TIdentifier
module TUName = TUIdentifier.Name

(* An inferred type with possible qualifiers *)
type 'rep inferred_type = { tp : TUType.t; qual : 'rep } [@@deriving sexp]

(* Qualifiers to type inference with additional information *)
module type QualifiedTypes = sig
  type t

  val t_of_sexp : Sexp.t -> t

  val sexp_of_t : t -> Sexp.t

  val mk_qualified_type : TUType.t -> t inferred_type
end

module type MakeTEnvFunctor = functor (Q : QualifiedTypes) (R : Rep) -> sig
  (* Resolving results *)
  type resolve_result

  val rr_loc : resolve_result -> loc

  val rr_rep : resolve_result -> R.rep

  val rr_typ : resolve_result -> Q.t inferred_type

  val rr_pp : resolve_result -> string

  val mk_qual_tp : TUType.t -> Q.t inferred_type

  module TEnv : sig
    type t

    type restore

    (* Make new type environment *)
    val mk : unit -> t

    (* Add to type environment *)
    val addT : t -> R.rep TUIdentifier.t -> TUType.t -> restore

    (* Add to many type bindings *)
    val addTs : t -> (R.rep TUIdentifier.t * TUType.t) list -> restore

    (* Add type variable to the environment *)
    val addV : t -> R.rep TUIdentifier.t -> restore

    (* Add many type variables to the environment. *)
    val addVs : t -> R.rep TUIdentifier.t list -> restore

    (* Remove the latest binding for the argument. *)
    val remT : t -> R.rep TUIdentifier.t -> restore

    (* Remove the latest bindings for the arguments. *)
    val remTs : t -> R.rep TUIdentifier.t list -> restore

    (* Restore the environment by applying the restore object. *)
    val apply_restore : t -> restore -> unit

    (* Combine a new list of restores with an older list. *)
    val combine_restores : older:restore -> newer:restore -> restore

    (* Check type for well-formedness in the type environment *)
    val is_wf_type : t -> TUType.t -> (unit, scilla_error list) result

    (* Resolve the identifier *)
    val resolveT :
      ?lopt:R.rep option ->
      t ->
      TUName.t ->
      (resolve_result, scilla_error list) result

    (* Is bound in environment? *)
    val existsT : t -> TUName.t -> bool

    (* Is bound in tvars? *)
    val existsV : t -> TUName.t -> bool

    (* Convert to list *)
    val to_list : t -> (string * resolve_result) list

    (* Get type variables *)
    val tvars : t -> (string * R.rep) list

    (* Print the type environment *)
    val pp : ?f:(string * resolve_result -> bool) -> t -> string
  end
end

module PlainTypes : QualifiedTypes

module TypeUtilities : sig
  module MakeTEnv : MakeTEnvFunctor

  val literal_type :
    ?lc:ErrorUtils.loc ->
    ?expected:TUType.t option ->
    TULiteral.t ->
    (TUType.t * (TUType.t * TULiteral.Bystrx.t) list, scilla_error list) result

  val is_wellformed_lit :
    ?lc:ErrorUtils.loc -> TULiteral.t -> (TUType.t, scilla_error list) result

  (* Useful generic types *)
  val fun_typ : TUType.t -> TUType.t -> TUType.t

  val tvar : string -> TUType.t

  val tfun_typ : string -> TUType.t -> TUType.t

  val map_typ : TUType.t -> TUType.t -> TUType.t

  val unit_typ : TUType.t

  (****************************************************************)
  (*                       Type sanitization                      *)
  (****************************************************************)

  val is_legal_message_field_type : TUType.t -> bool

  val is_legal_transition_parameter_type : TUType.t -> bool

  val is_legal_procedure_parameter_type : TUType.t -> bool

  val is_legal_field_type : TUType.t -> bool

  val is_ground_type : TUType.t -> bool

  val get_msgevnt_type :
    (string * 'a) list -> loc -> (TUType.t, scilla_error list) result

  val map_access_type : TUType.t -> int -> (TUType.t, scilla_error list) result

  val map_depth : TUType.t -> int

  val address_field_type :
    'a TUIdentifier.t -> TUType.t -> (TUType.t, scilla_error list) result

  (****************************************************************)
  (*             Utility function for matching types              *)
  (****************************************************************)

  val type_equiv_list : TUType.t list -> TUType.t list -> bool

  type typeCheckerErrorType = TypeError | GasError

  val mk_type_error0 : string -> typeCheckerErrorType * scilla_error list

  val mk_type_error1 : string -> loc -> typeCheckerErrorType * scilla_error list

  val wrap_error_with_errortype :
    typeCheckerErrorType ->
    ('a, 'b) result ->
    ('a, typeCheckerErrorType * 'b) result

  val mark_error_as_type_error :
    ('a, 'b) result -> ('a, typeCheckerErrorType * 'b) result

  val type_assignable_list :
    to_list:TUType.t list -> from_list:TUType.t list -> bool

  val assert_type_assignable :
    ?lc:ErrorUtils.loc ->
    expected:TUType.t ->
    actual:TUType.t ->
    (unit, scilla_error list) result

  (* Applying a function type *)
  val fun_type_applies :
    ?lc:ErrorUtils.loc ->
    TUType.t ->
    TUType.t list ->
    (TUType.t, scilla_error list) result

  (* Applying a procedure "type" *)
  val proc_type_applies :
    lc:ErrorUtils.loc ->
    TUType.t list ->
    TUType.t list ->
    (unit list, scilla_error list) result

  (* Applying a type function without gas charge (for builtins) *)
  val elab_tfun_with_args_no_gas :
    TUType.t -> TUType.t list -> (TUType.t, scilla_error list) result

  val pp_typ_list_error : TUType.t list -> string

  (****************************************************************)
  (*                        Working with ADTs                     *)
  (****************************************************************)

  (*  Apply type substitution  *)
  val apply_type_subst : (string * TUType.t) list -> TUType.t -> TUType.t

  (*  Get elaborated type for a constructor and list of type arguments *)
  val elab_constr_type :
    lc:ErrorUtils.loc ->
    TUName.t ->
    TUType.t list ->
    (TUType.t, scilla_error list) result

  (* For a given instantiated ADT and a construtor name, get type *
     assignments. This is the main working horse of type-checking
     pattern-matching. *)
  val constr_pattern_arg_types :
    ?lc:ErrorUtils.loc ->
    TUType.t ->
    TUName.t ->
    (TUType.t list, scilla_error list) result

  val validate_param_length :
    lc:ErrorUtils.loc ->
    TUName.t ->
    int ->
    int ->
    (unit, scilla_error list) result

  val assert_all_same_type :
    lc:ErrorUtils.loc -> TUType.t list -> (unit, scilla_error list) result
end

(****************************************************************)
(*                  Built-in typed entities                     *)
(****************************************************************)

val blocknum_name : string
