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

(* TODO: Change this to CanonicalLiteral = Literals based on canonical names. *)
module TULiteral = FlattenedLiteral
module TUType = TULiteral.LType
module TUIdentifier = TUType.TIdentifier

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

    (* Make new type environment *)
    val mk : t

    (* Add to type environment *)
    val addT : t -> R.rep TUIdentifier.t -> TUType.t -> t

    (* Add to many type bindings *)
    val addTs : t -> (R.rep TUIdentifier.t * TUType.t) list -> t

    (* Add type variable to the environment *)
    val addV : t -> R.rep TUIdentifier.t -> t

    (* Append env' to env in place. *)
    val append : t -> t -> t

    (* Retain only those keys for which (fb k v) is true. *)
    val filterTs : t -> f:(string -> resolve_result -> bool) -> t

    (* Check type for well-formedness in the type environment *)
    val is_wf_type : t -> TUType.t -> (unit, scilla_error list) result

    (* Resolve the identifier *)
    val resolveT :
      ?lopt:R.rep option ->
      t ->
      string ->
      (resolve_result, scilla_error list) result

    (* Is bound in environment? *)
    val existsT : t -> string -> bool

    (* Is bound in tvars? *)
    val existsV : t -> string -> bool

    (* Copy the environment *)
    val copy : t -> t

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

  val literal_type : TULiteral.t -> (TUType.t, scilla_error list) result

  val is_wellformed_lit : TULiteral.t -> (TUType.t, scilla_error list) result

  (* Useful generic types *)
  val fun_typ : TUType.t -> TUType.t -> TUType.t

  val tvar : string -> TUType.t

  val tfun_typ : string -> TUType.t -> TUType.t

  val map_typ : TUType.t -> TUType.t -> TUType.t

  val unit_typ : TUType.t

  (****************************************************************)
  (*                       Type sanitization                      *)
  (****************************************************************)

  val is_storable_type : TUType.t -> bool

  val is_serializable_type : TUType.t -> bool

  val is_ground_type : TUType.t -> bool

  val is_non_map_ground_type : TUType.t -> bool

  val get_msgevnt_type :
    (string * 'a) sexp_list -> (TUType.t, scilla_error sexp_list) result

  val map_access_type : TUType.t -> int -> (TUType.t, scilla_error list) result

  val map_depth : TUType.t -> int

  (****************************************************************)
  (*             Utility function for matching types              *)
  (****************************************************************)

  val type_equiv_list : TUType.t list -> TUType.t list -> bool

  type typeCheckerErrorType = TypeError | GasError

  val mk_type_error0 :
    string ->
    Stdint.uint64 ->
    typeCheckerErrorType * scilla_error list * Stdint.uint64

  val mk_type_error1 :
    string ->
    loc ->
    Stdint.uint64 ->
    typeCheckerErrorType * scilla_error list * Stdint.uint64

  val wrap_error_with_errortype_and_gas :
    typeCheckerErrorType ->
    Stdint.uint64 ->
    ('a, 'b) result ->
    ('a, typeCheckerErrorType * 'b * Stdint.uint64) result

  val mark_error_as_type_error :
    Stdint.uint64 ->
    ('a, 'b) result ->
    ('a, typeCheckerErrorType * 'b * Stdint.uint64) result

  val assert_type_equiv :
    TUType.t -> TUType.t -> (unit, scilla_error list) result

  val assert_type_equiv_with_gas :
    TUType.t ->
    TUType.t ->
    Stdint.uint64 ->
    ( Stdint.uint64,
      typeCheckerErrorType * scilla_error list * Stdint.uint64 )
    result

  (* Applying a function type *)
  val fun_type_applies :
    TUType.t -> TUType.t list -> (TUType.t, scilla_error list) result

  (* Applying a procedure "type" *)
  val proc_type_applies :
    TUType.t list -> TUType.t list -> (unit list, scilla_error list) result

  (* Applying a type function without gas charge (for builtins) *)
  val elab_tfun_with_args_no_gas :
    TUType.t -> TUType.t list -> (TUType.t, scilla_error list) result

  (* Applying a type function *)
  val elab_tfun_with_args :
    TUType.t ->
    TUType.t list ->
    Stdint.uint64 ->
    ( TUType.t * Stdint.uint64,
      typeCheckerErrorType * scilla_error list * Stdint.uint64 )
    result

  val pp_typ_list : TUType.t list -> string

  (****************************************************************)
  (*                        Working with ADTs                     *)
  (****************************************************************)

  (*  Apply type substitution  *)
  val apply_type_subst : (string * TUType.t) list -> TUType.t -> TUType.t

  (*  Get elaborated type for a constructor and list of type arguments *)
  val elab_constr_type :
    string -> TUType.t list -> (TUType.t, scilla_error list) result

  (* For a given instantiated ADT and a construtor name, get type *
     assignments. This is the main working horse of type-checking
     pattern-matching. *)
  val constr_pattern_arg_types :
    TUType.t -> string -> (TUType.t list, scilla_error list) result

  val validate_param_length :
    string -> int -> int -> (unit, scilla_error list) result

  val assert_all_same_type : TUType.t list -> (unit, scilla_error list) result
end

(****************************************************************)
(*                  Built-in typed entities                     *)
(****************************************************************)

val blocknum_name : string
