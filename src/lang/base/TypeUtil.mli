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

open Core
open ErrorUtils
open Syntax

(* An inferred type with possible qualifiers *)
type 'rep inferred_type = {
  tp   : typ;
  qual : 'rep
} [@@deriving sexp]

(* Qualifiers to type inference with additional information *)
module type QualifiedTypes = sig
  type t
  val t_of_sexp : Sexp.t -> t
  val sexp_of_t : t -> Sexp.t
  val mk_qualified_type : typ -> t inferred_type      
end

module type MakeTEnvFunctor = functor
  (Q: QualifiedTypes)
  (R : Rep)
  -> sig

    (* Resolving results *)
    type resolve_result
    val rr_loc : resolve_result -> loc
    val rr_rep : resolve_result -> R.rep
    val rr_typ : resolve_result -> Q.t inferred_type
    val rr_pp  : resolve_result -> string
    val mk_qual_tp : typ -> Q.t inferred_type

    module TEnv : sig
      type t
      (* Make new type environment *)
      val mk : t
      (* Add to type environment *)
      val addT : t -> R.rep ident -> typ -> t
      (* Add to many type bindings *)
      val addTs : t -> (R.rep ident * typ) list -> t
      (* Add type variable to the environment *)
      val addV : t -> R.rep ident -> t
      (* Append env' to env in place. *)
      val append : t -> t -> t
      (* Retain only those keys for which (fb k) is true. *)
      val filterTs : t -> f:(string -> bool) -> t
      (* Check type for well-formedness in the type environment *)
      val is_wf_type : t -> typ -> (unit, scilla_error list) result
      (* Resolve the identifier *)    
      val resolveT : ?lopt:(R.rep option) -> t -> string ->
        (resolve_result, scilla_error list) result
      (* Is bound in environment? *)
      val existsT : t -> string -> bool
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
    
  val literal_type : literal -> (typ, scilla_error list) result
  val is_wellformed_lit : literal -> (typ, scilla_error list) result

  (* Useful generic types *)
  val fun_typ : typ -> typ -> typ
  val tvar : string -> typ
  val tfun_typ : string -> typ -> typ
  val map_typ : typ -> typ -> typ
  val unit_typ : typ

  (****************************************************************)
  (*                       Type sanitization                      *)
  (****************************************************************)

  val is_storable_type : typ -> bool
  val is_serializable_type : typ -> bool
  val is_ground_type : typ -> bool
  val is_non_map_ground_type : typ -> bool
  val get_msgevnt_type : (string * 'a) sexp_list -> (typ, scilla_error sexp_list) result
  val map_access_type : typ -> int -> (typ, scilla_error list) result
  val map_depth : typ -> int

  (****************************************************************)
  (*             Utility function for matching types              *)
  (****************************************************************)

  val type_equiv : typ -> typ -> bool
  val type_equiv_list : typ list -> typ list -> bool

  val assert_type_equiv : typ -> typ -> (unit, scilla_error list) result

  (* Applying a function type *)
  val fun_type_applies : typ -> typ list -> (typ, scilla_error list) result
  (* Applying a procedure "type" *)
  val proc_type_applies : typ list -> typ list -> (unit list, scilla_error list) result

  (* Applying a type function *)
  val elab_tfun_with_args : typ -> typ list -> (typ, scilla_error list) result

  val pp_typ_list : typ list -> string  

  (****************************************************************)
  (*                        Working with ADTs                     *)
  (****************************************************************)

  (*  Apply type substitution  *)
  val apply_type_subst : (string * typ) list -> typ -> typ

  (*  Get elaborated type for a constructor and list of type arguments *)    
  val elab_constr_type : string -> typ list -> (typ, scilla_error list) result  

  (* For a given instantiated ADT and a construtor name, get type *
     assignments. This is the main working horse of type-checking
     pattern-matching. *)    
  val constr_pattern_arg_types : typ -> string -> (typ list, scilla_error list) result  

  val validate_param_length : string -> int -> int -> (unit, scilla_error list) result

  val assert_all_same_type : typ list -> (unit, scilla_error list) result

end

(****************************************************************)
(*                  Built-in typed entities                     *)
(****************************************************************)

val blocknum_name : string

