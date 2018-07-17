(*
 * Copyright (c) 2018 - present. 
 * Zilliqa, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open Core
open Big_int
open Syntax

val subst_type_in_type: 'a ident -> typ -> typ -> typ
val subst_type_in_literal: 'a ident -> typ -> literal -> literal
val subst_type_in_expr: 'a ident -> typ -> 'a expr -> 'a expr

(* An inferred type with possible qualifiers *)
type 'rep inferred_type = {
  tp   : typ;
  qual : 'rep
} [@@deriving sexp]

(* Qualifiers to type inference with additional information *)
module type QualifiedTypes = sig
  type t
  val mk_qualified_type : typ -> t inferred_type      
end

module type MakeTEnvFunctor = functor (Q: QualifiedTypes) -> sig
  (* Resolving results *)
  type resolve_result
  val rr_loc : resolve_result -> loc
  val rr_typ : resolve_result -> Q.t inferred_type
  val rr_pp  : resolve_result -> string
  val mk_qual_tp : typ -> Q.t inferred_type
  
  module TEnv : sig
    type t
    (* Make new type environment *)
    val mk : t
    (* Add to type environment *)
    val addT : t -> loc ident -> typ -> t
    (* Add type variable to the environment *)
    val addV : t -> loc ident -> t
    (* Resolve the identifier *)
    val resolveT : ?lopt:(loc option) -> t -> string -> (resolve_result, string) result
    (* Copy the environment *)
    val copy : t -> t
    (* Convert to list *)
    val to_list : t -> (string * resolve_result) list
    (* Get type variables *)
    val tvars : t -> (string * loc) list
    (* Print the type environment *)
    val pp : ?f:(string * resolve_result -> bool) -> t -> string        
  end
end

module PlainTypes : QualifiedTypes
module MakeTEnv : MakeTEnvFunctor

val literal_type : literal -> (typ, string) result
