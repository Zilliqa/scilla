(*
 * Copyright (c) 2018 - present , Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open Core
open Big_int
open Syntax

type 'rep inferred_type = {
  tp   : typ;
  qual : 'rep
} [@@deriving sexp]

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
    (* Resolve the identifier *)
    val resolveT : ?lopt:(loc option) -> t -> string -> (resolve_result, string) result
    (* Copy the environment *)
    val copy : t -> t
    (* Convert to list *)
    val to_list : t -> (string * resolve_result) list
    (* Print the type environment *)
    val pp : ?f:(string * resolve_result -> bool) -> t -> string        
  end
end

module PlainTypes : QualifiedTypes
module MakeTEnv : MakeTEnvFunctor
