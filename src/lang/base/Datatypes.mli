(*
 * Copyright (c) 2018 - present. 
 * Zilliqa, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open Syntax
open Core

(**********************************************************)
(*                 Built-in Algebraic Data Types          *)
(**********************************************************)

type constructor = {
  cname : string; (* constructor name *)
  arity : int;    (* How many arguments it takes *)  
}

type adt = {
  tname    : string;
  tparams  : string list; 
  tconstr  : constructor list;
  tmap     : (string * (typ list)) list;
}

module DataTypeDictionary : sig
  (* Hiding the actual data type dicionary *)

  (*  Get ADT by name  *)
  val lookup_name : string -> (adt, string) result
  (*  Get ADT by the constructor  *)
  val lookup_constructor : string -> (adt * constructor, string) result
  (* Get typing map for a constructor *)
  val constr_tmap : adt -> string -> (typ list) option

  val refresh_adt : adt -> string list -> adt
  
  (*  Built-in ADTs  *)
  val bool_typ : typ
  val nat_typ : typ
  val option_typ : typ -> typ
  val list_typ : typ -> typ
  val pair_typ : typ -> typ -> typ

end
