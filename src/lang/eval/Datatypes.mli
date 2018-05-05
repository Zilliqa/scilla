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

type constructor = {
  cname : string; (* constructor name *)
  arity : int;    (* How many arguments it takes *)  
}

type adt = {
  tname    : string;
  targs    : string list; 
  tconstr  : constructor list;
  tmap     : (string * (int * typ) list) list;
}

module DataTypeDictionary : sig
  (* Hiding the actual data type dicionary *)
  val lookup_constructor : string -> (adt * constructor, string) result
end
