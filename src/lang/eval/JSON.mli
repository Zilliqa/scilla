(*
 * Copyright (c) 2018 - present Zilliqa, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** The state input is a json containing an array state variables.
 ** Each state variable is a list of the following key value pairs:
 **    "vname" : "variable name"
 **    "type" : "valid scilla type"
 **    "value" : "value of the variable as a string"
 **)

module StateInput : sig

  (** 
  **  Returns a list of (vname:string,value:literal) items
  **  from the json in the input filename. Invalid inputs in the json are ignored 
  **)
  val get_json_data : string -> (string * Syntax.literal) list

end
