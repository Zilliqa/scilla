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
 ** Map variables are encoded with the "type" being set to "Map"
 ** and the actual keyType/valType and key/val being encoded as:
        "vname" : "backers",
        "type" : "Map",  
        "value" :
        [
         {
           "keyType" : "Address",
           "valType" : "Int"
         },
         {
           "key" : "0x1234567890123456789012345678901234567890",
           "val" : "100"
        },
       ]
    i.e., the "value" for Maps is an array of key/val pairs, with
    the first such pair being an exception, used to store the
    types of the key/val, specified as "keyType" and "valType".
 **)

module ContractState : sig

  (** 
  **  Returns a list of (vname:string,value:literal) items
  **  from the json in the input filename. Invalid inputs in the json are ignored 
  **)
  val get_json_data : string -> (string * Syntax.literal) list
  (** Prints a list of state variables (string, literal)
   ** as a json to the specified output filename.Afl_instrument
   **)
  val put_json_data : ?pp:bool -> string -> ((string * Syntax.literal) list) -> unit

end
