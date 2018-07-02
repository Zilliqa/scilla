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
module ContractState : sig

  (** 
   **  Returns a list of (vname:string,value:literal) items
   **  from the json in the input filename. Invalid inputs in the json are ignored 
   **)
  val get_json_data : string -> (string * Syntax.literal) list
  (** 
   ** Prints a list of state variables (string, literal)
   ** as a json and returns it as a string.
   ** pp enables pretty printing.
   **)
  val state_to_string : ?pp:bool -> ((string * Syntax.literal) list) -> string

  (* Get a json object from given states *)
  val state_to_json : ((string * Syntax.literal) list) -> Yojson.json

end

(** Message json parsing and printing. A message json has three mandatory
 *  fields "_tag" specifying the transition to be invoked, an "_amount"
 *  field, specifying amount to be transferred. and a "_sender" field
 *  do convey the sender of the message. These two are followed by an
 *  array of json objects for parameters to the transition. These parameters
 *  are encoded similar to the array of variables in ContractState. It is
 *  expected that the types of parameters match with those in the contract 
 *  definition.
    {
      "_tag" : "foo",
      "_sender" : "0x1234567890123456789012345678901234567890"
      "_amount": "100", 
      "params" :
      [
        {
          "vname" : "bar",
          "type" : "Uint128" ,
          "value" : "100"
        }
      ]
    }
 **)
module Message : sig

(** Parses and returns a list of (pname,pval), with
  "_tag", "_sender" and "_amount" at the beginning of this list.
  Invalid inputs in the json are ignored **)
  val get_json_data : string -> (string * Syntax.literal) list

  (** 
   ** Prints a message (string, literal) as a json to the 
   ** and returns the string. pp enables pretty printing.
   ** The difference b/w this and the one in ContractState is that
   ** this has a mandatory "_tag", "_sender" and "_amount" field,
   ** with the actual params themselves in an array json with
   ** name "params" (as described in comment in .mli file).
   **)
  val message_to_jstring : ?pp:bool -> ((string * Syntax.literal) list) -> string
  (* Same as message_to_jstring, but instead gives out raw json, not it's string *)
  val message_to_json : ((string * Syntax.literal) list) -> Yojson.json

end

module BlockChainState : sig

  (** 
   **  Returns a list of (vname:string,value:literal) items
   **  from the json in the input filename.
   **)
  val get_json_data : string -> (string * Syntax.literal) list

end

exception Invalid_json of string
