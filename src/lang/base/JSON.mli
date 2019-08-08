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
  val state_to_json : ((string * Syntax.literal) list) -> Yojson.Basic.t

  (* Given an init.json filename, return extlib fields *)
  val get_init_extlibs : string -> (string * string) list

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
  val message_to_json : ((string * Syntax.literal) list) -> Yojson.Basic.t

end

module BlockChainState : sig

  (** 
   **  Returns a list of (vname:string,value:literal) items
   **  from the json in the input filename.
   **)
  val get_json_data : string -> (string * Syntax.literal) list

end

module ContractInfo : sig
open ParserUtil.ParsedSyntax
  (* Given a parsed contract, give a string JSON with these details:
       { 
         "name" : "foo",
         "params" : [ 
           { "name" : "param1",
             "type" : "Int32" },
           { "name" : "param2",
             "type" : "String" }
          ],
          "fields" : [
           { "name" : "field1",
             "type" : "Int32" },
           { "name" : "field2",
             "type" : "String" }
          ],
          "transitions" : [
            {
              "name" : "bar",
              "params" : [
                {"name" : "barparam1", "type" : "Uint128"},
                {"name" : "barparam2", "type" : "Uint128"},
              ] 
            },
            {
              "name" : "bar2",
              "params" : [
                {"name" : "bar1param1", "type" : "Uint128"},
                {"name" : "bar1param2", "type" : "Uint128"},
              ] 
            }
          ]
        }
  *)
  val get_string : int -> contract -> (string * (string * Syntax.typ) list) list -> string
  val get_json : int -> contract -> (string * (string * Syntax.typ) list) list -> Yojson.Basic.t

end

module Event : sig

  (** 
   ** Prints an Event "(string, literal) list" as a json to the 
   ** and returns the string. pp enables pretty printing.
   **)
  val event_to_jstring : ?pp:bool -> (string * Syntax.literal) list -> string
  (* Same as Event_to_jstring, but instead gives out raw json, not it's string *)
  val event_to_json : (string * Syntax.literal) list -> Yojson.Basic.t

end

module CashflowInfo : sig
  (* Given: A pair of lists.
            The first element is an association list from fields to their tags.
            The second element is an association list from adts to their contstructor tags.
            Constructor tags are an association list from constructors to their argument tags.
            An argument tag is either a normal tag, or "_" if the argument was ignored during the cashflow analysis.

     Output: A string JSON with these details:

     {
       "cashflow_tags" : {
           "State variables": [
               { "field" : "owners",
                 "tag" : "NotMoney" },
               { "field" : "donations",
                 "tag" : "MapMoney" }
           ],
           "ADT constructors" : [
               {
                   "Type1" : [
                       { "constructor" : "Test1", "tags" : [ "NoInfo", "_", "NotMoney" ] },
                       { "constructor" : "Test2", "tags" : [ ] }
                   ]
               }               
           ]
        }
     }

  *)

  val get_json : ((string * string) list * (string * ((string * string list) list)) list) -> Yojson.Basic.t
end
