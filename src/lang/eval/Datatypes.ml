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
open MonadUtil

(* A tagged constructor *)
type constructor = {
  cname : string; (* constructor name *)
  arity : int;    (* How many arguments it takes *)  
}

(* An Algebraic Data Type *)
type adt = {
  tname    : string; (* type name *)
  targs    : string list;    (* type parameters *)

  (* supported constructors *)
  tconstr  : constructor list;

  (* Mapping for constructors' types *)
  tmap     : (string * (int * typ) list) list;
}

module DataTypeDictionary = struct
  (* Booleans *)
  let c_true = { cname = "True"; arity = 0 }
  let c_false = { cname = "False"; arity = 0 }
  let t_bool = {
    tname = "Bool";
    targs = [];
    tconstr = [c_true; c_false];
    tmap = []
  }
  
  (* Option *)
  let c_some = { cname = "Some"; arity = 1 }
  let c_none = { cname = "None"; arity = 0 }
  let t_option = {
    tname = "Option";
    targs = ["a"];
    tconstr = [c_some; c_none];
    tmap = [
      ("Some", [(0, TypeVar "a")])
    ]
  }             
  
  (* Lists *)
  let c_cons = { cname = "Cons"; arity = 2 }
  let c_nil  = { cname = "Nil"; arity = 0 }
  let t_list = {
    tname = "List";
    targs = ["a"];
    tconstr = [c_cons; c_nil];
    tmap = [
      ("Cons", [(0, TypeVar "a");
                (1, ADT ("List", [TypeVar "a"]))])
    ]
  }

  type t = adt list  
  let dict = [t_bool; t_option; t_list]

  let lookup_constructor cn =
    match List.find dict
      ~f:(fun t -> let cns = t.tconstr in
           List.exists cns ~f:(fun c -> c.cname = cn)) with
    | None -> fail @@
        sprintf "No data type with constructor %s found" cn
    | Some dt ->
        (match List.find dt.tconstr ~f:(fun c -> c.cname = cn) with
         | None -> fail @@
             sprintf "Data type %s must have constructor %s."
               dt.tname cn
         | Some ctr -> pure (dt, ctr))
  
end

(* TODO: support user_defined data types *)


