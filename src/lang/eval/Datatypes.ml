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

  (* recur    : Syntax.loc Env.value *)
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

  (* Natural numbers *)
  let c_zero = { cname = "Zero"; arity = 0 }
  let c_succ = { cname = "Succ"; arity = 1 }
  let t_nat = {
    tname = "Nat";
    targs = [];
    tconstr = [c_zero; c_succ];
    tmap = [("Succ", [(0, ADT ("Nat", []))])]
  }

  
  (* Option *)
  let c_some = { cname = "Some"; arity = 1 }
  let c_none = { cname = "None"; arity = 0 }
  let t_option = {
    tname = "Option";
    targs = ["'A"];
    tconstr = [c_some; c_none];
    tmap = [
      ("Some", [(0, TypeVar "'A")])
    ]
  }             
  
  (* Lists *)
  let c_cons = { cname = "Cons"; arity = 2 }
  let c_nil  = { cname = "Nil"; arity = 0 }
  let t_list = {
    tname = "List";
    targs = ["'A"];
    tconstr = [c_cons; c_nil];
    tmap = [
      ("Cons", [(0, TypeVar "'A");
                (1, ADT ("List", [TypeVar "'A"]))])
    ]
  }

  (* Products (Pairs) *)
  let c_pair = { cname = "Pair"; arity = 2 }
  let t_product = {
    tname = "Pair";
    targs = ["'A"; "'B"];
    tconstr = [c_pair];
    tmap = [
      ("Pair", [(0, TypeVar "a"); (1, TypeVar "b")])
    ]
  }

  type t = adt list  
  let dict = [t_bool; t_nat; t_option; t_list; t_product]

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


