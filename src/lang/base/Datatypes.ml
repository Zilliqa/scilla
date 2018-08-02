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

open Syntax
open Core
open MonadUtil
open Result.Let_syntax

(**********************************************************)
(*                 Built-in Algebraic Data Types          *)
(**********************************************************)

(* A tagged constructor *)
type constructor = {
  cname : string; (* constructor name *)
  arity : int;    (* How many arguments it takes *)  
}

(* An Algebraic Data Type *)
type adt = {
  tname    : string; (* type name *)
  tparams    : string list;    (* type parameters *)

  (* supported constructors *)
  tconstr  : constructor list;

  (* Mapping for constructors' types
     The arity of the constructor is the same as the length
     of the list, so the types are mapped correspondingly. *)
  tmap     : (string * (typ list)) list;

  (* recur    : Syntax.loc Env.value *)
}

module DataTypeDictionary = struct
  (* Booleans *)
  let c_true = { cname = "True"; arity = 0 }
  let c_false = { cname = "False"; arity = 0 }
  let t_bool = {
    tname = "Bool";
    tparams = [];
    tconstr = [c_true; c_false];
    tmap = []
  }

  (* Natural numbers *)
  let c_zero = { cname = "Zero"; arity = 0 }
  let c_succ = { cname = "Succ"; arity = 1 }
  let t_nat = {
    tname = "Nat";
    tparams = [];
    tconstr = [c_zero; c_succ];
    tmap = [("Succ", [ADT ("Nat", [])])]
  }

  
  (* Option *)
  let c_some = { cname = "Some"; arity = 1 }
  let c_none = { cname = "None"; arity = 0 }
  let t_option = {
    tname = "Option";
    tparams = ["'A"];
    tconstr = [c_some; c_none];
    tmap = [
      ("Some", [TypeVar "'A"])
    ]
  }             
  
  (* Lists *)
  let c_cons = { cname = "Cons"; arity = 2 }
  let c_nil  = { cname = "Nil"; arity = 0 }
  let t_list = {
    tname = "List";
    tparams = ["'A"];
    tconstr = [c_cons; c_nil];
    tmap = [
      ("Cons", [TypeVar "'A";
                ADT ("List", [TypeVar "'A"])])
    ]
  }

  (* Products (Pairs) *)
  let c_pair = { cname = "Pair"; arity = 2 }
  let t_product = {
    tname = "Pair";
    tparams = ["'A"; "'B"];
    tconstr = [c_pair];
    tmap = [
      ("Pair", [(TypeVar "'A"); (TypeVar "'B")])
    ]
  }

  type t = adt list  
  let dict = [t_bool; t_nat; t_option; t_list; t_product]

  (*  Get ADT by name *)
  let lookup_name name =
    match List.find dict ~f:(fun t -> t.tname = name) with
    | None ->
      fail @@ sprintf "ADT %s not found" name
    | Some a ->
        pure (a)

  (*  Get ADT by the constructor *)
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

  let constr_tmap adt cn = 
    List.find adt.tmap ~f:(fun (n, _) -> n = cn) |> Option.map ~f:snd

  let bool_typ = ADT (t_bool.tname, [])
  let nat_typ = ADT (t_nat.tname, [])
  let option_typ t = ADT (t_option.tname, [t])
  let list_typ t = ADT (t_list.tname, [t])
  let pair_typ t s = ADT (t_product.tname, [t; s])

end

(* TODO: support user_defined data types *)


