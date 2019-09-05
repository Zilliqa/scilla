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
  tname      : string;         (* type name *)
  tparams    : string list;    (* type parameters *)

  (* supported constructors *)
  tconstr  : constructor list;

  (* Mapping for constructors' types
     The arity of the constructor is the same as the length
     of the list, so the types are mapped correspondingly. *)
  tmap     : (string * (typ list)) list;
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

  (* adt.tname -> adt *)
  let adt_name_dict =
    let open Caml in
    let ht : ((string, adt) Hashtbl.t) = Hashtbl.create 5 in
    let _ = Hashtbl.add ht t_bool.tname t_bool in
    let _ = Hashtbl.add ht t_nat.tname t_nat in
    let _ = Hashtbl.add ht t_option.tname t_option in
    let _ = Hashtbl.add ht t_list.tname t_list in
    let _ = Hashtbl.add ht t_product.tname t_product in
      ht

  (* tconstr -> (adt * constructor) *)
  let adt_cons_dict =
    let open Caml in
    let ht : ((string, (adt * constructor)) Hashtbl.t) = Hashtbl.create 10 in
    Hashtbl.iter (fun _ a -> List.iter (fun c -> Hashtbl.add ht c.cname (a, c)) a.tconstr)
      adt_name_dict;
    ht

  let add_adt (new_adt : adt) error_loc =
    let open Caml in
    match Hashtbl.find_opt adt_name_dict new_adt.tname with
    | Some _ ->
        fail1 (sprintf "Multiple declarations of type %s" new_adt.tname) error_loc
    | None ->
        let _ = Hashtbl.add adt_name_dict new_adt.tname new_adt in
        foldM new_adt.tconstr ~init:()
          ~f:(fun () ctr ->
              match Hashtbl.find_opt adt_cons_dict ctr.cname with
              | Some _ -> fail1 (sprintf "Multiple declarations of type constructor %s" ctr.cname) error_loc
              | None ->
                  pure @@ Hashtbl.add adt_cons_dict ctr.cname (new_adt, ctr))
  
  (*  Get ADT by name *)
  let lookup_name name =
    let open Caml in
    match Hashtbl.find_opt adt_name_dict name with
    | None ->
      fail0 @@ sprintf "ADT %s not found" name
    | Some a ->
        pure (a)

  (*  Get ADT by the constructor *)
  let lookup_constructor cn =
    let open Caml in
    match Hashtbl.find_opt adt_cons_dict cn with
    | None -> fail0 @@
        sprintf "No data type with constructor %s found" cn
    | Some dt ->
      pure dt

  (* Get typing map for a constructor *)
  let constr_tmap adt cn = 
    List.find adt.tmap ~f:(fun (n, _) -> n = cn) |> Option.map ~f:snd

  let bool_typ = ADT (t_bool.tname, [])
  let nat_typ = ADT (t_nat.tname, [])
  let option_typ t = ADT (t_option.tname, [t])
  let list_typ t = ADT (t_list.tname, [t])
  let pair_typ t s = ADT (t_product.tname, [t; s])

  (* Get all known ADTs *)
  let get_all_adts () = Caml.Hashtbl.fold (fun _ a acc -> a :: acc) adt_name_dict []

  (* Get all known ADT constructors *)
  let get_all_ctrs () = Caml.Hashtbl.fold (fun _ c acc -> c :: acc) adt_cons_dict []

end


(* Convert Scilla list to OCaml list.
 * Not tail recursive. Don't use for long lists. *)
let scilla_list_to_ocaml v =
  let open Result.Let_syntax in
  let rec convert_to_list = (function
    | ADTValue ("Nil", _, []) -> pure []
    | ADTValue ("Cons", _, [h; t]) ->
        let%bind rest = convert_to_list t in
        pure @@ h :: rest
    | _ -> fail0 @@ sprintf "Cannot convert scilla list to ocaml list:\n")
  in
  convert_to_list v

(* Convert Scilla list to reverse OCaml list.
 * Tail recursive. *)
let scilla_list_to_ocaml_rev v =
  let rec convert_to_list l acc =
    match l with
    | ADTValue ("Nil", _, []) -> pure acc
    | ADTValue ("Cons", _, [h;t]) ->
        convert_to_list t (h::acc)
    | _ -> fail0 @@ sprintf "Cannot convert scilla list to reverse ocaml list:\n"
  in
  convert_to_list v []
