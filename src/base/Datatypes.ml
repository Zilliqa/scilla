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

open Core_kernel
open! Int.Replace_polymorphic_compare
open Literal
open MonadUtil
open Result.Let_syntax

module DTLiteral = GlobalLiteral
module DTType = DTLiteral.LType
module DTIdentifier = DTType.TIdentifier
module DTName = DTIdentifier.Name
open DTType
open DTLiteral

(**********************************************************)
(*                 Built-in Algebraic Data Types          *)
(**********************************************************)

(* A tagged constructor *)
type constructor = {
  cname : DTName.t;
  (* constructor name *)
  arity : int; (* How many arguments it takes *)
}
[@@deriving equal]

(* An Algebraic Data Type *)
type adt = {
  tname : DTName.t;
  (* type name *)
  tparams : string list;
  (* type parameters *)

  (* supported constructors *)
  tconstr : constructor list;
  (* Mapping for constructors' types
     The arity of the constructor is the same as the length
     of the list, so the types are mapped correspondingly. *)
  tmap : (DTName.t * DTType.t list) list;
}
[@@deriving equal]

module DataTypeDictionary = struct

  let dtname_of_string str = DTName.parse_simple_name str
  let dtid_of_string str = DTIdentifier.mk_loc_id @@ dtname_of_string str
  
  (* Booleans *)
  let c_true = { cname = dtname_of_string "True"; arity = 0 }

  let c_false = { cname = dtname_of_string "False"; arity = 0 }

  let t_bool =
    { tname = dtname_of_string "Bool"; tparams = []; tconstr = [ c_true; c_false ]; tmap = [] }

  (* Natural numbers *)
  let c_zero = { cname = dtname_of_string "Zero"; arity = 0 }

  let c_succ = { cname = dtname_of_string "Succ"; arity = 1 }

  let t_nat =
    {
      tname = dtname_of_string "Nat";
      tparams = [];
      tconstr = [ c_zero; c_succ ];
      tmap = [ (dtname_of_string "Succ", [ ADT (dtid_of_string "Nat", []) ]) ];
    }

  (* Option *)
  let c_some = { cname = dtname_of_string "Some"; arity = 1 }

  let c_none = { cname = dtname_of_string "None"; arity = 0 }

  let t_option =
    {
      tname = dtname_of_string "Option";
      tparams = [ "'A" ];
      tconstr = [ c_some; c_none ];
      tmap = [ (dtname_of_string "Some", [ TypeVar "'A" ]) ];
    }

  (* Lists *)
  let c_cons = { cname = dtname_of_string "Cons"; arity = 2 }

  let c_nil = { cname = dtname_of_string "Nil"; arity = 0 }

  let t_list =
    {
      tname = dtname_of_string "List";
      tparams = [ "'A" ];
      tconstr = [ c_cons; c_nil ];
      tmap =
        [
          ( dtname_of_string "Cons",
            [
              TypeVar "'A"; ADT (dtid_of_string "List", [ TypeVar "'A" ]);
            ] );
        ];
    }

  (* Products (Pairs) *)
  let c_pair = { cname = dtname_of_string "Pair"; arity = 2 }

  let t_product =
    {
      tname = dtname_of_string "Pair";
      tparams = [ "'A"; "'B" ];
      tconstr = [ c_pair ];
      tmap = [ (dtname_of_string "Pair", [ TypeVar "'A"; TypeVar "'B" ]) ];
    }

  (* adt.tname -> adt *)
  let adt_name_dict = Caml.Hashtbl.create 5

  (* tconstr -> (adt * constructor) *)
  let adt_cons_dict = Caml.Hashtbl.create 10

  (* Re-initialize environment dictionaries *)
  let reinit () =
    Caml.Hashtbl.(
      reset adt_name_dict;
      reset adt_cons_dict;
      add adt_name_dict t_bool.tname t_bool;
      add adt_name_dict t_nat.tname t_nat;
      add adt_name_dict t_option.tname t_option;
      add adt_name_dict t_list.tname t_list;
      add adt_name_dict t_product.tname t_product;
      iter
        (fun _ a ->
          Caml.List.iter (fun c -> add adt_cons_dict c.cname (a, c)) a.tconstr)
        adt_name_dict)

  let add_adt (new_adt : adt) error_loc =
    let open Caml in
    match Hashtbl.find_opt adt_name_dict new_adt.tname with
    | Some _ ->
        fail1
          (sprintf "Multiple declarations of type %s" (DTName.as_error_string new_adt.tname))
          error_loc
    | None ->
        let _ = Hashtbl.add adt_name_dict new_adt.tname new_adt in
        foldM new_adt.tconstr ~init:() ~f:(fun () ctr ->
            match Hashtbl.find_opt adt_cons_dict ctr.cname with
            | Some _ ->
                fail1
                  (sprintf "Multiple declarations of type constructor %s"
                     (DTName.as_error_string ctr.cname))
                  error_loc
            | None -> pure @@ Hashtbl.add adt_cons_dict ctr.cname (new_adt, ctr))

  (*  Get ADT by name *)
  let lookup_name ?(sloc = ErrorUtils.dummy_loc) name =
    let open Caml in
    match Hashtbl.find_opt adt_name_dict name with
    | None -> fail1 (sprintf "ADT %s not found" (DTName.as_error_string name)) sloc
    | Some a -> pure a

  (*  Get ADT by the constructor *)
  let lookup_constructor ?(sloc = ErrorUtils.dummy_loc) cn =
    let open Caml in
    match Hashtbl.find_opt adt_cons_dict cn with
    | None -> fail1 (sprintf "No data type with constructor %s found" (DTName.as_error_string cn)) sloc
    | Some dt -> pure dt

  (* Get typing map for a constructor *)
  let constr_tmap adt cn = List.Assoc.find adt.tmap cn ~equal:[%equal: DTName.t]

  let bool_typ = ADT (TIdentifier.mk_loc_id t_bool.tname, [])

  let nat_typ = ADT (TIdentifier.mk_loc_id t_nat.tname, [])

  let option_typ t = ADT (TIdentifier.mk_loc_id t_option.tname, [ t ])

  let list_typ t = ADT (TIdentifier.mk_loc_id t_list.tname, [ t ])

  let pair_typ t s = ADT (TIdentifier.mk_loc_id t_product.tname, [ t; s ])

  (* Get all known ADTs *)
  let get_all_adts () =
    Caml.Hashtbl.fold (fun _ a acc -> a :: acc) adt_name_dict []

  (* Get all known ADT constructors *)
  let get_all_ctrs () =
    Caml.Hashtbl.fold (fun _ c acc -> c :: acc) adt_cons_dict []
end

(* Helper functions for matching against names *)
let match_simple_names n m = [%equal: DTName.t] n m
let is_nil_ctr_name = match_simple_names DataTypeDictionary.c_nil.cname
let is_cons_ctr_name = match_simple_names DataTypeDictionary.c_cons.cname
let is_pair_ctr_name = match_simple_names DataTypeDictionary.c_pair.cname
let is_zero_ctr_name = match_simple_names DataTypeDictionary.c_zero.cname
let is_succ_ctr_name = match_simple_names DataTypeDictionary.c_succ.cname
let is_list_adt_name = match_simple_names DataTypeDictionary.t_list.tname

(* Convert Scilla list to OCaml list.
 * Not tail recursive. Don't use for long lists. *)
let scilla_list_to_ocaml v =
  let open Result.Let_syntax in
  let rec convert_to_list = function
    | ADTValue (c, _, [])
      when is_nil_ctr_name c -> pure []
    | ADTValue (c, _, [ h; t ])
      when is_cons_ctr_name c ->
        let%bind rest = convert_to_list t in
        pure @@ (h :: rest)
    | _ -> fail0 @@ sprintf "Cannot convert scilla list to ocaml list:\n"
  in
  convert_to_list v

(* Convert Scilla list to reverse OCaml list.
 * Tail recursive. *)
let scilla_list_to_ocaml_rev v =
  let rec convert_to_list l acc =
    match l with
    | ADTValue (c, _, [])
      when is_nil_ctr_name c -> pure acc
    | ADTValue (c, _, [ h; t ])
      when is_cons_ctr_name c -> convert_to_list t (h :: acc)
    | _ ->
        fail0 @@ sprintf "Cannot convert scilla list to reverse ocaml list:\n"
  in
  convert_to_list v []

module SnarkTypes = struct
  open Scilla_crypto.Snark
  open DataTypeDictionary

  let scalar_type = bystrx_typ scalar_len

  let g1point_type = pair_typ scalar_type scalar_type

  let g2point_type = pair_typ (bystrx_typ g2comp_len) (bystrx_typ g2comp_len)

  let g2comp_type = bystrx_typ g2comp_len

  let g1g2pair_type = pair_typ g1point_type g2point_type

  let g1g2pair_list_type = list_typ g1g2pair_type

  let scilla_scalar_to_ocaml s =
    match s with
    | ByStrX s' when Bystrx.width s' = scalar_len ->
        pure @@ Bystrx.to_raw_bytes s'
    | _ -> fail0 @@ sprintf "Cannot convert scilla G1 point to ocaml G1 point."

  let scilla_g1point_to_ocaml g1p =
    match g1p with
    | ADTValue (c, [ pxt; pyt ], [ ByStrX px; ByStrX py ])
      when is_pair_ctr_name c
           && [%equal: DTType.t] pxt scalar_type
           && [%equal: DTType.t] pyt scalar_type
           && Bystrx.width px = scalar_len
           && Bystrx.width py = scalar_len ->
        pure { g1x = Bystrx.to_raw_bytes px; g1y = Bystrx.to_raw_bytes py }
    | _ -> fail0 @@ sprintf "Cannot convert scilla G1 point to ocaml G1 point."

  let scilla_g2point_to_ocaml g2p =
    match g2p with
    | ADTValue (c, [ pxt; pyt ], [ ByStrX px; ByStrX py ])
      when is_pair_ctr_name c
           && [%equal: DTType.t] pxt g2comp_type
           && [%equal: DTType.t] pyt g2comp_type
           && Bystrx.width px = g2comp_len
           && Bystrx.width py = g2comp_len ->
        pure { g2x = Bystrx.to_raw_bytes px; g2y = Bystrx.to_raw_bytes py }
    | _ -> fail0 @@ sprintf "Cannot convert scilla G2 point to ocaml G2 point."

  let ocaml_g1point_to_scilla_lit g1p =
    match
      ( Bystrx.of_raw_bytes scalar_len g1p.g1x,
        Bystrx.of_raw_bytes scalar_len g1p.g1y )
    with
    | Some x, Some y ->
        pure
        @@ ADTValue
          (DataTypeDictionary.c_pair.cname,
           [ g1point_type; g1point_type ], [ ByStrX x; ByStrX y ])
    | _ -> fail0 @@ sprintf "Cannot convert OCaml G1 point to Scilla literal."

  let scilla_g1g2pairlist_to_ocaml g1g2pl =
    let%bind g1g2ol = scilla_list_to_ocaml g1g2pl in
    let%bind g1g2ol' =
      mapM g1g2ol ~f:(fun g1g2p_lit ->
          match g1g2p_lit with
          | ADTValue (c, [ g1pt; g2pt ], [ g1p; g2p ])
            when is_pair_ctr_name c
                 && [%equal: DTType.t] g1pt g1point_type
                 && [%equal: DTType.t] g2pt g2point_type ->
              let%bind g1p' = scilla_g1point_to_ocaml g1p in
              let%bind g2p' = scilla_g2point_to_ocaml g2p in
              pure (g1p', g2p')
          | _ ->
              fail0 @@ sprintf "Cannot convert scilla G1-G2 pair list to ocaml.")
    in
    pure g1g2ol'
end
