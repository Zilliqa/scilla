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
open ErrorUtils
open Identifier
open Type
open Literal
open MonadUtil
open Result.Let_syntax

module type Datatypes = sig

  module DTLiteral : Literal
  
  (**********************************************************)
  (*                 Built-in Algebraic Data Types          *)
  (**********************************************************)

  type constructor = {
    cname : string;
    (* constructor name *)
    arity : int; (* How many arguments it takes *)
  }
  [@@deriving equal]

  type adt = {
    tname : string;
    tparams : string list;
    tconstr : constructor list;
    tmap : (string * DTLiteral.LType.t list) list;
  }
  [@@deriving equal]

  module DataTypeDictionary : sig
    (* Hiding the actual data type dicionary *)

    (* Re-initialize environment with the built-in ADTs *)
    val reinit : unit -> unit

    (*  Get ADT by name  *)
    val lookup_name :
      ?sloc:ErrorUtils.loc -> string -> (adt, scilla_error list) result

    (*  Get ADT by the constructor  *)
    val lookup_constructor :
      ?sloc:ErrorUtils.loc ->
      string ->
      (adt * constructor, scilla_error list) result

    (* Get typing map for a constructor *)
    val constr_tmap : adt -> string -> DTLiteral.LType.t list option

    (* Get all known ADTs *)
    val get_all_adts : unit -> adt list

    (* Get all known ADT constructors *)
    val get_all_ctrs : unit -> (adt * constructor) list

    val add_adt : adt -> loc -> (unit, scilla_error list) result

    (*  Built-in ADTs  *)
    val bool_typ : DTLiteral.LType.t

    val nat_typ : DTLiteral.LType.t

    val option_typ : DTLiteral.LType.t -> DTLiteral.LType.t

    val list_typ : DTLiteral.LType.t -> DTLiteral.LType.t

    val pair_typ : DTLiteral.LType.t -> DTLiteral.LType.t -> DTLiteral.LType.t
  end

  val scilla_list_to_ocaml :
    DTLiteral.t -> (DTLiteral.t list, scilla_error list) result

  val scilla_list_to_ocaml_rev :
    DTLiteral.t -> (DTLiteral.t list, scilla_error list) result

  open Snark

  module SnarkTypes : sig
    val scalar_type : DTLiteral.LType.t

    val g1point_type : DTLiteral.LType.t

    val g2point_type : DTLiteral.LType.t

    val g2comp_type : DTLiteral.LType.t

    val g1g2pair_type : DTLiteral.LType.t

    val g1g2pair_list_type : DTLiteral.LType.t

    val scilla_scalar_to_ocaml : DTLiteral.t -> (scalar, scilla_error list) result

    val scilla_g1point_to_ocaml : DTLiteral.t -> (g1point, scilla_error list) result

    val scilla_g1g2pairlist_to_ocaml :
      DTLiteral.t -> ((g1point * g2point) list, scilla_error list) result

    val ocaml_g1point_to_scilla_lit :
      g1point -> (DTLiteral.t, scilla_error list) result
  end

end

module MkDatatype (Literal : Literal) = struct

  module DTLiteral = Literal
  module DTType = DTLiteral.LType
  module DTIdentifier = DTType.TIdentifier

  (**********************************************************)
  (*                 Built-in Algebraic Data Types          *)
  (**********************************************************)

  (* A tagged constructor *)
  type constructor = {
    cname : string;
    (* constructor name *)
    arity : int; (* How many arguments it takes *)
  }
  [@@deriving equal]

  (* An Algebraic Data Type *)
  type adt = {
    tname : string;
    (* type name *)
    tparams : string list;
    (* type parameters *)

    (* supported constructors *)
    tconstr : constructor list;
    (* Mapping for constructors' types
       The arity of the constructor is the same as the length
       of the list, so the types are mapped correspondingly. *)
    tmap : (string * DTType.t list) list;
  }
  [@@deriving equal]

  module DataTypeDictionary = struct
    (* Booleans *)
    let c_true = { cname = "True"; arity = 0 }

    let c_false = { cname = "False"; arity = 0 }

    let t_bool =
      { tname = "Bool"; tparams = []; tconstr = [ c_true; c_false ]; tmap = [] }

    (* Natural numbers *)
    let c_zero = { cname = "Zero"; arity = 0 }

    let c_succ = { cname = "Succ"; arity = 1 }

    let t_nat =
      {
        tname = "Nat";
        tparams = [];
        tconstr = [ c_zero; c_succ ];
        tmap = [ ("Succ", [ ADT (DTIdentifier.parse_builtin_adt_name "Nat", []) ]) ];
      }

    (* Option *)
    let c_some = { cname = "Some"; arity = 1 }

    let c_none = { cname = "None"; arity = 0 }

    let t_option =
      {
        tname = "Option";
        tparams = [ "'A" ];
        tconstr = [ c_some; c_none ];
        tmap = [ ("Some", [ TypeVar "'A" ]) ];
      }

    (* Lists *)
    let c_cons = { cname = "Cons"; arity = 2 }

    let c_nil = { cname = "Nil"; arity = 0 }

    let t_list =
      {
        tname = "List";
        tparams = [ "'A" ];
        tconstr = [ c_cons; c_nil ];
        tmap = [ ("Cons", [ TypeVar "'A"; ADT (DTIdentifier.parse_builtin_adt_name "List", [ TypeVar "'A" ]) ]) ];
      }

    (* Products (Pairs) *)
    let c_pair = { cname = "Pair"; arity = 2 }

    let t_product =
      {
        tname = "Pair";
        tparams = [ "'A"; "'B" ];
        tconstr = [ c_pair ];
        tmap = [ ("Pair", [ TypeVar "'A"; TypeVar "'B" ]) ];
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
            (sprintf "Multiple declarations of type %s" new_adt.tname)
            error_loc
      | None ->
          let _ = Hashtbl.add adt_name_dict new_adt.tname new_adt in
          foldM new_adt.tconstr ~init:() ~f:(fun () ctr ->
              match Hashtbl.find_opt adt_cons_dict ctr.cname with
              | Some _ ->
                  fail1
                    (sprintf "Multiple declarations of type constructor %s"
                       ctr.cname)
                    error_loc
              | None -> pure @@ Hashtbl.add adt_cons_dict ctr.cname (new_adt, ctr))

    (*  Get ADT by name *)
    let lookup_name ?(sloc = ErrorUtils.dummy_loc) name =
      let open Caml in
      match Hashtbl.find_opt adt_name_dict name with
      | None -> fail1 (sprintf "ADT %s not found" name) sloc
      | Some a -> pure a

    (*  Get ADT by the constructor *)
    let lookup_constructor ?(sloc = ErrorUtils.dummy_loc) cn =
      let open Caml in
      match Hashtbl.find_opt adt_cons_dict cn with
      | None -> fail1 (sprintf "No data type with constructor %s found" cn) sloc
      | Some dt -> pure dt

    (* Get typing map for a constructor *)
    let constr_tmap adt cn = List.Assoc.find adt.tmap cn ~equal:String.( = )

    let bool_typ = DTType.ADT (DTIdentifier.parse_builtin_adt_name t_bool.tname, [])

    let nat_typ = DTType.ADT (DTIdentifier.parse_builtin_adt_name t_nat.tname, [])

    let option_typ t = DTType.ADT (DTIdentifier.parse_builtin_adt_name t_option.tname, [ t ])

    let list_typ t = DTType.ADT (DTIdentifier.parse_builtin_adt_name t_list.tname, [ t ])

    let pair_typ t s = DTType.ADT (DTIdentifier.parse_builtin_adt_name t_product.tname, [ t; s ])

    (* Get all known ADTs *)
    let get_all_adts () =
      Caml.Hashtbl.fold (fun _ a acc -> a :: acc) adt_name_dict []

    (* Get all known ADT constructors *)
    let get_all_ctrs () =
      Caml.Hashtbl.fold (fun _ c acc -> c :: acc) adt_cons_dict []
  end

  (* Convert Scilla list to OCaml list.
   * Not tail recursive. Don't use for long lists. *)
  let scilla_list_to_ocaml v =
    let open Result.Let_syntax in
    let open DTLiteral in
    let rec convert_to_list = function
      | ADTValue ("Nil", _, []) -> pure []
      | ADTValue ("Cons", _, [ h; t ]) ->
          let%bind rest = convert_to_list t in
          pure @@ (h :: rest)
      | _ -> fail0 @@ sprintf "Cannot convert scilla list to ocaml list:\n"
    in
    convert_to_list v

  (* Convert Scilla list to reverse OCaml list.
   * Tail recursive. *)
  let scilla_list_to_ocaml_rev v =
    let rec convert_to_list l acc =
    let open DTLiteral in
      match l with
      | ADTValue ("Nil", _, []) -> pure acc
      | ADTValue ("Cons", _, [ h; t ]) -> convert_to_list t (h :: acc)
      | _ ->
          fail0 @@ sprintf "Cannot convert scilla list to reverse ocaml list:\n"
    in
    convert_to_list v []

  module SnarkTypes = struct
    open Snark
    open PrimTypes
    open DataTypeDictionary

    let scalar_type = bystrx_typ scalar_len

    let g1point_type = pair_typ scalar_type scalar_type

    let g2point_type = pair_typ (bystrx_typ g2comp_len) (bystrx_typ g2comp_len)

    let g2comp_type = bystrx_typ g2comp_len

    let g1g2pair_type = pair_typ g1point_type g2point_type

    let g1g2pair_list_type = list_typ g1g2pair_type

    let scilla_scalar_to_ocaml s =
      let open DTLiteral in
      match s with
      | ByStrX s' when Bystrx.width s' = scalar_len ->
          pure @@ Bystrx.to_raw_bytes s'
      | _ -> fail0 @@ sprintf "Cannot convert scilla G1 point to ocaml G1 point."

    let scilla_g1point_to_ocaml g1p =
      let open DTLiteral in
      match g1p with
      | ADTValue ("Pair", [ pxt; pyt ], [ ByStrX px; ByStrX py ])
        when [%equal: DTType.t] pxt scalar_type
          && [%equal: DTType.t] pyt scalar_type
          && Bystrx.width px = scalar_len
          && Bystrx.width py = scalar_len ->
          pure { g1x = Bystrx.to_raw_bytes px; g1y = Bystrx.to_raw_bytes py }
      | _ -> fail0 @@ sprintf "Cannot convert scilla G1 point to ocaml G1 point."

    let scilla_g2point_to_ocaml g2p =
      let open DTLiteral in
      match g2p with
      | ADTValue ("Pair", [ pxt; pyt ], [ ByStrX px; ByStrX py ])
        when [%equal: DTType.t] pxt g2comp_type
          && [%equal: DTType.t] pyt g2comp_type
          && Bystrx.width px = g2comp_len
          && Bystrx.width py = g2comp_len ->
          pure { g2x = Bystrx.to_raw_bytes px; g2y = Bystrx.to_raw_bytes py }
      | _ -> fail0 @@ sprintf "Cannot convert scilla G2 point to ocaml G2 point."

    let ocaml_g1point_to_scilla_lit g1p =
      let open DTLiteral in
      match
        ( Bystrx.of_raw_bytes scalar_len g1p.g1x,
          Bystrx.of_raw_bytes scalar_len g1p.g1y )
      with
      | Some x, Some y ->
          pure
          @@ ADTValue
            ("Pair", [ g1point_type; g1point_type ], [ ByStrX x; ByStrX y ])
      | _ -> fail0 @@ sprintf "Cannot convert OCaml G1 point to Scilla literal."

    let scilla_g1g2pairlist_to_ocaml g1g2pl =
      let%bind g1g2ol = scilla_list_to_ocaml g1g2pl in
      let%bind g1g2ol' =
        mapM g1g2ol ~f:(fun g1g2p_lit ->
            let open DTLiteral in
            match g1g2p_lit with
            | ADTValue ("Pair", [ g1pt; g2pt ], [ g1p; g2p ])
              when [%equal: DTType.t] g1pt g1point_type
                && [%equal: DTType.t] g2pt g2point_type ->
                let%bind g1p' = scilla_g1point_to_ocaml g1p in
                let%bind g2p' = scilla_g2point_to_ocaml g2p in
                pure (g1p', g2p')
            | _ ->
                fail0 @@ sprintf "Cannot convert scilla G1-G2 pair list to ocaml.")
      in
      pure g1g2ol'
  end

end
