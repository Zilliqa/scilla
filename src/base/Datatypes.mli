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

open ErrorUtils
open Core_kernel
open Literal
module DTLiteral = GlobalLiteral
module DTType = DTLiteral.LType
module DTIdentifier = DTType.TIdentifier
module DTName = DTIdentifier.Name

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

module DataTypeDictionary : sig
  (* Hiding the actual data type dicionary *)

  (* Re-initialize environment with the built-in ADTs *)
  val reinit : unit -> unit

  (*  Get ADT by name  *)
  val lookup_name :
    ?sloc:ErrorUtils.loc -> DTName.t -> (adt, scilla_error list) result

  (*  Get ADT by the constructor  *)
  val lookup_constructor :
    ?sloc:ErrorUtils.loc ->
    DTName.t ->
    (adt * constructor, scilla_error list) result

  (* Get typing map for a constructor *)
  val constr_tmap : adt -> DTName.t -> DTType.t list option

  (* Get all known ADTs *)
  val get_all_adts : unit -> adt list

  (* Get all known ADT constructors *)
  val get_all_ctrs : unit -> (adt * constructor) list

  val add_adt : adt -> loc -> (unit, scilla_error list) result

  (*  Built-in ADTs  *)
  val bool_typ : DTType.t

  val nat_typ : DTType.t

  val option_typ : DTType.t -> DTType.t

  val list_typ : DTType.t -> DTType.t

  val pair_typ : DTType.t -> DTType.t -> DTType.t
end

val is_true_ctr_name : DTName.t -> bool

val is_false_ctr_name : DTName.t -> bool

val is_bool_adt_name : DTName.t -> bool

val is_nil_ctr_name : DTName.t -> bool

val is_cons_ctr_name : DTName.t -> bool

val is_list_adt_name : DTName.t -> bool

val is_pair_ctr_name : DTName.t -> bool

val is_pair_adt_name : DTName.t -> bool

val is_zero_ctr_name : DTName.t -> bool

val is_succ_ctr_name : DTName.t -> bool

val is_nat_adt_name : DTName.t -> bool

val is_none_ctr_name : DTName.t -> bool

val is_some_ctr_name : DTName.t -> bool

val is_option_adt_name : DTName.t -> bool

val scilla_list_to_ocaml :
  DTLiteral.t -> (DTLiteral.t list, scilla_error list) result

val scilla_list_to_ocaml_rev :
  DTLiteral.t -> (DTLiteral.t list, scilla_error list) result

module SnarkTypes : sig
  open Scilla_crypto.Snark

  val scalar_type : DTType.t

  val g1point_type : DTType.t

  val g2point_type : DTType.t

  val g2comp_type : DTType.t

  val g1g2pair_type : DTType.t

  val g1g2pair_list_type : DTType.t

  val scilla_scalar_to_ocaml : DTLiteral.t -> (scalar, scilla_error list) result

  val scilla_g1point_to_ocaml :
    DTLiteral.t -> (g1point, scilla_error list) result

  val scilla_g1g2pairlist_to_ocaml :
    DTLiteral.t -> ((g1point * g2point) list, scilla_error list) result

  val ocaml_g1point_to_scilla_lit :
    g1point -> (DTLiteral.t, scilla_error list) result
end
