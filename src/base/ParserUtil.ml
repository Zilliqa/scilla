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
open Literal
open Syntax
open Polynomials

(*******************************************************)
(*              Location annotations                   *)
(*******************************************************)

module ParserRep = struct
  type rep = loc [@@deriving sexp]

  let dummy_rep = dummy_loc

  let get_loc l = l

  let address_rep = dummy_loc

  let uint128_rep = dummy_loc

  let uint32_rep = dummy_loc

  let bnum_rep = dummy_loc

  let string_rep = dummy_loc

  let parse_rep _ = dummy_loc

  let get_rep_str r = get_loc_str r
end

(*******************************************************)
(*              Parser syntax signature                *)
(*******************************************************)

module type Syn = sig
  module SLiteral : ScillaLiteral

  module SType = SLiteral.LType
  module SIdentifier = SType.TIdentifier

  (*******************************************************)
  (*                   Expressions                       *)
  (*******************************************************)

  type payload = MLit of SLiteral.t | MVar of ParserRep.rep SIdentifier.t

  type pattern =
    | Wildcard
    | Binder of ParserRep.rep SIdentifier.t
    | Constructor of ParserRep.rep SIdentifier.t * pattern list

  type gas_charge =
    | StaticCost of int
    (* Each identifier in the polynomial must resolve to a literal during Eval. *)
    | DynamicCost of ParserRep.rep SIdentifier.t Polynomial.polynomial

  type expr_annot = expr * ParserRep.rep

  and expr =
    | Literal of SLiteral.t
    | Var of ParserRep.rep SIdentifier.t
    | Let of
        ParserRep.rep SIdentifier.t * SType.t option * expr_annot * expr_annot
    | Message of (string * payload) list
    | Fun of ParserRep.rep SIdentifier.t * SType.t * expr_annot
    | App of ParserRep.rep SIdentifier.t * ParserRep.rep SIdentifier.t list
    | Constr of
        ParserRep.rep SIdentifier.t
        * SType.t list
        * ParserRep.rep SIdentifier.t list
    | MatchExpr of ParserRep.rep SIdentifier.t * (pattern * expr_annot) list
    | Builtin of ParserRep.rep builtin_annot * ParserRep.rep SIdentifier.t list
    (* Advanced features: to be added in Scilla 0.2 *)
    | TFun of ParserRep.rep SIdentifier.t * expr_annot
    | TApp of ParserRep.rep SIdentifier.t * SType.t list
    (* Fixpoint combinator: used to implement recursion principles *)
    | Fixpoint of ParserRep.rep SIdentifier.t * SType.t * expr_annot
    | GasExpr of gas_charge * expr_annot

  (*******************************************************)
  (*                   Statements                        *)
  (*******************************************************)

  type stmt_annot = stmt * ParserRep.rep

  and stmt =
    | Load of ParserRep.rep SIdentifier.t * ParserRep.rep SIdentifier.t
    | Store of ParserRep.rep SIdentifier.t * ParserRep.rep SIdentifier.t
    | Bind of ParserRep.rep SIdentifier.t * expr_annot
    (* m[k1][k2][..] := v OR delete m[k1][k2][...] *)
    | MapUpdate of
        ParserRep.rep SIdentifier.t
        * ParserRep.rep SIdentifier.t list
        * ParserRep.rep SIdentifier.t option
    (* v <- m[k1][k2][...] OR b <- exists m[k1][k2][...] *)
    (* If the bool is set, then we interpret this as value retrieve,
       otherwise as an "exists" query. *)
    | MapGet of
        ParserRep.rep SIdentifier.t
        * ParserRep.rep SIdentifier.t
        * ParserRep.rep SIdentifier.t list
        * bool
    | MatchStmt of
        ParserRep.rep SIdentifier.t * (pattern * stmt_annot list) list
    | ReadFromBC of ParserRep.rep SIdentifier.t * string
    | AcceptPayment
    (* forall l p *)
    | Iterate of ParserRep.rep SIdentifier.t * ParserRep.rep SIdentifier.t
    | SendMsgs of ParserRep.rep SIdentifier.t
    | CreateEvnt of ParserRep.rep SIdentifier.t
    | CallProc of ParserRep.rep SIdentifier.t * ParserRep.rep SIdentifier.t list
    | Throw of ParserRep.rep SIdentifier.t option
    | GasStmt of gas_charge

  (*******************************************************)
  (*                    Contracts                        *)
  (*******************************************************)

  type component = {
    comp_type : component_type;
    comp_name : ParserRep.rep SIdentifier.t;
    comp_params : (ParserRep.rep SIdentifier.t * SType.t) list;
    comp_body : stmt_annot list;
  }

  type ctr_def = {
    cname : ParserRep.rep SIdentifier.t;
    c_arg_types : SType.t list;
  }

  type lib_entry =
    | LibVar of ParserRep.rep SIdentifier.t * SType.t option * expr_annot
    | LibTyp of ParserRep.rep SIdentifier.t * ctr_def list

  type library = {
    lname : ParserRep.rep SIdentifier.t;
    lentries : lib_entry list;
  }

  type contract = {
    cname : ParserRep.rep SIdentifier.t;
    cparams : (ParserRep.rep SIdentifier.t * SType.t) list;
    cconstraint : expr_annot;
    cfields : (ParserRep.rep SIdentifier.t * SType.t * expr_annot) list;
    ccomps : component list;
  }

  (* Contract module: libary + contract definiton *)
  type cmodule = {
    smver : int;
    (* Scilla major version of the contract. *)
    libs : library option;
    (* lib functions defined in the module *)
    elibs :
      (ParserRep.rep SIdentifier.t * ParserRep.rep SIdentifier.t option) list;
    (* List of imports / external libs with an optional namespace. *)
    contr : contract;
  }

  (* Library module *)
  type lmodule = {
    smver : int;
    (* Scilla major version of the library. *)
    (* List of imports / external libs with an optional namespace. *)
    elibs :
      (ParserRep.rep SIdentifier.t * ParserRep.rep SIdentifier.t option) list;
    libs : library; (* lib functions defined in the module *)
  }
end

(*******************************************************)
(*       Syntax as generated by the parser             *)
(*******************************************************)

(* TODO: This is the parameterisation, which needs to happen in the places where the parser is instantiated. *)
module ParserSyntax = ScillaSyntax (ParserRep) (ParserRep) (FlattenedLiteral)
