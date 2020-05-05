(*
  This file is part of scilla.

  Copyright (c) 2020 - present Zilliqa Research Pvt. Ltd.
  
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
  [@@deriving sexp]

  type pattern =
    | Wildcard
    | Binder of ParserRep.rep SIdentifier.t
    | Constructor of ParserRep.rep SIdentifier.t * pattern list
  [@@deriving sexp]

  type expr_annot = expr * ParserRep.rep

  and expr =
    | Literal of SLiteral.t
    | Var of ParserRep.rep SIdentifier.t
    | Let of ParserRep.rep SIdentifier.t * SType.t option * expr_annot * expr_annot
    | Message of (string * payload) list
    | Fun of ParserRep.rep SIdentifier.t * SType.t * expr_annot
    | App of ParserRep.rep SIdentifier.t * ParserRep.rep SIdentifier.t list
    | Constr of ParserRep.rep SIdentifier.t * SType.t list * ParserRep.rep SIdentifier.t list
    | MatchExpr of ParserRep.rep SIdentifier.t * (pattern * expr_annot) list
    | Builtin of ParserRep.rep builtin_annot * ParserRep.rep SIdentifier.t list
    (* Advanced features: to be added in Scilla 0.2 *)
    | TFun of ParserRep.rep SIdentifier.t * expr_annot
    | TApp of ParserRep.rep SIdentifier.t * SType.t list
    (* Fixpoint combinator: used to implement recursion principles *)
    | Fixpoint of ParserRep.rep SIdentifier.t * SType.t * expr_annot
  [@@deriving sexp]

  val expr_rep : expr_annot -> ParserRep.rep

  val expr_loc : expr_annot -> loc option

  (* SExp printing for Expr for structural printing. *)
  val spp_expr : expr -> string

  val pp_expr : expr -> string

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
    | MatchStmt of ParserRep.rep SIdentifier.t * (pattern * stmt_annot list) list
    | ReadFromBC of ParserRep.rep SIdentifier.t * string
    | AcceptPayment
    (* forall l p *)
    | Iterate of ParserRep.rep SIdentifier.t * ParserRep.rep SIdentifier.t
    | SendMsgs of ParserRep.rep SIdentifier.t
    | CreateEvnt of ParserRep.rep SIdentifier.t
    | CallProc of ParserRep.rep SIdentifier.t * ParserRep.rep SIdentifier.t list
    | Throw of ParserRep.rep SIdentifier.t option
  [@@deriving sexp]

  val stmt_rep : stmt_annot -> ParserRep.rep

  val stmt_loc : stmt_annot -> loc

  val spp_stmt : stmt -> string

  val pp_stmt : stmt -> string

  (**************************************************)
  (*          Statement evaluation info             *)
  (**************************************************)
  type stmt_eval_context =
    (* literal being loaded *)
    | G_Load of SLiteral.t
    (* literal being stored *)
    | G_Store of SLiteral.t
    (* none *)
    | G_Bind
    (* nesting depth, new value *)
    | G_MapUpdate of int * SLiteral.t option
    (* nesting depth, literal retrieved *)
    | G_MapGet of int * SLiteral.t option
    (* number of clauses *)
    | G_MatchStmt of int
    | G_ReadFromBC
    | G_AcceptPayment
    | G_SendMsgs of SLiteral.t list
    | G_CreateEvnt of SLiteral.t
    | G_CallProc

  (*******************************************************)
  (*                    Contracts                        *)
  (*******************************************************)

  type component = {
    comp_type : component_type;
    comp_name : ParserRep.rep SIdentifier.t;
    comp_params : (ParserRep.rep SIdentifier.t * SType.t) list;
    comp_body : stmt_annot list;
  }

  type ctr_def = { cname : ParserRep.rep SIdentifier.t; c_arg_types : SType.t list }

  type lib_entry =
    | LibVar of ParserRep.rep SIdentifier.t * SType.t option * expr_annot
    | LibTyp of ParserRep.rep SIdentifier.t * ctr_def list

  type library = { lname : ParserRep.rep SIdentifier.t; lentries : lib_entry list }

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
    cname : ParserRep.rep SIdentifier.t;
    libs : library option;
    (* lib functions defined in the module *)
    (* List of imports / external libs with an optional namespace. *)
    elibs : (ParserRep.rep SIdentifier.t * ParserRep.rep SIdentifier.t option) list;
    contr : contract;
  }

  (* Library module *)
  type lmodule = {
    smver : int;
    (* Scilla major version of the library. *)
    (* List of imports / external libs with an optional namespace. *)
    elibs : (ParserRep.rep SIdentifier.t * ParserRep.rep SIdentifier.t option) list;
    libs : library; (* lib functions defined in the module *)
  }

  (* A tree of libraries linked to their dependents *)
  type libtree = {
    libn : library;
    (* The library this node represents *)
    deps : libtree list; (* List of dependent libraries *)
  }

  val pp_cparams : (ParserRep.rep SIdentifier.t * SType.t) list -> string

  (* Substitute type for a type variable *)
  val subst_type_in_expr : ParserRep.rep SIdentifier.t -> SType.t -> expr_annot -> expr_annot

  (* get variables that get bound in pattern. *)
  val get_pattern_bounds : pattern -> ParserRep.rep SIdentifier.t list

  (* Returns a list of free variables in expr. *)
  val free_vars_in_expr : expr_annot -> ParserRep.rep SIdentifier.t list

  (* Is expr dependent on any ident in blist.
   * This is the same as checking if a free var
   * in expr is present in blist. *)
  val free_vars_dep_check : expr_annot -> ParserRep.rep SIdentifier.t list -> bool

  (****************************************************************)
  (*                  Better error reporting                      *)
  (****************************************************************)
  val get_failure_msg : expr_annot -> string -> string -> (string * loc)

  val get_failure_msg_stmt : stmt_annot -> string -> string -> (string * loc)

  val wrap_with_info : string * loc -> ('a, scilla_error list) result -> ('a, scilla_error list) result
      
  val wrap_err : expr_annot -> string -> ?opt:string -> ('a, scilla_error list) result -> ('a, scilla_error list) result

  val wrap_serr : stmt_annot -> string -> ?opt:string -> ('a, scilla_error list) result -> ('a, scilla_error list) result

end

(*******************************************************)
(*       Syntax as generated by the parser             *)
(*******************************************************)

(* TODO: Change this to LocalLiteral = Literals based on local names. *)
module ParserSyntax = ScillaSyntax (ParserRep) (ParserRep) (FlattenedLiteral)
