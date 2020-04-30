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
open TypeUtil
open Literal
open Syntax
open ErrorUtils

(* This pass computes a list of all entities in the module for which
 * type information can be reported, along with their start and end locations.
 * Objects computed look like: "(name, type, start_loc, end_loc) list".
 * "name" may be empty if the entity being reported is an expression other than
 * a simple variable.
 * TODO: Until Issues #134, we cannot report types for expressions.
 * The alternate strategy is to have the client query for a particular variable
 * and we search through our AST for that location and report back its type.
 * We choose our current strategy for the following reasons:
    1. The client now has an option of caching the results and reusing based
      on what parts of the code were edited.
    2. Features such as code lens are easier to implement for the client. i.e.
      it can display the types of all entities (say in small / mild fonts) for
      the user to always see.
    3. It is simpler for us to fit this in our existing testsuite (selfish motive).
 *)

module ScillaTypeInfo
    (SR : Rep) (ER : sig
      include Rep

      val get_type : rep -> PlainTypes.t inferred_type
    end) =
struct
  module SER = SR
  module EER = ER

  (* TODO: Change this to CanonicalLiteral = Literals based on canonical names. *)
  module TILiteral = FlattenedLiteral
  module TIType = TILiteral.LType
  module TIIdentifier = TIType.TIdentifier
  module TISyntax = ScillaSyntax (SR) (ER) (TILiteral)
  open TIIdentifier
  open TISyntax

  (* Given an identifier, compute its type info. *)
  let calc_ident_locs i =
    let name = get_id i in
    let sloc = ER.get_loc (get_rep i) in
    (* Once Issue #134 is solved, this calculation can be avoided. *)
    let eloc = { sloc with cnum = sloc.cnum + String.length name } in
    let t = (ER.get_type (get_rep i)).tp in
    (name, t, sloc, eloc)

  let rec type_info_expr (e, _erep) =
    match e with
    | Literal _ -> []
    | Var a | TApp (a, _) -> [ calc_ident_locs a ]
    | Let (x, _, lhs, rhs) ->
        let lhs_l = type_info_expr lhs in
        let rhs_l = type_info_expr rhs in
        calc_ident_locs x :: (lhs_l @ rhs_l)
    | Message spl ->
        List.fold_right spl ~init:[] ~f:(fun (_, pl) acc ->
            match pl with MLit _ -> acc | MVar v -> calc_ident_locs v :: acc)
    | Fun (f, _, body) | Fixpoint (f, _, body) ->
        calc_ident_locs f :: type_info_expr body
    | App (i, il) -> List.map (i :: il) ~f:calc_ident_locs
    | Constr (_, _, il) ->
        (* Issue #456 prevents us form having a location for the constructor name. *)
        List.map il ~f:calc_ident_locs
    | MatchExpr (o, clauses) ->
        let ots = calc_ident_locs o in
        let clausets =
          List.map clauses ~f:(fun (p, branch) ->
              let patternvars = get_pattern_bounds p in
              let patternsts = List.map patternvars ~f:calc_ident_locs in
              let branchts = type_info_expr branch in
              patternsts @ branchts)
        in
        ots :: List.concat clausets
    | Builtin (_, il) -> List.map il ~f:calc_ident_locs
    | TFun (i, e) -> calc_ident_locs i :: type_info_expr e

  let rec type_info_stmts stmts =
    List.fold_right stmts ~init:[] ~f:(fun (stmt, _srep) acc ->
        ( match stmt with
        | Load (x, f) | Store (f, x) -> [ calc_ident_locs x; calc_ident_locs f ]
        | Bind (x, e) -> calc_ident_locs x :: type_info_expr e
        (* m[k1][k2][..] := v OR delete m[k1][k2][...] *)
        | MapUpdate (m, il, vopt) -> (
            [ calc_ident_locs m ]
            @ List.map il ~f:calc_ident_locs
            @ match vopt with Some v -> [ calc_ident_locs v ] | None -> [] )
        (* v <- m[k1][k2][...] OR b <- exists m[k1][k2][...] *)
        | MapGet (x, m, il, _) ->
            [ calc_ident_locs x; calc_ident_locs m ]
            @ List.map il ~f:calc_ident_locs
        | MatchStmt (o, clauses) ->
            let ots = calc_ident_locs o in
            let clausets =
              List.map clauses ~f:(fun (p, branch) ->
                  let patternvars = get_pattern_bounds p in
                  let patternsts = List.map patternvars ~f:calc_ident_locs in
                  let branchts = type_info_stmts branch in
                  patternsts @ branchts)
            in
            ots :: List.concat clausets
        | ReadFromBC (v, _) | SendMsgs v | CreateEvnt v -> [ calc_ident_locs v ]
        | AcceptPayment -> []
        | CallProc (_, il) -> List.map il ~f:calc_ident_locs
        | Iterate (l, _) -> [ calc_ident_locs l ]
        | Throw iopt -> (
            match iopt with Some i -> [ calc_ident_locs i ] | None -> [] ) )
        @ acc)

  let type_info_libentries lentries =
    List.fold_right lentries ~init:[] ~f:(fun lentry acc ->
        ( match lentry with
        | LibVar (i, _, e) -> calc_ident_locs i :: type_info_expr e
        | LibTyp (i, cdl) ->
            calc_ident_locs i
            :: List.map cdl ~f:(fun cd -> calc_ident_locs cd.cname) )
        @ acc)

  let type_info_library l = type_info_libentries l.lentries

  (* Given a library module, return a list of variables, their locations and types *)
  let type_info_lmod (lmod : lmodule) = type_info_library lmod.libs

  (* Given a contract, return a list of variables, their locations and types *)
  let type_info_cmod (cmod : cmodule) =
    (* If there's an internal library, provide info for it. *)
    (match cmod.libs with Some l -> type_info_library l | None -> [])
    (* Contract parameters *)
    @ List.map cmod.contr.cparams ~f:(fun (i, _) -> calc_ident_locs i)
    @ type_info_expr cmod.contr.cconstraint
    (* Contract fields *)
    @ ( List.concat
      @@ List.map cmod.contr.cfields ~f:(fun (i, _, e) ->
             calc_ident_locs i :: type_info_expr e) )
    (* Components *)
    @ List.concat
    @@ List.map cmod.contr.ccomps ~f:(fun comp ->
           List.map comp.comp_params ~f:(fun (i, _) -> calc_ident_locs i)
           @ type_info_stmts comp.comp_body)
end
