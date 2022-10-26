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

open Core
open Sexplib.Std
open Scilla_base
open ErrorUtils
open Literal
open GasCharge

(** Annotated Scilla syntax extended with comment annotations. *)
module ExtendedScillaSyntax
    (SR : Syntax.Rep)
    (ER : Syntax.Rep)
    (Lit : ScillaLiteral) =
struct
  module SLiteral = Lit
  module SType = SLiteral.LType
  module SIdentifier = SType.TIdentifier
  module SGasCharge = ScillaGasCharge (SIdentifier.Name)

  type comment_text = string [@@deriving sexp]
  type comment_pos = ComLeft | ComAbove | ComRight [@@deriving sexp]
  type comment = loc * comment_text * comment_pos [@@deriving sexp]
  type annot_comment = comment list [@@deriving sexp]

  type 'a id_ann = 'a SIdentifier.t * annot_comment [@@deriving sexp]
  (** Annotated identifier that may be commented. *)

  (*******************************************************)
  (*                   Expressions                       *)
  (*******************************************************)

  type payload = MLit of SLiteral.t | MVar of ER.rep id_ann [@@deriving sexp]

  type pattern =
    | Wildcard
    | Binder of ER.rep id_ann
    | Constructor of SR.rep id_ann * pattern list
  [@@deriving sexp]

  type expr_annot = expr * ER.rep * annot_comment

  and expr =
    | Literal of SLiteral.t
    | Var of ER.rep id_ann
    | Let of ER.rep id_ann * SType.t option * expr_annot * expr_annot
    | Message of (string * payload) list
    | Fun of ER.rep id_ann * SType.t * expr_annot
    | App of ER.rep id_ann * ER.rep id_ann list
    | Constr of SR.rep id_ann * SType.t list * ER.rep id_ann list
    | MatchExpr of
        ER.rep id_ann * (pattern * expr_annot * comment_text list) list
    | Builtin of ER.rep Syntax.builtin_annot * SType.t list * ER.rep id_ann list
    | TFun of ER.rep id_ann * expr_annot
    | TApp of ER.rep id_ann * SType.t list
    | Fixpoint of ER.rep id_ann * SType.t * expr_annot
    | GasExpr of SGasCharge.gas_charge * expr_annot
  [@@deriving sexp]

  (*******************************************************)
  (*                   Statements                        *)
  (*******************************************************)

  type bcinfo_query =
    | CurBlockNum
    | ChainID
    | Timestamp of ER.rep id_ann
    | ReplicateContr of (ER.rep id_ann * ER.rep id_ann)
  [@@deriving sexp]

  type stmt_annot = stmt * SR.rep * annot_comment

  and stmt =
    | Load of ER.rep id_ann * ER.rep id_ann
    | RemoteLoad of ER.rep id_ann * ER.rep id_ann * ER.rep id_ann
    | Store of ER.rep id_ann * ER.rep id_ann
    | Bind of ER.rep id_ann * expr_annot
    | MapUpdate of ER.rep id_ann * ER.rep id_ann list * ER.rep id_ann option
    | MapGet of ER.rep id_ann * ER.rep id_ann * ER.rep id_ann list * bool
    | RemoteMapGet of
        ER.rep id_ann
        * ER.rep id_ann
        * ER.rep id_ann
        * ER.rep id_ann list
        * bool
    | MatchStmt of
        ER.rep id_ann * (pattern * stmt_annot list * comment_text list) list
    | ReadFromBC of ER.rep id_ann * bcinfo_query
    | TypeCast of ER.rep id_ann * ER.rep id_ann * SType.t
    | AcceptPayment
    | Return of ER.rep id_ann
    | Iterate of ER.rep id_ann * SR.rep id_ann
    | SendMsgs of ER.rep id_ann
    | CreateEvnt of ER.rep id_ann
    | CallProc of SR.rep id_ann * ER.rep id_ann list
    | Throw of ER.rep id_ann option
    | GasStmt of SGasCharge.gas_charge
  [@@deriving sexp]

  (*******************************************************)
  (*                    Contracts                        *)
  (*******************************************************)

  type component = {
    comp_comments : string list;
    comp_type : Syntax.component_type;
    comp_name : SR.rep id_ann;
    comp_params : (ER.rep id_ann * SType.t) list;
    comp_body : stmt_annot list;
  }
  [@@deriving sexp]

  type ctr_def = {
    cname : ER.rep id_ann;
    c_comments : comment_text list;
    c_arg_types : SType.t list;
  }
  [@@deriving sexp]

  type lib_entry =
    | LibVar of comment_text list * ER.rep id_ann * SType.t option * expr_annot
    | LibTyp of comment_text list * ER.rep id_ann * ctr_def list
  [@@deriving sexp]

  type library = { lname : SR.rep id_ann; lentries : lib_entry list }
  [@@deriving sexp]

  type contract = {
    cname : SR.rep id_ann;
    cparams : (ER.rep id_ann * SType.t) list;
    cconstraint : expr_annot;
    cfields : (comment_text list * ER.rep id_ann * SType.t * expr_annot) list;
    ccomps : component list;
  }
  [@@deriving sexp]

  type cmodule = {
    smver : int;
    file_comments : comment_text list;
    lib_comments : comment_text list;
    libs : library option;
    elibs : (SR.rep id_ann * SR.rep id_ann option) list;
    contr_comments : comment_text list;
    contr : contract;
  }
  [@@deriving sexp]
  (** The structure of the extended [cmodule] is:
      scilla_version 0

      (* File comment *)

      import X

      (* Library comment *)
      library ExampleLib

      (* Contract comment *)
      contract ExampleContr()
    *)

  (* Library module *)
  type lmodule = {
    smver : int;
    (* Scilla major version of the library. *)
    (* List of imports / external libs with an optional namespace. *)
    elibs : (SR.rep id_ann * SR.rep id_ann option) list;
    libs : library; (* lib functions defined in the module *)
  }
  [@@deriving sexp]

  (* A tree of libraries linked to their dependents *)
  type libtree = {
    libn : library;
    (* The library this node represents *)
    deps : libtree list; (* List of dependent libraries *)
  }
end

module ExtendedScillaSyntaxTransformer
    (SR : Syntax.Rep)
    (ER : Syntax.Rep)
    (Lit : Literal.ScillaLiteral) =
struct
  module Syn = Syntax.ScillaSyntax (SR) (ER) (Lit)
  module ExtSyn = ExtendedScillaSyntax (SR) (ER) (Lit)
  module SType = Lit.LType
  module SIdentifier = SType.TIdentifier

  type t = { mutable comments : (loc * ExtSyn.comment_text) list }

  let mk comments =
    (* Comments are already sorted by line number and column, because the lexer
       works by this way. *)
    { comments }

  (****************************************************************************)
  (* Utility functions                                                        *)
  (****************************************************************************)

  let nth_comment tr = List.nth tr.comments
  let first_comment tr = List.hd tr.comments
  let second_comment tr = nth_comment tr 1

  (** Removes the top comment from the given transformer. *)
  let pop_comment tr =
    if not @@ List.is_empty tr.comments then
      tr.comments <- List.tl_exn tr.comments

  (** Returns true if [comment_loc] is located above the library definition:
      (* Library comment *)
      library Something
    *)
  let is_comment_above_lib (cmod : Syn.cmodule) comment_loc =
    match cmod.libs with
    | None -> false
    | Some lib ->
        let lib_loc = SR.get_loc (SIdentifier.get_rep lib.lname) in
        comment_loc.lnum < lib_loc.lnum

  (** Returns true if [comment_loc] is a file comment located above the
      contract definition :
         (* Contract comment *)
         contract Something()
    *)
  let is_comment_above_contract (cmod : Syn.cmodule) comment_loc =
    let contr_loc = SR.get_loc (SIdentifier.get_rep cmod.contr.cname) in
    contr_loc.lnum > comment_loc.lnum

  (****************************************************************************)
  (* Collect functions                                                        *)
  (*                                                                          *)
  (* Collect functions collect comments for the required positions and remove *)
  (* them from the Transformer's mutable state.                               *)
  (****************************************************************************)

  (** Collects comment annotations that must be placed between [loc_start] and
      [loc_end] and removes them from the [tr.comments] list. *)
  let collect_comments tr loc_start loc_end =
    let rec aux acc = function
      (* Placing comments left *)
      | (loc, s) :: xs
        when (*    (* com *) start end                                  *)
             phys_equal loc.lnum loc_start.lnum && loc.cnum < loc_start.cnum ->
          pop_comment tr;
          aux ((loc, s, ExtSyn.ComLeft) :: acc) xs
      (* Placing comments above *)
      | (loc, s) :: xs
        when (* (* com *)                   (* com *)
                  start end                   start
                                               end *)
             loc.lnum < loc_start.lnum
             || (* (* com *) start end         (* com *) start
                                                          end *)
             (loc.lnum <= loc_start.lnum && loc.cnum < loc_start.cnum) ->
          pop_comment tr;
          aux ((loc, s, ExtSyn.ComAbove) :: acc) xs
      (* Placing comments right *)
      | (loc, s) :: xs
        when (* start (* com *)
                 end *)
             phys_equal loc.lnum loc_start.lnum
             && loc.lnum > loc_end.lnum && loc.cnum > loc_start.cnum
             || (*    start (* com *) end                                  *)
             phys_equal loc.lnum loc_start.lnum
             && phys_equal loc.lnum loc_end.lnum
             && loc.cnum > loc_start.cnum && loc.cnum < loc_end.cnum ->
          pop_comment tr;
          aux ((loc, s, ExtSyn.ComRight) :: acc) xs
      | _ -> acc
    in
    if ErrorUtils.compare_loc loc_start loc_end > 0 then []
    else aux [] tr.comments

  (** A wrapper for [collect_comments] that returns only texts of comments,
      without location information. *)
  let collect_comment_texts tr loc_start loc_end =
    collect_comments tr loc_start loc_end
    |> List.map ~f:(fun (_, text, _) -> text)

  (** Collects a list of comments placed above [loc]. *)
  let collect_comments_above tr loc =
    let rec aux acc =
      match List.hd tr.comments with
      | Some (comment_loc, comment) when loc.lnum > comment_loc.lnum ->
          pop_comment tr;
          aux (acc @ [ comment ])
      | _ -> acc
    in
    aux []

  (** Collects a list of comments placed on the right of [loc]. *)
  let collect_comments_right tr loc =
    let rec aux acc =
      match List.hd tr.comments with
      | Some (comment_loc, comment)
        when phys_equal loc.lnum comment_loc.lnum && loc.cnum < comment_loc.cnum
        ->
          pop_comment tr;
          aux (acc @ [ comment ])
      | _ -> acc
    in
    aux []

  (* Collects file comments located on the top of the contract module. *)
  let collect_file_comments tr (cmod : Syn.cmodule) =
    (* Utility function that collects file comments before library or contract
       definition when we don't have imports. *)
    let get_file_comments f =
      let rec aux idx acc =
        let result all =
          (* [all] contains all the comments above the library/contract
             definition. We have to find a group of comments that are separated
              with at least a single newline with the comments above
              library/contract. If we have multiple comments above the
              library/contract comments, they will be considered as a part of
              the file comment:
               (* file comment1 *)
               (* file comment2 *)

               (* file comment3 *)

               (* library comment1 *)
               (* library comment2 *)
               library Something
          *)
          let rec first_comment_idx acc idx =
            match acc with
            | (c1_loc, _) :: (c2_loc, c2) :: cs ->
                if c1_loc.lnum > c2_loc.lnum + 1 then Some (idx + 1)
                else first_comment_idx ((c2_loc, c2) :: cs) (idx + 1)
            | _ -> None
          in
          let rev_all = List.rev all in
          match first_comment_idx rev_all 0 with
          | Some idx when (not @@ phys_equal idx 0) && idx < List.length acc ->
              List.sub rev_all ~pos:idx ~len:(List.length acc - idx)
              |> List.rev
              |> List.map ~f:(fun (_loc, c) ->
                     pop_comment tr;
                     c)
          | _ -> []
        in
        match nth_comment tr idx with
        | Some (comment1_loc, comment1) ->
            if not @@ f cmod comment1_loc then result acc
            else aux (idx + 1) (acc @ [ (comment1_loc, comment1) ])
        | _ -> result acc
      in
      aux 0 []
    in
    let has_imports = not @@ List.is_empty cmod.elibs in
    let has_library = Option.is_some cmod.libs in
    if has_imports then
      (* If we have imports then the file comment can be located only above them:
           scilla_version 0
           (* File comment *)
           import BoolUtils
      *)
      let first_import_loc =
        List.hd_exn cmod.elibs |> fun (id, _) ->
        SR.get_loc (SIdentifier.get_rep id)
      in
      collect_comments_above tr first_import_loc
    else if has_library then
      (* If we don't have import statements, the file comment may be located
         above the library comment. In this case, it must be separated with
         the library comment with at least one newline:
           scilla_version 0
           (* File comment *)

           (* Library comment *)
           library Something
      *)
      get_file_comments is_comment_above_lib
    else
      (* If we don't have import statements and library definition, the file
         comment may be located above the contract comment:
           scilla_version 0
           (* File comment *)

           (* Contract comment *)
           contract Something
      *)
      get_file_comments is_comment_above_contract

  (** Collects library comments that are located above the library definition. *)
  let collect_lib_comments tr (cmod : Syn.cmodule) =
    let has_library = Option.is_some cmod.libs in
    let rec aux acc =
      match first_comment tr with
      | Some (comment_loc, comment)
        when has_library && is_comment_above_lib cmod comment_loc ->
          pop_comment tr;
          aux (acc @ [ comment ])
      | _ -> acc
    in
    aux []

  (** Collects contract comment which is a comment located above the contract definition. *)
  let collect_contr_comments tr (cmod : Syn.cmodule) =
    let rec aux acc =
      match first_comment tr with
      | Some (comment_loc, comment)
        when is_comment_above_contract cmod comment_loc ->
          pop_comment tr;
          aux (acc @ [ comment ])
      | _ -> acc
    in
    aux []

  (****************************************************************************)
  (* Extend functions                                                         *)
  (*                                                                          *)
  (* Extend functions extend AST with annotations.                            *)
  (****************************************************************************)

  let extend_id ?(rep_end = None) tr id get_loc =
    let id_loc = get_loc (SIdentifier.get_rep id) in
    let end_loc =
      Option.value_map rep_end ~default:id_loc ~f:(fun rep -> get_loc rep)
    in
    let comments = collect_comments tr id_loc end_loc in
    (id, comments)

  let extend_er_id ?(rep_end = None) tr id = extend_id tr id ~rep_end ER.get_loc
  let extend_sr_id ?(rep_end = None) tr id = extend_id tr id ~rep_end SR.get_loc

  let extend_payload tr = function
    | Syn.MLit l -> ExtSyn.MLit l
    | Syn.MVar v -> ExtSyn.MVar (extend_er_id tr v)

  (** Extends the given pattern.
      [body_loc] is a start position of the body of this arm. *)
  let rec extend_pattern ?(body_loc = None) tr pat =
    let get_arm_comments pat_loc =
      Option.value_map body_loc
        ~f:(fun body_loc -> collect_comment_texts tr pat_loc body_loc)
        ~default:(collect_comments_right tr pat_loc)
    in
    match pat with
    | Syn.Wildcard ->
        let arm_comments = [] (* TODO: What is the start location here? *) in
        (ExtSyn.Wildcard, arm_comments)
    | Syn.Binder id ->
        let id' = extend_er_id tr id in
        let id_loc = ER.get_loc (SIdentifier.get_rep id) in
        let arm_comments = get_arm_comments id_loc in
        (ExtSyn.Binder id', arm_comments)
    | Syn.Constructor (id, args) ->
        let id' = extend_sr_id tr id in
        let id_loc = SR.get_loc (SIdentifier.get_rep id) in
        let args' =
          List.map args ~f:(fun arg ->
              extend_pattern tr arg |> fun (pat', _) -> pat')
        in
        let arm_comments = get_arm_comments id_loc in
        (ExtSyn.Constructor (id', args'), arm_comments)

  let rec extend_expr tr (e, ann) =
    let comment ?(rep_end = ann) () =
      collect_comments tr (ER.get_loc ann) (ER.get_loc rep_end)
    in
    match e with
    | Syn.Literal l -> (ExtSyn.Literal l, ann, comment ())
    | Syn.Var id ->
        let c = comment () in
        let id' = extend_er_id tr id in
        (ExtSyn.Var id', ann, c)
    | Syn.Let (id, ty, (lhs, lhs_rep), rhs) ->
        let c = comment () ~rep_end:(SIdentifier.get_rep id) in
        let id' = extend_er_id tr id ~rep_end:(Some lhs_rep) in
        let lhs' = extend_expr tr (lhs, lhs_rep) in
        let rhs' = extend_expr tr rhs in
        (ExtSyn.Let (id', ty, lhs', rhs'), ann, c)
    | Syn.Message msgs ->
        let c = comment () in
        let msgs' =
          List.map msgs ~f:(fun (s, pld) -> (s, extend_payload tr pld))
        in
        (ExtSyn.Message msgs', ann, c)
    | Syn.Fun (id, ty, (body, body_loc)) ->
        let c = comment ~rep_end:(SIdentifier.get_rep id) () in
        let id' = extend_er_id tr id ~rep_end:(Some body_loc) in
        let body' = extend_expr tr (body, body_loc) in
        (ExtSyn.Fun (id', ty, body'), ann, c)
    | Syn.App (id, args) ->
        let c = comment () in
        let id' = extend_er_id tr id in
        let args' = List.map args ~f:(fun arg -> extend_er_id tr arg) in
        (ExtSyn.App (id', args'), ann, c)
    | Syn.Constr (id, tys, args) ->
        let c = comment () in
        let id' = extend_sr_id tr id in
        let args' = List.map args ~f:(fun arg -> extend_er_id tr arg) in
        (ExtSyn.Constr (id', tys, args'), ann, c)
    | Syn.MatchExpr (id, arms) ->
        let c = comment () in
        let id' = extend_er_id tr id in
        let arms' =
          List.map arms ~f:(fun (pat, (body, body_rep)) ->
              let pat', arm_comments =
                let body_loc = Some (ER.get_loc body_rep) in
                extend_pattern ~body_loc tr pat
              in
              let body' = extend_expr tr (body, body_rep) in
              (pat', body', arm_comments))
        in
        (ExtSyn.MatchExpr (id', arms'), ann, c)
    | Syn.Builtin (builtin, ty, args) ->
        let c = comment () in
        let args' = List.map args ~f:(fun arg -> extend_er_id tr arg) in
        (ExtSyn.Builtin (builtin, ty, args'), ann, c)
    | Syn.TFun (id, (body, body_loc)) ->
        let c = comment ~rep_end:body_loc () in
        let id' = extend_er_id tr id in
        let body' = extend_expr tr (body, body_loc) in
        (ExtSyn.TFun (id', body'), ann, c)
    | Syn.TApp (id, tys) ->
        let c = comment () in
        let id' = extend_er_id tr id in
        (ExtSyn.TApp (id', tys), ann, c)
    | Syn.Fixpoint (id, ty, (body, body_loc)) ->
        let c = comment ~rep_end:body_loc () in
        let id' = extend_er_id tr id in
        let body' = extend_expr tr (body, body_loc) in
        (ExtSyn.Fixpoint (id', ty, body'), ann, c)
    | Syn.GasExpr (gc, (body, body_loc)) ->
        let c = comment ~rep_end:body_loc () in
        let body' = extend_expr tr (body, body_loc) in
        let gc' =
          Syn.SGasCharge.sexp_of_gas_charge gc
          |> ExtSyn.SGasCharge.gas_charge_of_sexp
        in
        (ExtSyn.GasExpr (gc', body'), ann, c)

  let extend_bcinfo_query tr = function
    | Syn.CurBlockNum -> ExtSyn.CurBlockNum
    | Syn.ChainID -> ExtSyn.ChainID
    | Syn.Timestamp id -> ExtSyn.Timestamp (extend_er_id tr id)
    | Syn.ReplicateContr (addr, param) ->
        let addr' = extend_er_id tr addr in
        let param' = extend_er_id tr param in
        ExtSyn.ReplicateContr (addr', param')

  let rec extend_stmt tr (s, ann) =
    let comment loc_end = collect_comments tr (SR.get_loc ann) loc_end in
    let loc_end_er id = SIdentifier.get_rep id |> ER.get_loc in
    let loc_end_sr id = SIdentifier.get_rep id |> SR.get_loc in
    match s with
    | Syn.Load (lhs, rhs) ->
        let c = comment (loc_end_er lhs) in
        let lhs' = extend_er_id tr lhs in
        let rhs' = extend_er_id tr rhs in
        (ExtSyn.Load (lhs', rhs'), ann, c)
    | Syn.RemoteLoad (lhs, addr, rhs) ->
        let c = comment (loc_end_er lhs) in
        let lhs' =
          extend_er_id tr lhs ~rep_end:(Some (SIdentifier.get_rep addr))
        in
        let addr' =
          extend_er_id tr addr ~rep_end:(Some (SIdentifier.get_rep rhs))
        in
        let rhs' = extend_er_id tr rhs in
        (ExtSyn.RemoteLoad (lhs', addr', rhs'), ann, c)
    | Syn.Store (lhs, rhs) ->
        let c = comment (loc_end_er lhs) in
        let lhs' = extend_er_id tr lhs in
        let rhs' = extend_er_id tr rhs in
        (ExtSyn.Store (lhs', rhs'), ann, c)
    | Syn.Bind (id, body) ->
        let c = comment (loc_end_er id) in
        let id' = extend_er_id tr id in
        let ea' = extend_expr tr body in
        (ExtSyn.Bind (id', ea'), ann, c)
    | Syn.MapUpdate (m, keys, v) ->
        let c = comment (loc_end_er m) in
        let m' = extend_er_id tr m in
        let keys' = List.map keys ~f:(fun k -> extend_er_id tr k) in
        let v' =
          Option.value_map v ~default:None ~f:(fun v ->
              Some (extend_er_id tr v))
        in
        (ExtSyn.MapUpdate (m', keys', v'), ann, c)
    | Syn.MapGet (v, m, keys, retrieve) ->
        let c = comment (loc_end_er v) in
        let v' = extend_er_id tr v in
        let m' = extend_er_id tr m in
        let keys' = List.map keys ~f:(fun k -> extend_er_id tr k) in
        (ExtSyn.MapGet (v', m', keys', retrieve), ann, c)
    | Syn.RemoteMapGet (v, addr, m, keys, retrieve) ->
        let c = comment (loc_end_er v) in
        let v' = extend_er_id tr v in
        let addr' = extend_er_id tr addr in
        let m' = extend_er_id tr m in
        let keys' = List.map keys ~f:(fun k -> extend_er_id tr k) in
        (ExtSyn.RemoteMapGet (v', addr', m', keys', retrieve), ann, c)
    | Syn.MatchStmt (id, arms) ->
        let c = comment (loc_end_er id) in
        let id' = extend_er_id tr id in
        let arms' =
          List.map arms ~f:(fun (pat, stmts) ->
              let body_loc =
                List.hd stmts
                |> Option.value_map
                     ~f:(fun (_, rep) -> Some (SR.get_loc rep))
                     ~default:None
              in
              let pat', arm_comments = extend_pattern ~body_loc tr pat in
              let stmts' =
                List.map stmts ~f:(fun stmt -> extend_stmt tr stmt)
              in
              (pat', stmts', arm_comments))
        in
        (ExtSyn.MatchStmt (id', arms'), ann, c)
    | Syn.ReadFromBC (id, q) ->
        let c = comment (loc_end_er id) in
        let id' = extend_er_id tr id in
        let q' = extend_bcinfo_query tr q in
        (ExtSyn.ReadFromBC (id', q'), ann, c)
    | Syn.TypeCast (id, addr, ty) ->
        let c = comment (loc_end_er id) in
        let id' = extend_er_id tr id in
        let addr' = extend_er_id tr addr in
        (ExtSyn.TypeCast (id', addr', ty), ann, c)
    | Syn.AcceptPayment ->
        let c = comment (SR.get_loc ann) in
        (ExtSyn.AcceptPayment, ann, c)
    | Syn.Return id ->
        let c = comment (loc_end_er id) in
        let id' = extend_er_id tr id in
        (ExtSyn.Return id', ann, c)
    | Syn.Iterate (l, f) ->
        let c = comment (loc_end_er l) in
        let l' = extend_er_id tr l in
        let f' = extend_sr_id tr f in
        (ExtSyn.Iterate (l', f'), ann, c)
    | Syn.SendMsgs id ->
        let c = comment (loc_end_er id) in
        let id' = extend_er_id tr id in
        (ExtSyn.SendMsgs id', ann, c)
    | Syn.CreateEvnt id ->
        let c = comment (loc_end_er id) in
        let id' = extend_er_id tr id in
        (ExtSyn.CreateEvnt id', ann, c)
    | Syn.CallProc (id, args) ->
        let c = comment (loc_end_sr id) in
        let id' = extend_sr_id tr id in
        let args' = List.map args ~f:(fun arg -> extend_er_id tr arg) in
        (ExtSyn.CallProc (id', args'), ann, c)
    | Syn.Throw id_opt -> (
        match id_opt with
        | Some id ->
            let c = comment (loc_end_er id) in
            let id' = extend_er_id tr id in
            (ExtSyn.Throw (Some id'), ann, c)
        | None ->
            let c = comment (SR.get_loc ann) in
            (ExtSyn.Throw None, ann, c))
    | Syn.GasStmt gc ->
        let c = comment (SR.get_loc ann) in
        let gc' =
          Syn.SGasCharge.sexp_of_gas_charge gc
          |> ExtSyn.SGasCharge.gas_charge_of_sexp
        in
        (ExtSyn.GasStmt gc', ann, c)

  let extend_ctr_def tr (ctr : Syn.ctr_def) =
    let cname_loc = ER.get_loc (SIdentifier.get_rep ctr.cname) in
    let c_comments = collect_comments_right tr cname_loc in
    let cname' = extend_er_id tr ctr.cname in
    { ExtSyn.cname = cname'; c_comments; c_arg_types = ctr.c_arg_types }

  let extend_lentry tr = function
    | Syn.LibVar (id, ty_opt, ea) ->
        let id_loc = ER.get_loc (SIdentifier.get_rep id) in
        let comments =
          collect_comments_above tr id_loc @ collect_comments_right tr id_loc
        in
        let id' = extend_er_id tr id in
        let ea' = extend_expr tr ea in
        ExtSyn.LibVar (comments, id', ty_opt, ea')
    | Syn.LibTyp (id, ctrs) ->
        let id_loc = ER.get_loc (SIdentifier.get_rep id) in
        let comments =
          collect_comments_above tr id_loc @ collect_comments_right tr id_loc
        in
        let id' = extend_er_id tr id in
        let ctrs' = List.map ctrs ~f:(fun ctr -> extend_ctr_def tr ctr) in
        ExtSyn.LibTyp (comments, id', ctrs')

  let extend_lib tr (lib : Syn.library) =
    let lname' = extend_sr_id tr lib.lname in
    let lentries' =
      List.map lib.lentries ~f:(fun lentry -> extend_lentry tr lentry)
    in
    { ExtSyn.lname = lname'; lentries = lentries' }

  let extend_elib tr elib =
    let import, import_as = elib in
    let import' = extend_sr_id tr import in
    let import_as' =
      Option.value_map import_as ~default:None ~f:(fun id ->
          Some (extend_sr_id tr id))
    in
    (import', import_as')

  let extend_component tr comp =
    let comp_comments =
      let comp_name_loc = SR.get_loc (SIdentifier.get_rep comp.Syn.comp_name) in
      collect_comments_above tr comp_name_loc
    in
    let comp_type = comp.Syn.comp_type in
    let comp_name = extend_sr_id tr comp.comp_name in
    let comp_params =
      List.map comp.comp_params ~f:(fun (id, ty) -> (extend_er_id tr id, ty))
    in
    let comp_body =
      List.map comp.comp_body ~f:(fun stmt -> extend_stmt tr stmt)
    in
    { comp_comments; ExtSyn.comp_type; comp_name; comp_params; comp_body }

  let extend_contract tr (contr : Syn.contract) : ExtSyn.contract =
    let cname = extend_sr_id tr contr.cname in
    let cparams =
      List.map contr.cparams ~f:(fun (id, ty) -> (extend_er_id tr id, ty))
    in
    let cconstraint = extend_expr tr contr.cconstraint in
    let cfields =
      List.map contr.cfields ~f:(fun (id, ty, init) ->
          let id_loc = ER.get_loc (SIdentifier.get_rep id) in
          let comments = collect_comments_above tr id_loc in
          let id' = extend_er_id tr id in
          let init' = extend_expr tr init in
          (comments, id', ty, init'))
    in
    let ccomps = List.map contr.ccomps ~f:(fun c -> extend_component tr c) in
    { cname; cparams; cconstraint; cfields; ccomps }

  let extend_cmodule tr (cmod : Syn.cmodule) : ExtSyn.cmodule =
    let smver = cmod.smver in
    let file_comments = collect_file_comments tr cmod in
    let elibs = List.map cmod.elibs ~f:(fun l -> extend_elib tr l) in
    let lib_comments = collect_lib_comments tr cmod in
    let libs =
      Option.value_map cmod.libs ~default:None ~f:(fun l ->
          Some (extend_lib tr l))
    in
    let contr_comments = collect_contr_comments tr cmod in
    let contr = extend_contract tr cmod.contr in
    { smver; file_comments; lib_comments; libs; elibs; contr_comments; contr }
end

module LocalLiteralTransformer =
  ExtendedScillaSyntaxTransformer (ParserUtil.ParserRep) (ParserUtil.ParserRep)
    (Literal.LocalLiteral)

module GlobalLiteralTransformer =
  ExtendedScillaSyntaxTransformer (ParserUtil.ParserRep) (ParserUtil.ParserRep)
    (Literal.GlobalLiteral)
