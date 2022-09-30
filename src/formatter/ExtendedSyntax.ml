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

(** Annotated Scilla syntax extended with comment nodes. *)
module ExtendedScillaSyntax
    (SR : Syntax.Rep)
    (ER : Syntax.Rep)
    (Lit : ScillaLiteral) =
struct
  module SLiteral = Lit
  module SType = SLiteral.LType
  module SIdentifier = SType.TIdentifier
  module SGasCharge = ScillaGasCharge (SIdentifier.Name)

  type comment_pos = ComLeft | ComAbove | ComRight [@@deriving sexp]
  type comment = loc * string * comment_pos [@@deriving sexp]
  type annot_comment = comment list [@@deriving sexp]

  type 'a id_ann = 'a SIdentifier.t * annot_comment [@@deriving sexp]
  (** Annotated identifier that may be commented *)

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
    | MatchExpr of ER.rep id_ann * (pattern * expr_annot) list
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
    | MatchStmt of ER.rep id_ann * (pattern * stmt_annot list) list
    | ReadFromBC of ER.rep id_ann * bcinfo_query
    | TypeCast of ER.rep id_ann * ER.rep id_ann * SType.t
    | AcceptPayment  (** [AcceptPayment] is an [accept] statement. *)
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
    comp_type : Syntax.component_type;
    comp_name : SR.rep id_ann;
    comp_params : (ER.rep id_ann * SType.t) list;
    comp_body : stmt_annot list;
  }
  [@@deriving sexp]

  type ctr_def = { cname : ER.rep id_ann; c_arg_types : SType.t list }
  [@@deriving sexp]

  type lib_entry =
    | LibVar of ER.rep id_ann * SType.t option * expr_annot
    | LibTyp of ER.rep id_ann * ctr_def list
  [@@deriving sexp]

  type library = { lname : SR.rep id_ann; lentries : lib_entry list }
  [@@deriving sexp]

  type contract = {
    cname : SR.rep id_ann;
    cparams : (ER.rep id_ann * SType.t) list;
    cconstraint : expr_annot;
    cfields : (ER.rep id_ann * SType.t * expr_annot) list;
    ccomps : component list;
  }
  [@@deriving sexp]

  type cmodule = {
    smver : int;
    file_comments : string list;
    lib_comments : string list;
    libs : library option;
    elibs : (SR.rep id_ann * SR.rep id_ann option) list;
    contr_comments : string list;
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

  type t = { mutable comments : (loc * string) list }

  let mk comments =
    (* Comments are already sorted by line number and column, because the lexer
       works by this way. *)
    { comments }

  (** Creates comment annotations that must be placed between [loc_start] and
      [loc_end] and removes them from the [tr.comments] list. *)
  let place_comments tr loc_start loc_end =
    let rec aux acc = function
      (* Placing comments left *)
      | (loc, s) :: xs
        when (*    (* com *) start end                                  *)
             phys_equal loc.lnum loc_start.lnum && loc.cnum < loc_start.cnum ->
          tr.comments <- xs;
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
          tr.comments <- xs;
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
          tr.comments <- xs;
          aux ((loc, s, ExtSyn.ComRight) :: acc) xs
      | _ -> acc
    in
    if ErrorUtils.compare_loc loc_start loc_end > 0 then []
    else aux [] tr.comments

  let extend_id ?(rep_end = None) tr id get_loc =
    let id_loc = get_loc (SIdentifier.get_rep id) in
    let end_loc =
      Option.value_map rep_end ~default:id_loc ~f:(fun rep -> get_loc rep)
    in
    let comments = place_comments tr id_loc end_loc in
    (id, comments)

  let extend_er_id ?(rep_end = None) tr id = extend_id tr id ~rep_end ER.get_loc
  let extend_sr_id ?(rep_end = None) tr id = extend_id tr id ~rep_end SR.get_loc

  let extend_payload tr = function
    | Syn.MLit l -> ExtSyn.MLit l
    | Syn.MVar v -> ExtSyn.MVar (extend_er_id tr v)

  let rec extend_pattern tr = function
    | Syn.Wildcard -> ExtSyn.Wildcard
    | Syn.Binder id -> ExtSyn.Binder (extend_er_id tr id)
    | Syn.Constructor (id, args) ->
        let args' = List.map args ~f:(fun arg -> extend_pattern tr arg) in
        ExtSyn.Constructor (extend_sr_id tr id, args')

  let rec extend_expr tr (e, ann) =
    let comment ?(rep_end = ann) () =
      place_comments tr (ER.get_loc ann) (ER.get_loc rep_end)
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
    | Syn.Fun (id, ty, (body, body_rep)) ->
        let c = comment ~rep_end:(SIdentifier.get_rep id) () in
        let id' = extend_er_id tr id ~rep_end:(Some body_rep) in
        let body' = extend_expr tr (body, body_rep) in
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
          List.map arms ~f:(fun (pat, body) ->
              (extend_pattern tr pat, extend_expr tr body))
        in
        (ExtSyn.MatchExpr (id', arms'), ann, c)
    | Syn.Builtin (builtin, ty, args) ->
        let c = comment () in
        let args' = List.map args ~f:(fun arg -> extend_er_id tr arg) in
        (ExtSyn.Builtin (builtin, ty, args'), ann, c)
    | Syn.TFun (id, (body, body_rep)) ->
        let c = comment ~rep_end:body_rep () in
        let id' = extend_er_id tr id in
        let body' = extend_expr tr (body, body_rep) in
        (ExtSyn.TFun (id', body'), ann, c)
    | Syn.TApp (id, tys) ->
        let c = comment () in
        let id' = extend_er_id tr id in
        (ExtSyn.TApp (id', tys), ann, c)
    | Syn.Fixpoint (id, ty, (body, body_rep)) ->
        let c = comment ~rep_end:body_rep () in
        let id' = extend_er_id tr id in
        let body' = extend_expr tr (body, body_rep) in
        (ExtSyn.Fixpoint (id', ty, body'), ann, c)
    | Syn.GasExpr (gc, (body, body_rep)) ->
        let c = comment ~rep_end:body_rep () in
        let body' = extend_expr tr (body, body_rep) in
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
    let comment loc_end = place_comments tr (SR.get_loc ann) loc_end in
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
              let pat' = extend_pattern tr pat in
              let stmts' =
                List.map stmts ~f:(fun stmt -> extend_stmt tr stmt)
              in
              (pat', stmts'))
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
    let cname' = extend_er_id tr ctr.cname in
    { ExtSyn.cname = cname'; c_arg_types = ctr.c_arg_types }

  let extend_lentry tr = function
    | Syn.LibVar (id, ty_opt, ea) ->
        let id' = extend_er_id tr id in
        let ea' = extend_expr tr ea in
        ExtSyn.LibVar (id', ty_opt, ea')
    | Syn.LibTyp (id, ctrs) ->
        let id' = extend_er_id tr id in
        let ctrs' = List.map ctrs ~f:(fun ctr -> extend_ctr_def tr ctr) in
        ExtSyn.LibTyp (id', ctrs')

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
    let comp_type = comp.Syn.comp_type in
    let comp_name = extend_sr_id tr comp.comp_name in
    let comp_params =
      List.map comp.comp_params ~f:(fun (id, ty) -> (extend_er_id tr id, ty))
    in
    let comp_body =
      List.map comp.comp_body ~f:(fun stmt -> extend_stmt tr stmt)
    in
    { ExtSyn.comp_type; comp_name; comp_params; comp_body }

  let extend_contract tr (contr : Syn.contract) : ExtSyn.contract =
    let cname = extend_sr_id tr contr.cname in
    let cparams =
      List.map contr.cparams ~f:(fun (id, ty) -> (extend_er_id tr id, ty))
    in
    let cconstraint = extend_expr tr contr.cconstraint in
    let cfields =
      List.map contr.cfields ~f:(fun (id, ty, init) ->
          (extend_er_id tr id, ty, extend_expr tr init))
    in
    let ccomps = List.map contr.ccomps ~f:(fun c -> extend_component tr c) in
    { cname; cparams; cconstraint; cfields; ccomps }

  (** Extracts top-level comments of the [cmod] based on their locations. *)
  let parse_toplevel_comments tr (cmod : Syn.cmodule) :
      string list * string list * string list =
    let get_first_comment () = List.hd tr.comments in
    let get_second_comment () = List.nth tr.comments 1 in
    let cut_comments () = tr.comments <- List.tl_exn tr.comments in
    let has_imports = not @@ List.is_empty cmod.elibs in
    let has_library = Option.is_some cmod.libs in
    (* Returns true if there are import statements and there is a comment
       before them. *)
    let is_comment_before_imports comment_loc =
      let first_import_loc =
        List.hd_exn cmod.elibs |> fun (id, _) ->
        SR.get_loc (SIdentifier.get_rep id)
      in
      if first_import_loc.lnum > comment_loc.lnum then true else false
    in
    (* Returns true if [comment_loc] is located before library definition. *)
    let is_comment_before_lib comment_loc =
      match cmod.libs with
      | None -> false
      | Some lib ->
          let lib_loc = SR.get_loc (SIdentifier.get_rep lib.lname) in
          comment_loc.lnum < lib_loc.lnum
    in
    (* Returns true if it is a file comment located before contract comment:
         (* Contract comment *)
         contract Something
    *)
    let is_comment_before_contract comment2_loc =
      let contr_loc = SR.get_loc (SIdentifier.get_rep cmod.contr.cname) in
      contr_loc.lnum < comment2_loc.lnum
    in
    (* File comments have be located on the top of the contract module. *)
    let file_comments =
      let rec aux acc =
        match (get_first_comment (), get_second_comment ()) with
        | Some (comment1_loc, comment1), Some (comment2_loc, _) ->
            if
              (has_imports && is_comment_before_imports comment1_loc)
              || (not has_imports) && has_library
                 && is_comment_before_lib comment2_loc
              || (not has_imports) && (not has_library)
                 && is_comment_before_contract comment2_loc
            then (
              cut_comments ();
              aux (acc @ [ comment1 ]))
            else acc
        | _ -> acc
      in
      aux []
    in
    (* Library comments are comments located above the library definition. *)
    let lib_comments =
      let rec aux acc =
        match get_first_comment () with
        | Some (comment_loc, comment)
          when has_library && is_comment_before_lib comment_loc ->
            cut_comments ();
            aux (acc @ [ comment ])
        | _ -> acc
      in
      aux []
    in
    (* Contract comment is a comment located above the contract definition. *)
    let contract_comments =
      let rec aux acc =
        match get_first_comment () with
        | Some (comment_loc, comment)
          when is_comment_before_contract comment_loc ->
            cut_comments ();
            aux (acc @ [ comment ])
        | _ -> acc
      in
      aux []
    in
    (file_comments, lib_comments, contract_comments)

  let extend_cmodule tr (cmod : Syn.cmodule) : ExtSyn.cmodule =
    let smver = cmod.smver in
    let file_comments, lib_comments, contr_comments =
      parse_toplevel_comments tr cmod
    in
    let elibs = List.map cmod.elibs ~f:(fun l -> extend_elib tr l) in
    let libs =
      Option.value_map cmod.libs ~default:None ~f:(fun l ->
          Some (extend_lib tr l))
    in
    let contr = extend_contract tr cmod.contr in
    { smver; file_comments; lib_comments; libs; elibs; contr_comments; contr }
end

module LocalLiteralTransformer =
  ExtendedScillaSyntaxTransformer (ParserUtil.ParserRep) (ParserUtil.ParserRep)
    (Literal.LocalLiteral)
