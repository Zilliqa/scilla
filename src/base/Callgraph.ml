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
open Literal
open Syntax

(** Represents a callgraph that consists of procedures/transitions and pure
    library functions. The callgraph doesn't include imported library
    functions. *)
module ScillaCallgraph (SR : Rep) (ER : Rep) = struct
  module CGLiteral = GlobalLiteral
  module CGType = CGLiteral.LType
  module CGIdentifier = CGType.TIdentifier
  module CGSyntax = ScillaSyntax (SR) (ER) (CGLiteral)
  open CGSyntax

  module Edge = struct
    type edge_ty =
      | Call  (** [dst] is called in [src] body *)
      | Alias  (** [dst] is a different name for [src] *)
      | TFunApp  (** [dst] is a an application of type function [src] *)

    type 'node t = { ty : edge_ty; src_node : 'node; dst_node : 'node }

    let mk ty src_node dst_node = { ty; src_node; dst_node }
    let src edge = edge.src_node
    let dst edge = edge.dst_node
  end

  module Node = struct
    type node_ty =
      | Trans
      | Proc
      | Fun  (** Pure library function *)
      | FunAlias  (** A different name for a function *)
      | TFun  (** Type function *)
      | TFunAlias  (** A different name for a type function *)

    type t = {
      id : SR.rep CGIdentifier.t;
      ty : node_ty;
      mutable out_edges : t Edge.t list;  (** Outcoming edges *)
      mutable in_edges : t Edge.t list;  (** Incoming edges *)
    }

    let mk id ty = { id; ty; out_edges = []; in_edges = [] }

    let succs node =
      List.fold_left node.out_edges ~init:[] ~f:(fun acc e ->
          acc @ [ Edge.dst e ])

    let preds node =
      List.fold_left node.in_edges ~init:[] ~f:(fun acc e ->
          acc @ [ Edge.src e ])

    let to_string node =
      CGIdentifier.Name.as_string (CGIdentifier.get_id node.id)

    let compare n1 n2 = CGIdentifier.compare n1.id n2.id
  end

  type cg = { nodes : Node.t list; edges : Node.t Edge.t list }

  let find_node (nodes : Node.t list) name =
    List.find nodes ~f:(fun node -> CGIdentifier.equal node.id name)

  (** Collects names of functions/procedures/transitions defined in the
      contract.
      @return List of created nodes. *)
  let collect_nodes (cmod : cmodule) =
    let collect_lib_functions (lib : library) =
      let rec aux init lib =
        List.fold_left lib.lentries ~init ~f:(fun acc lentry ->
            let mk_node name ty =
              let id : SR.rep CGIdentifier.t =
                CGIdentifier.mk_id (CGIdentifier.get_id name) SR.dummy_rep
              in
              Node.mk id ty
            in
            let collect_functions_in_expr name = function
              | Fun _ | TApp _ -> [ mk_node name Node.Fun ]
              | TFun _ -> [ mk_node name Node.TFun ]
              | Literal _ | Let _ | Message _ | App _ | Constr _ | MatchExpr _
              | Builtin _ | Var _ | Fixpoint _ | GasExpr _ ->
                  []
            in
            let collect_function_aliases (acc : Node.t list) name = function
              | Var id -> (
                  let f (n : Node.t) =
                    SIdentifier.Name.equal (SIdentifier.get_id n.id)
                      (SIdentifier.get_id id)
                  in
                  match List.find acc ~f with
                  | Some n ->
                      let new_ty =
                        match n.ty with
                        | TFun | TFunAlias -> Node.TFunAlias
                        | Trans | Proc | Fun | FunAlias -> Node.FunAlias
                      in
                      [ mk_node name new_ty ]
                  | None -> [])
              | Fun _ | Let _ | Literal _ | Message _ | App _ | Constr _
              | MatchExpr _ | Builtin _ | TFun _ | TApp _ | Fixpoint _
              | GasExpr _ ->
                  []
            in
            match lentry with
            (* NOTE: LibVar's type will be set only if there is a user-defined
                     annotation. `_ty` won't be inferred here. *)
            | LibVar (name, _ty, (e, _annot)) ->
                collect_function_aliases acc name e
                @ collect_functions_in_expr name e
                @ acc
                |> List.dedup_and_sort ~compare:Node.compare
            | LibTyp _ -> acc)
        |> fun res ->
        if phys_equal 0 @@ Caml.List.compare_lengths res init then res
        else aux res lib
      in
      aux [] lib
    in
    let collect_comps cmod =
      List.fold_left ~init:[] cmod.contr.ccomps ~f:(fun acc comp ->
          let ty =
            match comp.comp_type with
            | CompProc -> Node.Proc
            | CompTrans -> Node.Trans
          in
          acc @ [ Node.mk comp.comp_name ty ])
    in
    Option.value_map cmod.libs ~default:[] ~f:(fun lib ->
        collect_lib_functions lib)
    |> List.append @@ collect_comps cmod

  (** Collects identifiers of functions/procedures called in the given
      expression. *)
  let rec collect_funcalls (e, _annot) collected_nodes =
    match e with
    | Let (_id, _ty, body, in_) ->
        collect_funcalls body collected_nodes
        @ collect_funcalls in_ collected_nodes
    | App (f, _) | TApp (f, _) | Var f ->
        find_node collected_nodes f
        |> Option.value_map ~default:[] ~f:(fun n -> [ n ])
    | MatchExpr (_id, arms) ->
        List.fold_left arms ~init:[] ~f:(fun acc (_p, ea) ->
            acc @ collect_funcalls ea collected_nodes)
    | Fun (_, _, ea) | TFun (_, ea) | Fixpoint (_, _, ea) | GasExpr (_, ea) ->
        collect_funcalls ea collected_nodes
    | Literal _ | Message _ | Constr _ | Builtin _ -> []

  (** Returns a list of nodes that were called inside body of the component. *)
  let get_called_nodes comp (collected_nodes : Node.t list) =
    List.fold_left comp.comp_body ~init:[] ~f:(fun acc (s, _annot) ->
        match s with
        | Bind (_id, ea) ->
            collect_funcalls ea collected_nodes |> List.append acc
        | CallProc (id, _args) ->
            find_node collected_nodes id
            |> Option.value_map ~default:[] ~f:(fun n -> [ n ])
            |> List.append acc
        | Load _ | RemoteLoad _ | Store _ | MapUpdate _ | MapGet _
        | RemoteMapGet _ | MatchStmt _ | ReadFromBC _ | TypeCast _
        | AcceptPayment | Iterate _ | SendMsgs _ | CreateEvnt _ | Throw _
        | GasStmt _ ->
            acc)

  (** Creates edges between nodes defined in the contract.
      @return List of updated nodes and list of created edges. *)
  let fill_edges (cmod : cmodule) (collected_nodes : Node.t list) =
    let mk_edge (src : Node.t) (dst : Node.t) =
      let open Node in
      let edge_ty =
        match (src.ty, dst.ty) with
        | Fun, TFun -> Edge.TFunApp
        | TFunAlias, TFun | TFun, TFunAlias -> Edge.Alias
        | FunAlias, Fun | Fun, FunAlias -> Edge.Alias
        | _ -> Edge.Call
      in
      Edge.mk edge_ty src dst
    in
    let fill_components cmod =
      List.fold_left cmod.contr.ccomps ~init:(collected_nodes, [])
        ~f:(fun (nodes, edges) comp ->
          find_node nodes comp.comp_name
          |> Option.value_map ~default:(collected_nodes, []) ~f:(fun n ->
                 get_called_nodes comp collected_nodes
                 |> List.fold_left ~init:edges
                      ~f:(fun acc_edges (called_node : Node.t) ->
                        let out_edge = mk_edge n called_node in
                        n.out_edges <- n.out_edges @ [ out_edge ];
                        acc_edges @ [ out_edge ])
                 |> fun edges -> (nodes, edges)))
    in
    let fill_library (cmod : cmodule) =
      Option.value_map ~default:([], []) cmod.libs ~f:(fun lib ->
          List.fold_left lib.lentries ~init:([], [])
            ~f:(fun (nodes, edges) lentry ->
              match lentry with
              | LibVar (id, _ty, ea) ->
                  find_node collected_nodes id
                  |> Option.value_map ~default:(nodes, edges)
                       ~f:(fun (n : Node.t) ->
                         collect_funcalls ea collected_nodes
                         |> List.fold_left ~init:edges
                              ~f:(fun acc_edges (called_node : Node.t) ->
                                let out_edge = mk_edge n called_node in
                                n.out_edges <- n.out_edges @ [ out_edge ];
                                acc_edges @ [ out_edge ])
                         |> fun edges -> (nodes, edges))
              | LibTyp _ -> (nodes, edges)))
    in
    let comp_nodes, comp_edges = fill_components cmod
    and lib_nodes, lib_edges = fill_library cmod in
    (comp_nodes @ lib_nodes, comp_edges @ lib_edges)

  let mk (cmod : cmodule) =
    collect_nodes cmod |> fill_edges cmod |> fun (nodes, edges) ->
    { nodes; edges }

  (** Returns names of functions called insisde the body of the caller. *)
  let get_callees (cg : cg) callee =
    List.find cg.nodes ~f:(fun (n : Node.t) -> CGIdentifier.equal n.id callee)
    |> Option.value_map ~default:[] ~f:(fun n -> Node.succs n)

  module GDot = Graph.Graphviz.Dot (struct
    type t = cg

    module V = struct
      type t = Node.t
    end

    module E = struct
      type t = V.t Edge.t

      let src e = Edge.src e
      let dst e = Edge.dst e
    end

    let iter_vertex f g = List.iter g.nodes ~f
    let iter_edges_e f g = List.iter g.edges ~f
    let graph_attributes _ = []
    let default_vertex_attributes _ = []

    let vertex_name v =
      (* Dots used in QualifiedNames are forbidden in the .dot format. *)
      Node.to_string v |> String.split ~on:'.' |> fun l ->
      if List.is_empty l then "" else List.nth_exn l (List.length l - 1)

    let vertex_attributes (v : V.t) =
      match v.ty with
      | Node.Trans -> [ `Shape `Box; `Style `Filled; `Fillcolor 0xd3869b ]
      | Node.Proc -> [ `Shape `Box; `Style `Filled; `Fillcolor 0x83a598 ]
      | Node.Fun -> [ `Shape `Box; `Style `Filled; `Fillcolor 0x8ec07c ]
      | Node.FunAlias -> [ `Shape `Box; `Style `Filled; `Fillcolor 0x689d6a ]
      | Node.TFun -> [ `Shape `Box; `Style `Filled; `Fillcolor 0x98971a ]
      | Node.TFunAlias -> [ `Shape `Box; `Style `Filled; `Fillcolor 0x79740e ]

    let get_subgraph _ = None
    let default_edge_attributes _ = []

    let edge_attributes (e : E.t) =
      match e.ty with
      | Edge.Call -> [ `Style `Solid ]
      | Edge.Alias -> [ `Style `Dotted ]
      | Edge.TFunApp -> [ `Style `Dotted; `Arrowhead `Dot; `Color 0x7c6f64 ]
  end)

  (** Dumps callgraph in the .dot format to the output channel [out]. *)
  let dump_callgraph out cg = GDot.output_graph out cg
end
