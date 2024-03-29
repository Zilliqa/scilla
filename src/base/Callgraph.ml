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

(** Represents a callgraph (CG) that consists of procedures/transitions and
    pure library functions. The callgraph doesn't include imported library
    functions.

    The generated CG is not complete in the sense that it cannot handle partial
    applications of the both functions and type functions. This is an
    intentional design decision, because the CG is used only for analyses, and
    we want to make it work fast. But the algorithms for building CG for
    functional programs have at least n^3 complexity. *)
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
    [@@deriving sexp]

    type 'node t = { ty : edge_ty; src_node : 'node; dst_node : 'node }
    [@@deriving sexp]

    let mk ty src_node dst_node = { ty; src_node; dst_node }
    let src edge = edge.src_node
    let dst edge = edge.dst_node
  end

  module Node = struct
    include CGIdentifier.Name
    include Comparable.Make (CGIdentifier.Name)

    type node_ty =
      | Trans
      | Proc
      | Fun  (** Pure library function *)
      | FunAlias  (** A different name for a function *)
      | TFun  (** Type function *)
      | TFunAlias  (** A different name for a type function *)
    [@@deriving sexp, compare]

    type t = {
      id : SR.rep CGIdentifier.t;
      ty : node_ty;
      mutable out_edges : t Edge.t list;  (** Outcoming edges *)
      mutable in_edges : t Edge.t list;  (** Incoming edges *)
    }
    [@@deriving sexp]

    let mk id ty = { id; ty; out_edges = []; in_edges = [] }
    let id n = n.id

    let succs node =
      List.fold_left node.out_edges ~init:[] ~f:(fun acc e ->
          acc @ [ Edge.dst e ])

    let preds node =
      List.fold_left node.in_edges ~init:[] ~f:(fun acc e ->
          acc @ [ Edge.src e ])

    let has_id (n : t) id =
      SIdentifier.Name.equal (SIdentifier.get_id n.id) (SIdentifier.get_id id)

    let to_string node =
      CGIdentifier.Name.as_string (CGIdentifier.get_id node.id)

    let compare n1 n2 = CGIdentifier.compare n1.id n2.id
  end

  module NodeSet = Set.Make (Node)

  let emp_nodes_set = NodeSet.empty

  type cg = { nodes : Node.t list; edges : Node.t Edge.t list }

  let find_node (nodes : Node.t list) name =
    List.find nodes ~f:(fun node -> CGIdentifier.equal node.id name)

  let get_node (cg : cg) name = find_node cg.nodes name

  (** Collects names of functions/procedures/transitions defined in the
      contract.
      @return List of created nodes. *)
  let collect_nodes (cmod : cmodule) =
    let collect_lib_functions (lib : library) =
      let rec aux init lib =
        List.fold_left lib.lentries ~init ~f:(fun acc lentry ->
            let mk_node name ty =
              let id : SR.rep CGIdentifier.t =
                CGIdentifier.mk_id (CGIdentifier.get_id name)
                @@ SR.parse_rep (ER.get_rep_str (CGIdentifier.get_rep name))
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
                  match List.find acc ~f:(fun n -> Node.has_id n id) with
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
            | LibVar (name, _user_ty_annot, (e, _annot)) ->
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

  (** Collects set of nodes with identifiers of functions/procedures called in
      the given expression. *)
  let rec collect_funcalls (e, _annot) collected_nodes =
    match e with
    | Let (_id, _ty, body, in_) ->
        collect_funcalls body collected_nodes
        |> NodeSet.union @@ collect_funcalls in_ collected_nodes
    | App (f, _) | TApp (f, _) | Var f ->
        find_node collected_nodes f
        |> Option.value_map ~default:emp_nodes_set ~f:(fun n ->
               NodeSet.singleton n)
    | MatchExpr (_id, arms) ->
        List.fold_left arms ~init:emp_nodes_set ~f:(fun acc (_p, ea) ->
            acc |> NodeSet.union @@ collect_funcalls ea collected_nodes)
    | Fun (_, _, ea) | TFun (_, ea) | Fixpoint (_, _, ea) | GasExpr (_, ea) ->
        collect_funcalls ea collected_nodes
    | Literal _ | Message _ | Constr _ | Builtin _ -> emp_nodes_set

  (** Returns a set of nodes that were called inside body of the component. *)
  let get_called_nodes comp (collected_nodes : Node.t list) =
    let rec visit_stmt (s, _annot) =
      match s with
      | Bind (_id, ea) -> collect_funcalls ea collected_nodes
      | CallProc (_, proc, _) | Iterate (_, proc) -> (
          find_node collected_nodes proc |> function
          | Some n -> NodeSet.singleton n
          | None -> emp_nodes_set)
      | MatchStmt (_id, arms) ->
          List.fold_left arms ~init:emp_nodes_set
            ~f:(fun acc (_pattern, stmts) ->
              List.fold_left stmts ~init:emp_nodes_set ~f:(fun acc sa ->
                  NodeSet.union acc @@ visit_stmt sa)
              |> NodeSet.union acc)
      | Load _ | RemoteLoad _ | Store _ | MapUpdate _ | MapGet _
      | RemoteMapGet _ | ReadFromBC _ | TypeCast _ | AcceptPayment | Return _
      | SendMsgs _ | CreateEvnt _ | Throw _ | GasStmt _ ->
          emp_nodes_set
    in
    List.fold_left comp.comp_body ~init:emp_nodes_set ~f:(fun acc s ->
        visit_stmt s |> NodeSet.union acc)

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
                 |> NodeSet.fold ~init:edges
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
                         |> NodeSet.fold ~init:edges
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

  let find_function (cg : cg) name =
    List.find cg.nodes ~f:(fun (n : Node.t) ->
        CGIdentifier.Name.equal (CGIdentifier.get_id n.id) name)

  (** Returns true iff [name] is the function name on the CG *)
  let is_function (cg : cg) name = find_function cg name |> Option.is_some

  (** Returns names of functions called inside the body of the caller. *)
  let get_callees (cg : cg) callee =
    find_function cg callee
    |> Option.value_map ~default:[] ~f:(fun n -> Node.succs n)

  (** Left fold over nodes in the DFS order. [f] takes an accumulator and a
      node and updates the accumulator. *)
  let fold_over_nodes_dfs (cg : cg) ~init ~f =
    let rec aux (n : Node.t) visited acc =
      if not @@ NodeSet.mem visited n then
        let visited, acc =
          List.fold_left (Node.succs n)
            ~init:(NodeSet.add visited n, acc)
            ~f:(fun (visited, acc) n -> aux n visited acc)
        in
        (visited, f acc n)
      else (visited, acc)
    in
    let _visited, acc =
      List.fold_left cg.nodes ~init:(emp_nodes_set, init)
        ~f:(fun (visited, acc) n -> aux n visited acc)
    in
    acc

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

    let iter_vertex f g =
      (* We iterate first transitions, then procedures, then functions.
         This will give us a better representation of the callgraph. *)
      List.sort g.nodes ~compare:(fun lv rv -> Node.compare_node_ty lv.ty rv.ty)
      |> List.iter ~f

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
