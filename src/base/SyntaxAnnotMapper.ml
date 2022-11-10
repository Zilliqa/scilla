(*
  This file is part of scilla.

  Copyright (c) 2022 - present Zilliqa Research Pvt. Ltd.

  scilla is free software: you can redistribute it and/or modify it under the
  terms of the GNU General Public License as published by the Free Software
  Foundation, either version 3 of the License, or (at your option) any later
  version.

  scilla is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
  A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along with
*)

(**
  This module allows one to map all AST annotations for a given AST
 *)

open Core
open Syntax
open Literal

module MapSyntax
    (SRfrom : Rep)
    (ERfrom : Rep)
    (Literal : ScillaLiteral)
    (SRto : Rep)
    (ERto : Rep) =
struct
  module MLiteral = Literal
  module MType = MLiteral.LType
  module MIdentifier = MType.TIdentifier
  module OutputSyntax = ScillaSyntax (SRto) (ERto) (Literal)
  include OutputSyntax
  module InputSyntax = ScillaSyntax (SRfrom) (ERfrom) (Literal)
  open InputSyntax

  let map_id f = function
    | MIdentifier.Ident (id, ann) -> MIdentifier.mk_id id (f ann)

  let payload p ~fe =
    match p with
    | MLit l -> OutputSyntax.MLit l
    | MVar v -> OutputSyntax.MVar (map_id fe v)

  let pattern pat ~fe ~fs =
    let rec walk = function
      | Wildcard -> OutputSyntax.Wildcard
      | Binder b -> OutputSyntax.Binder (map_id fe b)
      | Constructor (id, ps) ->
          OutputSyntax.Constructor
            (map_id fs id, List.map ps ~f:(fun p -> walk p))
    in
    walk pat

  (* type idents are identifiers with _location_ annotation, not general annotation *)
  let rec map_type ty ~fl =
    let rec walk ty =
      let open MType in
      match ty with
      | PrimType _ | Unit | TypeVar _ -> ty
      | MapType (ty1, ty2) -> MapType (walk ty1, walk ty2)
      | FunType (ty1, ty2) -> FunType (walk ty1, walk ty2)
      | PolyFun (tv, ty) -> PolyFun (tv, walk ty)
      | ADT (tid, tys) ->
          ADT (map_id fl tid, List.map tys ~f:(fun ty -> walk ty))
      | Address ak -> Address (addr_kind ak ~fl)
    in
    walk ty

  and addr_kind kind ~fl =
    let open MType in
    match kind with
    | AnyAddr | CodeAddr | LibAddr -> kind
    | ContrAddr fields_map ->
        let mapped_keys =
          IdLoc_Comp.Map.map_keys_exn fields_map ~f:(fun f -> map_id fl f)
        in
        let mapped_values =
          IdLoc_Comp.Map.map mapped_keys ~f:(fun ty -> map_type ty ~fl)
        in
        ContrAddr mapped_values

  let literal lit ~fl =
    let rec walk lit =
      let open MLiteral in
      match lit with
      | StringLit _ | IntLit _ | UintLit _ | BNum _ | ByStrX _ | ByStr _ | Clo _
      | TAbs _ ->
          lit
      | ADTValue (name, tys, args) ->
          ADTValue
            ( name,
              List.map tys ~f:(fun ty -> map_type ty ~fl),
              List.map args ~f:(fun a -> walk a) )
      | Map ((kt, vt), table) -> Map ((map_type kt ~fl, map_type vt ~fl), table)
      | Msg entries ->
          Msg
            (List.map entries ~f:(fun (msg_field, ty, value) ->
                 (msg_field, map_type ty ~fl, walk value)))
    in
    walk lit

  let rec gas_charge gc =
    let open SGasCharge in
    match gc with
    | StaticCost i -> OutputSyntax.SGasCharge.StaticCost i
    | SizeOf v -> OutputSyntax.SGasCharge.SizeOf v
    | ValueOf v -> OutputSyntax.SGasCharge.ValueOf v
    | LengthOf v -> OutputSyntax.SGasCharge.LengthOf v
    | MapSortCost m -> OutputSyntax.SGasCharge.MapSortCost m
    | SumOf (g1, g2) ->
        OutputSyntax.SGasCharge.SumOf (gas_charge g1, gas_charge g2)
    | ProdOf (g1, g2) ->
        OutputSyntax.SGasCharge.ProdOf (gas_charge g1, gas_charge g2)
    | MinOf (g1, g2) ->
        OutputSyntax.SGasCharge.MinOf (gas_charge g1, gas_charge g2)
    | DivCeil (g1, g2) -> OutputSyntax.SGasCharge.DivCeil (gas_charge g1, g2)
    | LogOf g -> OutputSyntax.SGasCharge.LogOf (gas_charge g)

  let expr_annot e_ann ~fe ~fs ~fl =
    let rec walk e_ann =
      let e, ann = e_ann in
      let ann' = fe ann in
      match e with
      | Literal l -> (OutputSyntax.Literal (literal l ~fl), ann')
      | Message m ->
          let ns, ps = List.unzip m in
          let ps' = List.map ps ~f:(fun p -> payload p ~fe) in
          let m' = List.zip_exn ns ps' in
          (OutputSyntax.Message m', ann')
      | Var v -> (OutputSyntax.Var (map_id fe v), ann')
      | Let (v, oty, elhs, erhs) ->
          let dt = Option.map oty ~f:(fun t -> map_type t ~fl) in
          (OutputSyntax.Let (map_id fe v, dt, walk elhs, walk erhs), ann')
      | Fun (i, ty, e_ann) ->
          (OutputSyntax.Fun (map_id fe i, map_type ty ~fl, walk e_ann), ann')
      | App (i, ils) ->
          (OutputSyntax.App (map_id fe i, List.map ils ~f:(map_id fe)), ann')
      | Constr (cname, tys, idl) ->
          ( OutputSyntax.Constr
              ( map_id fs cname,
                List.map tys ~f:(fun ty -> map_type ty ~fl),
                List.map idl ~f:(map_id fe) ),
            ann' )
      | MatchExpr (pv, branches) ->
          let pats, es = List.unzip branches in
          let pats' = List.map pats ~f:(fun p -> pattern p ~fe ~fs) in
          let es' = List.map es ~f:walk in
          let branches' = List.zip_exn pats' es' in
          (OutputSyntax.MatchExpr (map_id fe pv, branches'), ann')
      | Builtin ((b, bann), tys, args) ->
          ( OutputSyntax.Builtin
              ( (b, fe bann),
                List.map tys ~f:(fun ty -> map_type ty ~fl),
                List.map args ~f:(map_id fe) ),
            ann' )
      | TFun (ti, e) -> (OutputSyntax.TFun (map_id fe ti, walk e), ann')
      | TApp (i, tys) ->
          ( OutputSyntax.TApp
              (map_id fe i, List.map tys ~f:(fun ty -> map_type ty ~fl)),
            ann' )
      | Fixpoint (i, ty, e) ->
          (OutputSyntax.Fixpoint (map_id fe i, map_type ty ~fl, walk e), ann')
      | GasExpr (gc, e) -> (OutputSyntax.GasExpr (gas_charge gc, walk e), ann')
    in
    walk e_ann

  let bc_query q ~fe =
    match q with
    | CurBlockNum -> OutputSyntax.CurBlockNum
    | ChainID -> OutputSyntax.ChainID
    | Timestamp id -> OutputSyntax.Timestamp (map_id fe id)
    | ReplicateContr (addr, init_params_id) ->
        OutputSyntax.ReplicateContr (map_id fe addr, map_id fe init_params_id)

  let rec statement stmt ~fe ~fl ~fs =
    match stmt with
    | Load (id, field) -> OutputSyntax.Load (map_id fe id, map_id fe field)
    | RemoteLoad (id, addr, field, is_mutable) ->
        OutputSyntax.RemoteLoad (map_id fe id, map_id fe addr, map_id fe field, is_mutable)
    | Store (field, value) ->
        OutputSyntax.Store (map_id fe field, map_id fe value)
    | Bind (id, e) -> OutputSyntax.Bind (map_id fe id, expr_annot e ~fe ~fl ~fs)
    | MapUpdate (map, keys, mode) ->
        OutputSyntax.MapUpdate
          ( map_id fe map,
            List.map keys ~f:(fun k -> map_id fe k),
            Option.map mode ~f:(fun m -> map_id fe m) )
    | MapGet (value, map, keys, mode) ->
        OutputSyntax.MapGet
          ( map_id fe value,
            map_id fe map,
            List.map keys ~f:(fun k -> map_id fe k),
            mode )
    | RemoteMapGet (value, addr, map, is_mutable, keys, mode) ->
        OutputSyntax.RemoteMapGet
          ( map_id fe value,
            map_id fe addr,
            map_id fe map,
            is_mutable,
            List.map keys ~f:(fun k -> map_id fe k),
            mode )
    | MatchStmt (matchee, branches) ->
        let pats, stmtss = List.unzip branches in
        let pats = List.map pats ~f:(fun p -> pattern p ~fe ~fs) in
        let stmtss =
          List.map stmtss ~f:(fun stmts -> statements_annot stmts ~fe ~fl ~fs)
        in
        let branches = List.zip_exn pats stmtss in
        OutputSyntax.MatchStmt (map_id fe matchee, branches)
    | ReadFromBC (value, query) ->
        OutputSyntax.ReadFromBC (map_id fe value, bc_query query ~fe)
    | TypeCast (id, addr, ty) ->
        OutputSyntax.TypeCast (map_id fe id, map_id fe addr, map_type ty ~fl)
    | AcceptPayment -> OutputSyntax.AcceptPayment
    | Iterate (list, proc) ->
        OutputSyntax.Iterate (map_id fe list, map_id fs proc)
    | SendMsgs id -> OutputSyntax.SendMsgs (map_id fe id)
    | CreateEvnt id -> OutputSyntax.CreateEvnt (map_id fe id)
    | CallProc (proc, args) ->
        OutputSyntax.CallProc
          (map_id fs proc, List.map args ~f:(fun a -> map_id fe a))
    | Throw oid ->
        OutputSyntax.Throw (Option.map oid ~f:(fun id -> map_id fe id))
    | GasStmt gc -> OutputSyntax.GasStmt (gas_charge gc)

  and statements_annot stmts ~fe ~fl ~fs =
    List.map stmts ~f:(fun s -> statement_annot s ~fe ~fl ~fs)

  and statement_annot (stmt, ann) ~fe ~fl ~fs =
    (statement stmt ~fe ~fl ~fs, fs ann)

  let ctr_def { cname; c_arg_types } ~fe ~fl =
    OutputSyntax.
      {
        cname = map_id fe cname;
        c_arg_types = List.map c_arg_types ~f:(fun ty -> map_type ty ~fl);
      }

  let lib_entry ~fe ~fl ~fs = function
    | LibVar (def, oty, e) ->
        OutputSyntax.LibVar
          ( map_id fe def,
            Option.map oty ~f:(fun ty -> map_type ty ~fl),
            expr_annot e ~fe ~fl ~fs )
    | LibTyp (ty_id, ctr_defs) ->
        OutputSyntax.LibTyp
          (map_id fe ty_id, List.map ctr_defs ~f:(fun cd -> ctr_def cd ~fe ~fl))

  let component { comp_type; comp_name; comp_params; comp_body } ~fe ~fl ~fs =
    OutputSyntax.
      {
        comp_type;
        comp_name = map_id fs comp_name;
        comp_params =
          List.map comp_params ~f:(fun (param, ty) ->
              (map_id fe param, map_type ty ~fl));
        comp_body = statements_annot comp_body ~fe ~fl ~fs;
      }

  let contract { cname; cparams; cconstraint; cfields; ccomps } ~fe ~fs ~fl =
    OutputSyntax.
      {
        cname = map_id fs cname;
        cparams =
          List.map cparams ~f:(fun (param, ty) ->
              (map_id fe param, map_type ty ~fl));
        cconstraint = expr_annot cconstraint ~fe ~fs ~fl;
        cfields =
          List.map cfields ~f:(fun (field, ty, init) ->
              (map_id fe field, map_type ty ~fl, expr_annot init ~fe ~fs ~fl));
        ccomps = List.map ccomps ~f:(fun c -> component c ~fe ~fl ~fs);
      }

  let library { lname; lentries } ~fe ~fl ~fs =
    OutputSyntax.
      {
        lname = map_id fs lname;
        lentries = List.map lentries ~f:(fun le -> lib_entry le ~fl ~fe ~fs);
      }

  let cmodule { smver; libs; elibs; contr } ~fe ~fl ~fs =
    OutputSyntax.
      {
        smver;
        libs = Option.map libs ~f:(fun l -> library l ~fe ~fl ~fs);
        elibs =
          List.map elibs ~f:(fun (libname, onamespace) ->
              ( map_id fs libname,
                Option.map onamespace ~f:(fun ns -> map_id fs ns) ));
        contr = contract contr ~fe ~fs ~fl;
      }
end
