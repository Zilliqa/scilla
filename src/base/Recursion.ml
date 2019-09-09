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

open Syntax
open Core
open ErrorUtils
open MonadUtil
open Result.Let_syntax
open Datatypes
    
(*****************************************************************)
(*   Recursion principles for built-in and user-defined ADTs     *)
(*****************************************************************)

module ScillaRecursion
  (SR : Rep)
  (ER : Rep) = struct

  module PreRecursionSyntax = ScillaSyntax (SR) (ER)
  module SRecRep = SR
  module ERecRep = ER
  module RecursionSyntax = ScillaSyntax (SRecRep) (ERecRep)
  
  include RecursionSyntax
  include ERecRep
  
  open PreRecursionSyntax
      
  let wrap_recursion_err e ?opt:(opt = "") = wrap_err e "ADT" ~opt:opt
  let wrap_recursion_serr s ?opt:(opt = "") = wrap_serr s "ADT" ~opt:opt

  (**************************************************************)
  (*                  Checking libraries                        *)
  (**************************************************************)

  let recursion_typ is_adt_in_scope t =
    let rec walk t =
      match t with
      | PrimType _
      | TypeVar _
      | Unit -> pure @@ true
      | MapType (t1, t2)
      | FunType (t1, t2) ->
          let%bind _ = walk t1 in
          walk t2
      | ADT (s, targs) ->
          let%bind _ = is_adt_in_scope s in
          forallM ~f:walk targs
      | PolyFun (_, t) ->
          walk t in
    walk t
  
  let recursion_payload p =
    match p with
    | MLit l -> RecursionSyntax.MLit l
    | MVar x -> RecursionSyntax.MVar x

  let recursion_pattern is_adt_ctr_in_scope p =
    let rec walk p =
      match p with
      | Wildcard -> pure @@ RecursionSyntax.Wildcard
      | Binder x -> pure @@ RecursionSyntax.Binder x
      | Constructor (s, ps) ->
          let%bind _ = is_adt_ctr_in_scope s in
          let%bind new_ps = mapM ps ~f:walk in
          pure @@ RecursionSyntax.Constructor (s, new_ps) in
    walk p
  
  let recursion_exp is_adt_in_scope is_adt_ctr_in_scope erep =
    let rec walk erep =
      let (e, rep) = erep in
      let%bind new_e = 
        match e with
        | Literal l -> pure @@ RecursionSyntax.Literal l
        | Var x -> pure @@ RecursionSyntax.Var x
        | Let (x, t, e1, body) ->
            let%bind new_e1 = walk e1 in
            let%bind _ =
              match t with
              | Some t' -> recursion_typ is_adt_in_scope t'
              | None -> pure @@ true in
            let%bind new_body = walk body in
            pure @@ RecursionSyntax.Let (x, t, new_e1, new_body)
        | Message sps ->
            pure @@ RecursionSyntax.Message (
              List.map sps ~f:(fun (s, p) -> (s, recursion_payload p)))
        | Fun (f, t, body) ->
            let%bind _ = recursion_typ is_adt_in_scope t in
            let%bind new_body = walk body in
            pure @@ RecursionSyntax.Fun (f, t, new_body)
        | App (f, args) ->
            pure @@ RecursionSyntax.App (f, args)
        | Constr (s, ts, args) ->
            let%bind _ = forallM ~f:(fun t -> recursion_typ is_adt_in_scope t) ts in
            let%bind _ = is_adt_ctr_in_scope s in
            pure @@ RecursionSyntax.Constr (s, ts, args)
        | MatchExpr (x, pes) ->
            let%bind new_pes =
              mapM ~f:(fun (p, e) ->
                  let%bind new_p = recursion_pattern is_adt_ctr_in_scope p in
                  let%bind new_e = walk e in
                  pure @@ (new_p, new_e)) pes in
            pure @@ RecursionSyntax.MatchExpr (x, new_pes)
        | Builtin (f, args) ->
            pure @@ RecursionSyntax.Builtin (f, args)
        | TFun (s, e) ->
            let%bind new_e = walk e in
            pure @@ RecursionSyntax.TFun (s, new_e)
        | TApp (f, ts) ->
            let%bind _ = forallM ~f:(fun t -> recursion_typ is_adt_in_scope t) ts in
            pure @@ RecursionSyntax.TApp (f, ts)
        | Fixpoint (x, t, e) ->
            let%bind _ = recursion_typ is_adt_in_scope t in
            let%bind new_e = walk e in
            pure @@ RecursionSyntax.Fixpoint (x, t, new_e) in
      pure @@ (new_e, rep) in
    walk erep

  let recursion_stmt is_adt_in_scope is_adt_ctr_in_scope is_proc_in_scope srep =
    let rec_exp e = recursion_exp is_adt_in_scope is_adt_ctr_in_scope e in
    let rec walk srep =
      let (s, rep) = srep in
      let%bind new_s =
        match s with
        | Load (x, f) -> pure @@ RecursionSyntax.Load (x, f)
        | Store (f, x) -> pure @@ RecursionSyntax.Store (f, x)
        | Bind (x, e) ->
            let%bind new_e = rec_exp e in
            pure @@ RecursionSyntax.Bind (x, new_e)
        | MapUpdate (m, is, vopt) -> pure @@ RecursionSyntax.MapUpdate (m, is, vopt)
        | MapGet (x, m, is, del) -> pure @@ RecursionSyntax.MapGet (x, m, is, del)
        | MatchStmt (x, pss) ->
            let%bind new_pss =
              mapM ~f:(fun (p, ss) ->
                  let%bind new_p = recursion_pattern is_adt_ctr_in_scope p in
                  let%bind new_ss = mapM ~f:walk ss in
                  pure @@ (new_p, new_ss)) pss in
            pure @@ RecursionSyntax.MatchStmt (x, new_pss)
        | ReadFromBC (x, f) -> pure @@ RecursionSyntax.ReadFromBC (x, f)
        | AcceptPayment -> pure @@ RecursionSyntax.AcceptPayment
        | SendMsgs msg -> pure @@ RecursionSyntax.SendMsgs msg
        | CreateEvnt evnt -> pure @@ RecursionSyntax.CreateEvnt evnt
        | CallProc (p, args) ->
            if is_proc_in_scope (get_id p)
            then pure @@ RecursionSyntax.CallProc(p, args)
            else fail1 (sprintf "Procedure %s is not in scope." (get_id p)) (SR.get_loc rep)
        | Throw ex -> pure @@ RecursionSyntax.Throw ex in
      pure @@ (new_s, rep) in
    walk srep

  let recursion_component is_adt_in_scope is_adt_ctr_in_scope is_proc_in_scope comp =
    let { comp_type; comp_name ; comp_params ; comp_body } = comp in
    let%bind _ = forallM ~f:(fun (_, t) -> recursion_typ is_adt_in_scope t) comp_params in
    let%bind recursion_comp_body =
      mapM ~f:(fun s ->
          recursion_stmt is_adt_in_scope is_adt_ctr_in_scope is_proc_in_scope s) comp_body in
    pure @@
    { RecursionSyntax.comp_type = comp_type ;
      RecursionSyntax.comp_name = comp_name ;
      RecursionSyntax.comp_params = comp_params ;
      RecursionSyntax.comp_body = recursion_comp_body }
  
  let recursion_contract is_adt_in_scope is_adt_ctr_in_scope c =
    let { cname ; cparams ; cfields ; ccomps } = c in
    let%bind _ = forallM ~f:(fun (_, t) -> recursion_typ is_adt_in_scope t) cparams in
    let%bind recursion_cfields =
      mapM ~f:(fun (x, t, e) ->
          let%bind _ = recursion_typ is_adt_in_scope t in
          let%bind new_e = recursion_exp is_adt_in_scope is_adt_ctr_in_scope e in
          pure @@ (x, t, new_e)) cfields in
    let%bind (recursion_ccomps_rev, _) =
      foldM ccomps ~init:([], []) ~f:(fun (acc_comps, acc_procs) comp ->
          let is_proc_in_scope p =
            List.exists acc_procs ~f:(fun x -> p = x) in
          let%bind checked_component =
            recursion_component is_adt_in_scope is_adt_ctr_in_scope is_proc_in_scope comp in
          let new_acc_procs =
            match comp.comp_type with
            | CompTrans -> acc_procs
            | CompProc -> (get_id comp.comp_name) :: acc_procs in
          pure @@ (checked_component :: acc_comps, new_acc_procs)
        ) in
    pure @@
    { RecursionSyntax.cname = cname ;
      RecursionSyntax.cparams = cparams ;
      RecursionSyntax.cfields = recursion_cfields ;
      RecursionSyntax.ccomps = List.rev recursion_ccomps_rev }
  
  let recursion_adt_constructor_arg is_adt_in_scope t error_loc =
    let rec walk t =
      match t with
      | PrimType t -> pure @@ PrimType t
      | MapType (t1, t2) ->
          let%bind checked_t1 = walk t1 in
          let%bind checked_t2 = walk t2 in
          pure @@ MapType (checked_t1, checked_t2)
      | FunType (t1, t2) ->
          let%bind checked_t1 = walk t1 in
          let%bind checked_t2 = walk t2 in
          pure @@ FunType (checked_t1, checked_t2)
      | ADT (s, targs) ->
          (* Only allow ADTs that are already in scope. This prevents mutually inductive definitions. *)
          let%bind _ = is_adt_in_scope s in
          let%bind checked_targs = mapM targs ~f:walk in
          pure @@ ADT (s, checked_targs)
      | TypeVar _ -> 
          (* Disallow polymorphic definitions for the time being. *)
          fail1 "Type variables not allowed in type definitions" error_loc
      | PolyFun _ ->
          (* Disallow polymorphic definitions for the time being. *)
          fail1 "Type variables not allowed in type definitions" error_loc
      | Unit -> pure @@ Unit in
    walk t
      
  let recursion_lib_entry is_adt_in_scope is_adt_ctr_in_scope lib_entry =
    match lib_entry with
    | LibVar (n, t, e) ->
        wrap_with_info (
          sprintf "Type error in library variable %s:\n" (get_id n), ER.get_loc (get_rep n)) @@
        let%bind new_e = recursion_exp is_adt_in_scope is_adt_ctr_in_scope e in
        let%bind _ =
          match t with
          | Some t' -> recursion_typ is_adt_in_scope t'
          | None -> pure @@ true in
        pure @@ (RecursionSyntax.LibVar (n, t, new_e), None)
    | LibTyp (tname, ctr_defs) ->
        wrap_with_info (
          sprintf "Type error in library type %s:\n" (get_id tname), ER.get_loc (get_rep tname)) @@
        let%bind checked_ctr_defs =
          mapM ~f:(fun ({ cname ; c_arg_types } : ctr_def) ->
              let error_loc = ER.get_loc (get_rep cname) in
              let%bind recursion_c_arg_types = mapM ~f:(fun c_arg -> recursion_adt_constructor_arg is_adt_in_scope c_arg error_loc) c_arg_types in
              pure @@ { RecursionSyntax.cname = cname ; RecursionSyntax.c_arg_types = recursion_c_arg_types }) ctr_defs in
        let (datatype_ctrs, datatype_tmap) =
          List.fold_right ctr_defs ~init:([], [])
            ~f:(fun ctr_def (ctrs, maps) ->
                let { cname ; c_arg_types } = ctr_def in
                let ctr = { Datatypes.cname = get_id cname ; Datatypes.arity = List.length c_arg_types } in
                let map = ( get_id cname, c_arg_types ) in
                ( ctr :: ctrs, map :: maps )) in
        (* Add type to ADTs in scope once checked. Adding the type after checking prevents inductive definitions. *)
        pure @@
        ( RecursionSyntax.LibTyp (tname, checked_ctr_defs),
          Some ({ Datatypes.tname = get_id tname ;
                  (* Polymorphic definitions disallowed for the time being *)
                  Datatypes.tparams = [] ;
                  Datatypes.tconstr = datatype_ctrs ;
                  Datatypes.tmap = datatype_tmap }, ER.get_loc (get_rep tname)))
            
  let recursion_library lib =
    let { lname ; lentries } = lib in
    let is_adt_in_scope adts_in_scope adt_name =
      (* Check if type has already been declared *)
      match List.findi adts_in_scope ~f:(fun _ n -> n = adt_name) with
      | Some _ -> pure @@ ()
      | None ->
          (* Check if the name is a builtin ADT *)
          let%bind _ = DataTypeDictionary.lookup_name adt_name
          in pure @@ () in
    let is_adt_ctr_in_scope adt_ctrs_in_scope ctr_name =
      (* Check if type has already been declared *)
      match List.findi adt_ctrs_in_scope ~f:(fun _ n -> n = ctr_name) with
      | Some _ -> pure @@ ()
      | None ->
          (* Check if the name is a builtin ADT *)
          let%bind _ = DataTypeDictionary.lookup_constructor ctr_name
          in pure @@ () in
    wrap_with_info (
      sprintf "Type error in library %s:\n" (get_id lname), SR.get_loc (get_rep lname)) @@
    let%bind (recursion_entries, adts, _, _) =
      foldM
        ~f:(fun (rec_entries, datatypes, adts_in_scope, adt_ctrs_in_scope) entry ->
            let%bind (new_entry, adt_opt) = recursion_lib_entry (is_adt_in_scope adts_in_scope) (is_adt_ctr_in_scope adt_ctrs_in_scope) entry in
            match adt_opt with
            (* LibVar *)
            | None -> pure @@ (new_entry :: rec_entries, datatypes, adts_in_scope, adt_ctrs_in_scope)
            (* LibTyp *)
            | Some (adt, loc) ->
                pure @@ (new_entry :: rec_entries,
                         (adt, loc) :: datatypes,
                         adt.tname :: adts_in_scope,
                         (List.map adt.tconstr ~f:(fun ctr -> ctr.cname)) @ adt_ctrs_in_scope ))
        ~init:([], [], [], [])
        lentries in
    pure @@ (
      { RecursionSyntax.lname = lname ;
        RecursionSyntax.lentries = List.rev recursion_entries },
      List.rev adts)


  let recursion_rprins_elibs recursion_principles ext_libs libs =

    let rec recurser libl =
      List.fold_left libl ~init:(([], []), [])
        ~f:(fun ((rec_elibs_acc, adts_acc), emsgs_acc) ext_lib ->
            let ((rec_elib, elib_adt), emsg) =
              let ((rec_dep_libs, dep_adt), dep_emsgs) = recurser ext_lib.deps in
              match recursion_library ext_lib.libn with
              | Ok (lib, adt) ->
                let (libn' : RecursionSyntax.libtree) = { libn = lib; deps = rec_dep_libs } in
                ((rec_elibs_acc @ [libn'], adts_acc @ dep_adt @ adt), emsgs_acc @ dep_emsgs)
              | Error el -> ((rec_elibs_acc, adts_acc), emsgs_acc @ dep_emsgs @ el) in
            ((rec_elib, elib_adt), emsg)) in

    let ((recursion_elibs, elibs_adts), emsgs) = recurser ext_libs in
    let emsgs =
      List.fold_left elibs_adts ~init:emsgs
        ~f:(fun emsgs_acc (adt, loc) ->
            match DataTypeDictionary.add_adt adt loc with
            | Ok _ -> emsgs_acc
            | Error e -> emsgs_acc @ e) in
    
    let%bind (recursion_md_libs, emsgs) =
      match libs with
      | None -> Ok (None, emsgs)
      | Some l ->
          match recursion_library l with
          | Ok (recursion_l, recursion_adts) ->
              let new_emsgs = List.fold_left recursion_adts ~init:emsgs
                  ~f:(fun emsgs_acc (adt, loc) ->
                      match DataTypeDictionary.add_adt adt loc with
                      | Ok _ -> emsgs
                      | Error e -> emsgs_acc @ e) in
              Ok (Some recursion_l, new_emsgs)
          | Error el -> Ok (None, emsgs @ el) in
    
    (* TODO, issue #225: Recursion principles should be generated by this phase *)
    let%bind (recursion_rprins_rev, emsgs) =
      foldM recursion_principles ~init:([], emsgs)
        ~f:(fun (rec_rprins_acc, emsgs_acc) rprin ->
            match recursion_lib_entry
                    (fun _ -> pure @@ ())
                    (fun _ -> pure @@ ())
                    rprin with
            | Ok (rec_rprin, _) -> Ok (rec_rprin :: rec_rprins_acc, emsgs_acc)
            | Error el -> Ok (rec_rprins_acc, emsgs_acc @ el)) in
    let recursion_rprins = List.rev recursion_rprins_rev in
    (* We're done, return the computed values. *)
    pure (recursion_rprins, recursion_elibs, recursion_md_libs, emsgs)

  let recursion_lmodule
      (md : lmodule)
      (recursion_principles : lib_entry list)
      (ext_libs : libtree list)
    : (RecursionSyntax.lmodule * (RecursionSyntax.lib_entry list) * (RecursionSyntax.libtree list), scilla_error list) result =
    wrap_with_info (
      sprintf "Type error(s) in library %s:\n" (get_id md.libs.lname), SR.get_loc (get_rep md.libs.lname)) @@

    let%bind (recursion_rprins, recursion_elibs, recursion_md_libs, emsgs) = 
      recursion_rprins_elibs recursion_principles ext_libs (Some md.libs) in

    let%bind recursion_md_libs' =
      match recursion_md_libs with
      | Some s -> pure s
      | None -> fail1 "Internal error in typing library module." (SR.get_loc (get_rep md.libs.lname))
    in

    if emsgs = [] then
      pure @@ (
        { RecursionSyntax.libs = recursion_md_libs';
          RecursionSyntax.elibs = md.elibs },
        recursion_rprins,
        recursion_elibs)
    else
      fail @@ emsgs

  let recursion_module
      (md : cmodule)
      (recursion_principles : lib_entry list)
      (ext_libs : libtree list)
    : (RecursionSyntax.cmodule * (RecursionSyntax.lib_entry list) * (RecursionSyntax.libtree list), scilla_error list) result =
    let { smver ; cname ; libs ; elibs ; contr } = md in
    wrap_with_info (
      sprintf "Type error(s) in contract %s:\n" (get_id contr.cname), SR.get_loc (get_rep contr.cname)) @@

    let%bind (recursion_rprins, recursion_elibs, recursion_md_libs, emsgs) = 
      recursion_rprins_elibs recursion_principles ext_libs libs in

    let%bind (recursion_contr, emsgs) =
      match recursion_contract
              (fun n -> let%bind _ = DataTypeDictionary.lookup_name n in pure @@ ())
              (fun n -> let%bind _ = DataTypeDictionary.lookup_constructor n in pure @@ ())
              contr with
      | Ok rec_contr -> Ok (rec_contr, emsgs)
      | Error el -> Ok ({cname = cname; cparams = [] ; cfields = [] ; ccomps = []}, emsgs @ el) in

    if emsgs = [] then
      pure @@ (
        { RecursionSyntax.smver = smver ;
          RecursionSyntax.cname = cname ;
          RecursionSyntax.libs = recursion_md_libs ;
          RecursionSyntax.elibs = elibs ;
          RecursionSyntax.contr = recursion_contr },
        recursion_rprins,
        recursion_elibs)
    else
      fail @@ emsgs

  (**************************************************************)
  (*                    Staging API                             *)
  (**************************************************************)

  module OutputSyntax = RecursionSyntax
  module OutputSRep = SRecRep
  module OutputERep = ERecRep

end
