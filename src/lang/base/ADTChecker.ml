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
      
  let wrap_type_err e ?opt:(opt = "") = wrap_err e "recursion" ~opt:opt
  let wrap_type_serr s ?opt:(opt = "") = wrap_serr s "recursion" ~opt:opt
      
  (**************************************************************)
  (*                  Checking libraries                        *)
  (**************************************************************)

  let recursion_payload p =
    match p with
    | MTag s -> RecursionSyntax.MTag s
    | MLit l -> RecursionSyntax.MLit l
    | MVar x -> RecursionSyntax.MVar x

  let rec recursion_pattern p =
    match p with
    | Wildcard -> RecursionSyntax.Wildcard
    | Binder x -> RecursionSyntax.Binder x
    | Constructor (s, ps) ->
        RecursionSyntax.Constructor (s, List.map ps ~f:recursion_pattern)
  
  and recursion_exp erep =
    let (e, rep) = erep in
    let new_e =
      match e with
      | Literal l -> RecursionSyntax.Literal l
      | Var x -> RecursionSyntax.Var x
      | Let (x, t, e1, body) ->
          RecursionSyntax.Let (x, t, recursion_exp e1, recursion_exp body)
      | Message sps ->
          RecursionSyntax.Message (List.map sps ~f:(fun (s, p) -> (s, recursion_payload p)))
      | Fun (f, t, body) ->
          RecursionSyntax.Fun (f, t, recursion_exp body)
      | App (f, args) ->
          RecursionSyntax.App (f, args)
      | Constr (s, ts, args) ->
          RecursionSyntax.Constr (s, ts, args)
      | MatchExpr (x, pes) ->
          RecursionSyntax.MatchExpr (x, List.map pes ~f:(fun (p, e) -> (recursion_pattern p, recursion_exp e)))
      | Builtin (f, args) ->
          RecursionSyntax.Builtin (f, args)
      | TFun (s, e) ->
          RecursionSyntax.TFun (s, recursion_exp e)
      | TApp (f, ts) ->
          RecursionSyntax.TApp (f, ts)
      | Fixpoint (x, t, e) ->
          RecursionSyntax.Fixpoint (x, t, recursion_exp e) in
    (new_e, rep)

  let rec recursion_stmt srep =
    let (s, rep) = srep in
    let new_s =
      match s with
      | Load (x, f) -> RecursionSyntax.Load (x, f)
      | Store (f, x) -> RecursionSyntax.Store (f, x)
      | Bind (x, e) -> RecursionSyntax.Bind (x, recursion_exp e)
      | MapUpdate (m, is, vopt) -> RecursionSyntax.MapUpdate (m, is, vopt)
      | MapGet (x, m, is, del) -> RecursionSyntax.MapGet (x, m, is, del)
      | MatchStmt (x, pss) ->
          RecursionSyntax.MatchStmt (x, List.map pss ~f:(fun (p, ss) -> (recursion_pattern p, List.map ss ~f:recursion_stmt)))
      | ReadFromBC (x, f) -> RecursionSyntax.ReadFromBC (x, f)
      | AcceptPayment -> RecursionSyntax.AcceptPayment
      | SendMsgs msg -> RecursionSyntax.SendMsgs msg
      | CreateEvnt evnt -> RecursionSyntax.CreateEvnt evnt
      | Throw ex -> RecursionSyntax.Throw ex in
    (new_s, rep)

  let recursion_transition trns =
    let { tname ; tparams ; tbody } = trns in
    let recursion_tbody = List.map tbody ~f:recursion_stmt in
    { RecursionSyntax.tname = tname ;
      RecursionSyntax.tparams = tparams ;
      RecursionSyntax.tbody = recursion_tbody }
  
  let recursion_contract c =
    let { cname ; cparams ; cfields ; ctrans } = c in
    let recursion_cfields = List.map cfields ~f:(fun (x, t, e) -> (x, t, recursion_exp e)) in
    let recursion_ctrans = List.map ctrans ~f:recursion_transition in
    { RecursionSyntax.cname = cname ;
      RecursionSyntax.cparams = cparams ;
      RecursionSyntax.cfields = recursion_cfields ;
      RecursionSyntax.ctrans = recursion_ctrans }
  
  let recursion_adt_constructor_arg is_adt_type_valid t error_loc =
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
          let%bind _ = is_adt_type_valid s in
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
      
  let recursion_lib_entry is_adt_type_valid lib_entry =
    match lib_entry with
    | LibVar (n, e) -> pure @@ (RecursionSyntax.LibVar (n, recursion_exp e), None)
    | LibTyp (tname, ctr_defs) ->
        let%bind checked_ctr_defs =
          mapM ~f:(fun ({ cname ; c_arg_types } : ctr_def) ->
              let error_loc = ER.get_loc (get_rep cname) in
              let%bind recursion_c_arg_types = mapM ~f:(fun c_arg -> recursion_adt_constructor_arg is_adt_type_valid c_arg error_loc) c_arg_types in
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
          Some { Datatypes.tname = get_id tname ;
                 (* Polymorphic definitions disallowed for the time being *)
                 Datatypes.tparams = [] ;
                 Datatypes.tconstr = datatype_ctrs ;
                 Datatypes.tmap = datatype_tmap } )
            
    
  let recursion_library lib =
    let { lname ; lentries } = lib in
    let is_adt_type_valid adts_in_scope adt_name =
      (* Check if type has already been declared *)
      match List.findi adts_in_scope ~f:(fun _ n -> n = adt_name) with
      | Some _ -> pure @@ ()
      | None ->
          (* Check if the name is a builtin ADT *)
          let%bind _ = DataTypeDictionary.lookup_name adt_name
          in pure @@ () in
    let%bind (recursion_entries, adts, _) =
      foldrM
        ~f:(fun (rec_entries, datatypes, adts_in_scope) entry ->
            let%bind (new_entry, adt_opt) = recursion_lib_entry (is_adt_type_valid adts_in_scope) entry in
            match adt_opt with
            (* LibVar *)
            | None -> pure @@ (new_entry :: rec_entries, datatypes, adts_in_scope)
            (* LibTyp *)
            | Some adt ->
                pure @@ (new_entry :: rec_entries, adt :: datatypes, adt.tname :: adts_in_scope ))
        ~init:([], [], [])
        lentries in
    pure @@ (
      { RecursionSyntax.lname = lname ; RecursionSyntax.lentries = recursion_entries },
      adts)

  let recursion_module
      (md : cmodule)
      (recursion_principles : lib_entry list)
      (ext_libs : library list)
    : (RecursionSyntax.cmodule * (RecursionSyntax.lib_entry list) * (RecursionSyntax.library list), scilla_error list) result =
    let { smver ; cname ; libs ; elibs ; contr } = md in
    let%bind rec_elibs_and_adts = mapM ~f:recursion_library ext_libs in
    let (recursion_elibs, elibs_adts_unflattened) = List.unzip rec_elibs_and_adts in
    let _ = List.iter (List.concat elibs_adts_unflattened) ~f:DataTypeDictionary.add_adt in
    let%bind recursion_md_libs =
      match libs with
      | None -> pure @@ None
      | Some l ->
          (* TODO: Ensure elib ADTs are in scope for md_lib datatype constructors *)
          let%bind (recursion_l, recursion_adts) = recursion_library l in
          let _ = List.iter recursion_adts ~f:DataTypeDictionary.add_adt in
          pure @@ Some recursion_l in
    (* TODO: Recursion principles should be generated by this phase *)
    let%bind recursion_rprins_adts = mapM recursion_principles ~f:(fun rprin -> recursion_lib_entry (fun _ -> pure @@ ()) rprin) in
    let (recursion_rprins, _) = List.unzip recursion_rprins_adts in
    let recursion_contr = recursion_contract contr in
    pure @@ (
      { RecursionSyntax.smver = smver ;
        RecursionSyntax.cname = cname ;
        RecursionSyntax.libs = recursion_md_libs ;
        RecursionSyntax.elibs = elibs ;
        RecursionSyntax.contr = recursion_contr },
      recursion_rprins,
      recursion_elibs)

  (**************************************************************)
  (*                    Staging API                             *)
  (**************************************************************)

  module OutputSyntax = RecursionSyntax
  module OutputSRep = SRecRep
  module OutputERep = ERecRep

end
