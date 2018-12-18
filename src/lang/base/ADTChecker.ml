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
(*               Checking user-defined ADTs                      *)
(*****************************************************************)

module ScillaADTChecker
  (SR : Rep)
  (ER : Rep) = struct

  module ADTUncheckedSyntax = ScillaSyntax (SR) (ER)
  module SADTR = SR
  module EADTR = ER
  module ADTCheckedSyntax = ScillaSyntax (SADTR) (EADTR)
  
  include ADTCheckedSyntax
  include ER
  
  open ADTUncheckedSyntax
      
  let wrap_type_err e ?opt:(opt = "") = wrap_err e "ADT checking" ~opt:opt
  let wrap_type_serr s ?opt:(opt = "") = wrap_serr s "ADT checking" ~opt:opt
      
  (**************************************************************)
  (*                  Checking libraries                        *)
  (**************************************************************)

  let adt_check_payload p =
    match p with
    | MTag s -> ADTCheckedSyntax.MTag s
    | MLit l -> ADTCheckedSyntax.MLit l
    | MVar x -> ADTCheckedSyntax.MVar x

  let rec adt_check_pattern p =
    match p with
    | Wildcard -> ADTCheckedSyntax.Wildcard
    | Binder x -> ADTCheckedSyntax.Binder x
    | Constructor (s, ps) ->
        ADTCheckedSyntax.Constructor (s, List.map ps ~f:adt_check_pattern)
  
  and adt_check_exp erep =
    let (e, rep) = erep in
    let new_e =
      match e with
      | Literal l -> ADTCheckedSyntax.Literal l
      | Var x -> ADTCheckedSyntax.Var x
      | Let (x, t, e1, body) ->
          ADTCheckedSyntax.Let (x, t, adt_check_exp e1, adt_check_exp body)
      | Message sps ->
          ADTCheckedSyntax.Message (List.map sps ~f:(fun (s, p) -> (s, adt_check_payload p)))
      | Fun (f, t, body) ->
          ADTCheckedSyntax.Fun (f, t, adt_check_exp body)
      | App (f, args) ->
          ADTCheckedSyntax.App (f, args)
      | Constr (s, ts, args) ->
          ADTCheckedSyntax.Constr (s, ts, args)
      | MatchExpr (x, pes) ->
          ADTCheckedSyntax.MatchExpr (x, List.map pes ~f:(fun (p, e) -> (adt_check_pattern p, adt_check_exp e)))
      | Builtin (f, args) ->
          ADTCheckedSyntax.Builtin (f, args)
      | TFun (s, e) ->
          ADTCheckedSyntax.TFun (s, adt_check_exp e)
      | TApp (f, ts) ->
          ADTCheckedSyntax.TApp (f, ts)
      | Fixpoint (x, t, e) ->
          ADTCheckedSyntax.Fixpoint (x, t, adt_check_exp e) in
    (new_e, rep)
  
  let adt_check_library lib =
    let open Caml in
    let declared_adts : ((string, unit) Hashtbl.t) = Hashtbl.create 5 in
    let is_adt_type_valid adt_name =
      (* Check if type has already been declared *)
      match Hashtbl.find_opt declared_adts adt_name with
      | Some _ -> pure @@ ()
      | None ->
          (* Not declared. Check if type exists as builtin ADT *)
          let%bind _ = DataTypeDictionary.lookup_name adt_name in
          pure @@ () in
    let validate_constructor_arg t error_loc =
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
      walk t in
    let check_entry (lib_entry : lib_entry) =
      match lib_entry with
      | LibVar (n, e) -> pure @@ ADTCheckedSyntax.LibVar (n, adt_check_exp e)
      | LibTyp (tname, ctr_defs) ->
          let%bind checked_ctr_defs =
            mapM ~f:(fun ({ cname ; c_arg_types } : ctr_def) ->
                let error_loc = ER.get_loc (get_rep cname) in
                let%bind _ = forallM ~f:(fun c_arg -> validate_constructor_arg c_arg error_loc) c_arg_types in
                pure @@ { ADTCheckedSyntax.cname ; ADTCheckedSyntax.c_arg_types }) ctr_defs in
          (* Add type to ADTs in scope once checked. Adding the type after checking prevents inductive definitions. *)
          let _ = Hashtbl.add declared_adts (get_id tname) in
          pure @@ ADTCheckedSyntax.LibTyp (tname, checked_ctr_defs) in
    mapM ~f:check_entry lib.lentries
    
  let adt_check_module
      (md : cmodule)
      (recursion_principles : lib_entry list)
      (ext_libs : library list)
    : (ADTCheckedSyntax.cmodule * ADTCheckedSyntax.lib_entry list * ADTCheckedSyntax.library list, scilla_error list) result =
    let { smver ; cname ; libs ; elibs ; contr } = md in
    let%bind checked_elibs = mapM ~f:adt_check_library ext_libs in
    let%bind checked_md_libs =
      match libs with
      | None -> pure @@ None
      | Some l ->
          let%bind checked_l = adt_check_library l in
          pure @@ Some l in
    pure @@ (
      { ADTCheckedSyntax.smver = smver ;
        ADTCheckedSyntax.cname = cname ;
        ADTCheckedSyntax.libs = checked_md_libs ;
        ADTCheckedSyntax.elibs = elibs ;
        ADTCheckedSyntax.contr = contr },
      recursion_principles,
      checked_elibs)

  (**************************************************************)
  (*                    Staging API                             *)
  (**************************************************************)

  module OutputSyntax = ADTCheckedSyntax
  module OutputSRep = SADTR
  module OutputERep = EADTRR

end
