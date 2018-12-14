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
    
(*****************************************************************)
(*               Checking user-defined ADTs                      *)
(*****************************************************************)

module ScillaADTChecker
  (SR : Rep)
  (ER : Rep) = struct

  module ADTCheckedSyntax = ScillaSyntax (SR) (ER)
  include ADTCheckedSyntax
  include ER
  
  open ADTCheckedSyntax
      
  let wrap_type_err e ?opt:(opt = "") = wrap_err e "ADT checking" ~opt:opt
  let wrap_type_serr s ?opt:(opt = "") = wrap_serr s "ADT checking" ~opt:opt
      
  (**************************************************************)
  (*                  Checking libraries                        *)
  (**************************************************************)
      
  let adt_check_library lib =
    let open Caml in
    let adts_in_scope : ((string, unit) Hashtbl.t) = Hashtbl.create 5 in
    let validate_constructor_arg t error_loc =
      let rec walk t =
        match t with
        | PrimType _
        | Unit -> pure @@ true
        | MapType (t1, t2)
        | FunType (t1, t2) ->
            let%bind _ = walk t1 in
            walk t2
        | ADT (s, targs) ->
            (* Only allow ADTs that are already in scope. This prevents mutually inductive definitions. *)
            (match Hashtbl.find_opt adts_in_scope s with
             | None -> fail1 (sprintf "Type %s not declared." s) error_loc
             | Some _ ->
                 forallM ~f:walk targs)
        | TypeVar _
        | PolyFun _ ->
            (* Disallow polymorphic definitions for the time being. *)
            fail1 "Type variables not allowed in type definitions" error_loc in
      walk t in
    let check_entry (lib_entry : lib_entry) =
      match lib_entry with
      | LibVar _ -> pure @@ true
      | LibTyp (tname, ctr_defs) ->
          let%bind _ =
            forallM ~f:(fun ({ cname ; c_arg_types } : ctr_def) ->
                let error_loc = ER.get_loc (get_rep cname) in
                forallM ~f:(fun c_arg -> validate_constructor_arg c_arg error_loc) c_arg_types) ctr_defs in
          (* Add type to ADTs in scope once checked. Adding the type after checking prevents inductive definitions. *)
          let _ = Hashtbl.add adts_in_scope (get_id tname) in
          pure @@ true in
    forallM ~f:check_entry lib.lentries
    
  let adt_check_module
      (md : cmodule)
      (_rec_libs : lib_entry list)
      (elibs : library list)
    : (bool, scilla_error list) result =
    let%bind _ =
      match md.libs with
      | None -> pure @@ true
      | Some l -> adt_check_library l in
    forallM ~f:adt_check_library elibs

  (**************************************************************)
  (*                    Staging API                             *)
  (**************************************************************)

  module OutputSyntax = ADTCheckedSyntax
  module OutputSRep = SR
  module OutputERep = ER

end
