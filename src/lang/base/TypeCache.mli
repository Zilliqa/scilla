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

(*****************************************************************)
(*                    Library type caching                       *)
(*****************************************************************)

open Syntax
open TypeUtil

module StdlibTypeCacher
    (Q : MakeTEnvFunctor)
    (R : QualifiedTypes)
    (SR : Rep)
    (ER : Rep) : sig

  module L :
  sig
    type expr_annot
    type ctr_def
    type lib_entry =
      | LibVar of ER.rep ident * typ option * expr_annot
      | LibTyp of ER.rep ident * ctr_def list
    type library = { lname : SR.rep ident; lentries : lib_entry list }
  end
    
  type t = Q(R)(ER).TEnv.t

  (* Get type info for "lib" from cache, if it exists. *)
  val get_lib_tenv_cache : t -> L.library -> t option
  (* Store type info tenv, for "lib" in the cache. *)
  val cache_lib_tenv : t -> L.library -> unit
end
