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

open Scilla_base
open Syntax
open TypeUtil

module ScillaProduct
    (SR : Rep) (ER : sig
      include Rep

      val get_type : rep -> PlainTypes.t inferred_type
    end) : sig
  module PSyntax : sig
    type cmodule
    type lib_entry
  end

  open PSyntax

  val run : (cmodule * lib_entry list) list -> (cmodule * lib_entry list) option
  (** Merges the given lists of modules and imported libraries to a
      single "product" module and set of libraries. *)
end
