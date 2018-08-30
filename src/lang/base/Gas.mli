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

module ScillaGas
    (SR : Rep)
    (ER : Rep) : sig

(* The cost of storing a literal: propotional to it's size. *)
val literal_cost : literal -> (int, string) result

(* Cost of built-in operations, given the built-in name
   and the arguments (during eval) to the built-in. *)
val builtin_cost : loc ident -> literal list -> (int, string) result

end