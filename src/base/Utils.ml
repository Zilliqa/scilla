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

open Core_kernel

(* Add item a to list if it isn't already present. Use ~equal to check presence. *)
let list_add_unique ~equal ls a = if List.mem ls a ~equal then ls else a :: ls

(* Fold n times, each time applying 0-(n-1) and accummulator to f. *)
let int_fold ~init ~(f : 'a -> int -> 'a) n =
  let rec recurser acc i =
    if i = n then acc
    else
      let acc' = f acc i in
      recurser acc' (i + 1)
  in
  recurser init 0
