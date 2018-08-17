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
open PrimTypes

(*******************************************************)
(*                   Annotations                       *)
(*******************************************************)

module TypecheckerERep (R : Rep) = struct
  type rep = typ * R.rep

  let get_loc r = match r with | (_, rr) -> R.get_loc rr
  let mk_msg_payload_id s =
    match R.mk_msg_payload_id s with
    | Ident (n, r) -> Ident (n, (uint128_typ, r))


  let parse_rep s = (uint128_typ, R.parse_rep s)
  let get_rep_str r = match r with | (_, rr) -> R.get_rep_str rr
end

