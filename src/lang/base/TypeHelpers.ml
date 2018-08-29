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

open PrimTypes
open TypeUtil
open Syntax

(*******************************************************)
(*                   Annotations                       *)
(*******************************************************)

module TypecheckerERep (R : Rep) = struct
  type rep = PlainTypes.t inferred_type * R.rep
  [@@deriving sexp]
 
  let get_loc r = match r with | (_, rr) -> R.get_loc rr

  let mk_msg_payload_id s t =
    match s with
    | Ident (n, r) -> Ident (n, (PlainTypes.mk_qualified_type t, r))

  let mk_msg_payload_id_address s = mk_msg_payload_id (R.mk_msg_payload_id_address s) (bystr_typ address_length)
  let mk_msg_payload_id_uint128 s = mk_msg_payload_id (R.mk_msg_payload_id_uint128 s) uint128_typ
  
  let mk_rep (r : R.rep) (t : PlainTypes.t inferred_type) = (t, r)
  
  let parse_rep s = (PlainTypes.mk_qualified_type uint128_typ, R.parse_rep s)
  let get_rep_str r = match r with | (_, rr) -> R.get_rep_str rr

  let get_type (r : rep) = fst r
end

