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

open Idl

(** Server error *)
type t = {
  code : int;
  message : string;
}

(** Server exception *)
exception ServerError of t

(** Type of the RPC error,
    needed by the ocaml-rpc lib *)
val rpc_err : t Error.t

(** Error reason *)
type reason =
  | InvalidQuery of string
  [@@deriving show]

(** Makes an error out of [reason] *)
val mk : reason -> t

(** Makes an "invalid query" error out
    of the given descrption [string] *)
val invalid_query : string -> t
