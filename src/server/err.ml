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

type t = {
  code : int;
  message : string;
} [@@deriving rpcty]

exception ServerError of t

let rpc_err = Error.{
  def = t;
  raiser = (function | e -> raise (ServerError e));
  matcher = function | ServerError e -> Some e | _ -> None
}

type reason =
  | InvalidQuery of string
  [@@deriving show]

let mk = function
  | InvalidQuery msg -> { code = 1; message = "Invalid query: " ^ msg }

let invalid_query msg = mk (InvalidQuery msg)
