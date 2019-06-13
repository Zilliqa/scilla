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

type ioFiles = {
    input_init : string;
    input_state : string;
    input_message : string;
    input_blockchain : string;
    output : string;
    input : string;
    libdirs : string list;
    gas_limit : Stdint.uint64;
    pp_json : bool;
    ipc_port : int;
}

val parse : unit -> ioFiles
