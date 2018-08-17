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
open TypeUtil


(*****************************************************************)
(*                    Transition typing                          *)
(*****************************************************************)

(*******************************************************)
(*                   Annotations                       *)
(*******************************************************)

module TypecheckerERep (R : Rep) = struct
  type rep = R.rep inferred_type * R.rep
end

(*****************************************************************)
(*                 Typing entire contracts                       *)
(*****************************************************************)

module Typechecker_Contracts
    (SR : Rep)
    (ER : Rep) = struct

  module STR = SR
  module ETR = TypecheckerERep(ER)
  module UntypedContract = Contract (SR) (ER)
  module TypedContract = Contract (STR) (ETR)

  include TypedContract
      
end

(* TODO: This doesn't feel right *)
open ParserUtil
module TypedContracts = Typechecker_Contracts (ParserRep) (ParserRep)
