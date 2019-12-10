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


exception IntOverflow
exception IntUnderflow

module type IntRep =
  sig
    type t
    val compare : t -> t -> Base.int
    val add : t -> t -> t
    val sub : t -> t -> t
    val mul : t -> t -> t
    val div : t -> t -> t
    val rem : t -> t -> t
    val zero : t
    val one : t
    val min_int : t
  end

(* Integer arithmitic
   with bounds checking for overflows and underflows *)
module SafeInt :
  functor (Unsafe : IntRep) ->
    sig
      val add : Unsafe.t -> Unsafe.t -> Unsafe.t
      val sub : Unsafe.t -> Unsafe.t -> Unsafe.t
      val mul : Unsafe.t -> Unsafe.t -> Unsafe.t
      val div : Unsafe.t -> Unsafe.t -> Unsafe.t
      val rem : Unsafe.t -> Unsafe.t -> Unsafe.t
      val pow : Unsafe.t -> Stdint.Uint32.t -> Unsafe.t
      val lt : Unsafe.t -> Unsafe.t -> bool
    end

(* Unsigned integer arithmitic
   with bounds checkingfor overflows and underflows *)
module SafeUint :
  functor (Unsafe : IntRep) ->
    sig
      val add : Unsafe.t -> Unsafe.t -> Unsafe.t
      val sub : Unsafe.t -> Unsafe.t -> Unsafe.t
      val mul : Unsafe.t -> Unsafe.t -> Unsafe.t
      val div : Unsafe.t -> Unsafe.t -> Unsafe.t
      val rem : Unsafe.t -> Unsafe.t -> Unsafe.t
      val pow : Unsafe.t -> Stdint.Uint32.t -> Unsafe.t
      val lt : Unsafe.t -> Unsafe.t -> bool
    end
