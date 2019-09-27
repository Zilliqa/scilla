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

open Ctypes
open Foreign
open CFFICommon

(* Force link external C++ code: https://github.com/ocamllabs/ocaml-ctypes/issues/541 *)
external _force_link_ : unit -> unit = "alt_bn128_G1_add_Z"

let scalar_len = 32
let point_len = 64
let pair_len = 32 * 2 + 64 * 2 (* each pair in alt_bn128_pairing_product *)

(* privKey, pubKey and msg are raw bytes. *)
let alt_bn128_G1_add p1 p2 =

  (* void alt_bn128_G1_add_Z(const RawBytes_Z* p1, const RawBytes_Z* p2, RawBytes_Z* result) *)
  let alt_bn128_G1_add_Z =
    foreign "alt_bn128_G1_add_Z"
      (ptr rawBytes_Z @-> ptr rawBytes_Z @-> ptr rawBytes_Z @-> returning void) in

  (* Create container for inputs and output. *)
  let p1S = make rawBytes_Z in
  let p2S = make rawBytes_Z in
  let presS = make rawBytes_Z in
  (* and allocate data *)
  let p1D = allocate_n char ~count:point_len in
  let p2D = allocate_n char ~count:point_len in
  let presD = allocate_n char ~count:point_len in
  (* and fill the fields. *)
  let _ = setf p1S rawBytes_data p1D in
  let _ = setf p1S rawBytes_len point_len in
  let _ = setf p2S rawBytes_data p2D in
  let _ = setf p2S rawBytes_len point_len in
  let _ = setf presS rawBytes_data presD in
  let _ = setf presS rawBytes_len point_len in
  (* Copy input data to input structs. *)
  let _ = copy_to_cptr p1D p1 in
  let _ = copy_to_cptr p2D p2 in
  (* Call the signing C function. *)
  let _ = alt_bn128_G1_add_Z (addr p1S) (addr p2S) (addr presS) in
  (* Copy back the signature. *)
  let pres = copy_from_tstring @@ string_from_ptr presD ~length:point_len in
  (* Dummy use to avoid GC of memory. *)
  let _ = p1S, p1D, p2S, p2D, presS, presD in
  pres
