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

let alt_bn128_G1_add p1 p2 =

  if String.length p1 <> point_len || String.length p2 <> point_len then None else

  (* bool alt_bn128_G1_add_Z(const RawBytes_Z* p1, const RawBytes_Z* p2, RawBytes_Z* result) *)
  let alt_bn128_G1_add_Z =
    foreign "alt_bn128_G1_add_Z"
      (ptr rawBytes_Z @-> ptr rawBytes_Z @-> ptr rawBytes_Z @-> returning bool) in

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
  (* Call the C function. *)
  let succ = alt_bn128_G1_add_Z (addr p1S) (addr p2S) (addr presS) in
  (* Copy back the result. *)
  let pres =
    if succ
    then Some (copy_from_tstring @@ string_from_ptr presD ~length:point_len)
    else None
  in
  (* Dummy use to avoid GC of memory. *)
  let _ = p1S, p1D, p2S, p2D, presS, presD in
  pres

let alt_bn128_G1_mul p s =

  if String.length p <> point_len || String.length s <> scalar_len then None else

  (* bool alt_bn128_G1_mul_Z(const RawBytes_Z* p, const RawBytes_Z* s, RawBytes_Z* result) *)
  let alt_bn128_G1_mul_Z =
    foreign "alt_bn128_G1_mul_Z"
      (ptr rawBytes_Z @-> ptr rawBytes_Z @-> ptr rawBytes_Z @-> returning bool) in

  (* Create container for inputs and output. *)
  let pS = make rawBytes_Z in
  let sS = make rawBytes_Z in
  let presS = make rawBytes_Z in
  (* and allocate data *)
  let pD = allocate_n char ~count:point_len in
  let sD = allocate_n char ~count:scalar_len in
  let presD = allocate_n char ~count:point_len in
  (* and fill the fields. *)
  let _ = setf pS rawBytes_data pD in
  let _ = setf pS rawBytes_len point_len in
  let _ = setf sS rawBytes_data sD in
  let _ = setf sS rawBytes_len scalar_len in
  let _ = setf presS rawBytes_data presD in
  let _ = setf presS rawBytes_len point_len in
  (* Copy input data to input structs. *)
  let _ = copy_to_cptr pD p in
  let _ = copy_to_cptr sD s in
  (* Call the C function. *)
  let succ = alt_bn128_G1_mul_Z (addr pS) (addr sS) (addr presS) in
  let pres =
    if succ
    (* Copy back the result. *)
    then Some (copy_from_tstring @@ string_from_ptr presD ~length:point_len)
    else None
  in
  (* Dummy use to avoid GC of memory. *)
  let _ = pS, pD, sS, sD, presS, presD in
  pres
