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

type scalar = string (* TODO: Replace this with something like ByStr32 *)

type g1point = { g1x : scalar; g1y : scalar }

type g2point = { g2x : string; g2y : string }

(* TODO: Replace with (ByStr64,ByStr64) *)

let scalar_len = 32

let g1point_len = scalar_len + scalar_len

let g2comp_len = 64

let g2point_len = g2comp_len + g2comp_len

let g1g2pair_len = g1point_len + g2point_len

(* each pair in alt_bn128_pairing_product *)

let encode_g1point_bytes g1 = g1.g1x ^ g1.g1y

let decode_g1point_bytes g1b =
  if String.length g1b <> g1point_len then None
  else
    Some
      {
        g1x = String.sub g1b 0 scalar_len;
        g1y = String.sub g1b scalar_len scalar_len;
      }

let encode_g2point_bytes g2 = g2.g2x ^ g2.g2y

let encode_g1g2pair_bytes g1p g2p =
  encode_g1point_bytes g1p ^ encode_g2point_bytes g2p

let alt_bn128_G1_add p1 p2 =
  (* This check can be removed once we have a strong type for scalar. *)
  if
    String.length p1.g1x <> scalar_len
    || String.length p1.g1y <> scalar_len
    || String.length p2.g1x <> scalar_len
    || String.length p2.g1y <> scalar_len
  then None
  else
    (* bool alt_bn128_G1_add_Z(const RawBytes_Z* p1, const RawBytes_Z* p2, RawBytes_Z* result) *)
    let alt_bn128_G1_add_Z =
      foreign "alt_bn128_G1_add_Z"
        (ptr rawBytes_Z @-> ptr rawBytes_Z @-> ptr rawBytes_Z @-> returning bool)
    in

    (* Create container for inputs and output. *)
    let p1S = make rawBytes_Z in
    let p2S = make rawBytes_Z in
    let presS = make rawBytes_Z in
    (* and allocate data *)
    let p1D = allocate_n char ~count:g1point_len in
    let p2D = allocate_n char ~count:g1point_len in
    let presD = allocate_n char ~count:g1point_len in
    (* and fill the fields. *)
    let _ = setf p1S rawBytes_data p1D in
    let _ = setf p1S rawBytes_len g1point_len in
    let _ = setf p2S rawBytes_data p2D in
    let _ = setf p2S rawBytes_len g1point_len in
    let _ = setf presS rawBytes_data presD in
    let _ = setf presS rawBytes_len g1point_len in
    (* Copy input data to input structs. *)
    let _ = copy_to_cptr p1D (encode_g1point_bytes p1) in
    let _ = copy_to_cptr p2D (encode_g1point_bytes p2) in
    (* Call the C function. *)
    let succ = alt_bn128_G1_add_Z (addr p1S) (addr p2S) (addr presS) in
    (* Copy back the result. *)
    let pres =
      if succ then
        decode_g1point_bytes
          (copy_from_tstring (string_from_ptr presD ~length:g1point_len))
      else None
    in
    (* Dummy use to avoid GC of memory. *)
    let _ = (p1S, p1D, p2S, p2D, presS, presD) in
    pres

let alt_bn128_G1_mul p s =
  (* This check can be removed once we have a strong type for scalar. *)
  if
    String.length p.g1x <> scalar_len
    || String.length p.g1y <> scalar_len
    || String.length s <> scalar_len
  then None
  else
    (* bool alt_bn128_G1_mul_Z(const RawBytes_Z* p, const RawBytes_Z* s, RawBytes_Z* result) *)
    let alt_bn128_G1_mul_Z =
      foreign "alt_bn128_G1_mul_Z"
        (ptr rawBytes_Z @-> ptr rawBytes_Z @-> ptr rawBytes_Z @-> returning bool)
    in

    (* Create container for inputs and output. *)
    let pS = make rawBytes_Z in
    let sS = make rawBytes_Z in
    let presS = make rawBytes_Z in
    (* and allocate data *)
    let pD = allocate_n char ~count:g1point_len in
    let sD = allocate_n char ~count:scalar_len in
    let presD = allocate_n char ~count:g1point_len in
    (* and fill the fields. *)
    let _ = setf pS rawBytes_data pD in
    let _ = setf pS rawBytes_len g1point_len in
    let _ = setf sS rawBytes_data sD in
    let _ = setf sS rawBytes_len scalar_len in
    let _ = setf presS rawBytes_data presD in
    let _ = setf presS rawBytes_len g1point_len in
    (* Copy input data to input structs. *)
    let _ = copy_to_cptr pD (encode_g1point_bytes p) in
    let _ = copy_to_cptr sD s in
    (* Call the C function. *)
    let succ = alt_bn128_G1_mul_Z (addr pS) (addr sS) (addr presS) in
    let pres =
      if succ (* Copy back the result. *) then
        decode_g1point_bytes
          (copy_from_tstring (string_from_ptr presD ~length:g1point_len))
      else None
    in
    (* Dummy use to avoid GC of memory. *)
    let _ = (pS, pD, sS, sD, presS, presD) in
    pres

let alt_bn128_pairing_product pairs =
  (* This check can be removed once we have a strong type for g2point. *)
  if
    List.exists
      (fun (g1p, g2p) ->
        String.length g1p.g1x <> scalar_len
        || String.length g1p.g1y <> scalar_len
        || String.length g2p.g2x <> g2comp_len
        || String.length g2p.g2y <> g2comp_len)
      pairs
  then None
  else
    (* bool alt_bn128_pairing_product_Z(const RawBytes_Z* p, RawBytes_Z* result) *)
    let alt_bn128_pairing_product_Z =
      foreign "alt_bn128_pairing_product_Z"
        (ptr rawBytes_Z @-> ptr rawBytes_Z @-> returning bool)
    in

    (* Create container for inputs and output. *)
    let pS = make rawBytes_Z in
    let presS = make rawBytes_Z in
    (* and allocate data *)
    let data_size = g1g2pair_len * List.length pairs in
    let pD = allocate_n char ~count:data_size in
    let presD = allocate_n char ~count:scalar_len in
    (* and fill the fields. *)
    let _ = setf pS rawBytes_data pD in
    let _ = setf pS rawBytes_len data_size in
    let _ = setf presS rawBytes_data presD in
    let _ = setf presS rawBytes_len scalar_len in
    (* Copy input data to input structs. *)
    let pairs' =
      String.concat ""
        (List.map (fun (g1p, g2p) -> encode_g1g2pair_bytes g1p g2p) pairs)
    in
    let _ = copy_to_cptr pD pairs' in
    (* Call the C function. *)
    let succ = alt_bn128_pairing_product_Z (addr pS) (addr presS) in
    let pres =
      if succ then
        (* Copy back the result. *)
        let res =
          copy_from_tstring @@ string_from_ptr presD ~length:scalar_len
        in
        let zero_string =
          let b = Bytes.init scalar_len (fun _ -> '\000') in
          Bytes.to_string b
        in
        Some (res <> zero_string)
      else None
    in
    (* Dummy use to avoid GC of memory. *)
    let _ = (pS, pD, presS, presD) in
    pres

let eq_scalar = ( = )

(* Replace with proper operator once we have a string type. *)

let eq_g1 p1 p2 = p1.g1x = p2.g1x && p1.g1y = p2.g1y

let eq_g2 p1 p2 = p1.g2x = p2.g2x && p1.g2y = p2.g2y
