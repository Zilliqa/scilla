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

(* Force link external C++ code: https://github.com/ocamllabs/ocaml-ctypes/issues/541 *)
external _force_link_ : unit -> unit = "genKeyPair_Z"

open Ctypes
open Foreign
open CFFICommon

let privkey_len = 32
let pubkey_len = 33
let signature_len = 64

let genKeyPair () =

  (* bool genKeyPair_Z(RawBytes_Z* privKey, RawBytes_Z* pubKey); *)
  let genKeyPair_Z = foreign "genKeyPair_Z" (ptr rawBytes_Z @-> ptr rawBytes_Z @-> returning bool) in

  (* Allocate buffers *)
  let dataPrivKey = allocate_n char ~count:privkey_len in
  let dataPubKey = allocate_n char ~count:pubkey_len in
  (* create container struct objects *)
  let privK = make rawBytes_Z in
  let pubK = make rawBytes_Z in
  (* and fill the fields. *)
  let _ = setf privK rawBytes_data dataPrivKey in
  let _ = setf pubK rawBytes_data dataPubKey in
  let _ = setf privK rawBytes_len privkey_len in
  let _ = setf pubK rawBytes_len pubkey_len in
  (* Call the C function to generate a key pair. *)
  if not (genKeyPair_Z (addr privK) (addr pubK)) then None else
  (* Read the keys into OCaml strings. *)
  let privK' = copy_from_tstring @@ string_from_ptr dataPrivKey ~length:privkey_len in
  let pubK' = copy_from_tstring @@ string_from_ptr dataPubKey ~length:pubkey_len in
  (* Dummy use to avoid GC of memory. *)
  let _ = dataPrivKey, dataPubKey, privK, pubK in
  Some (privK', pubK')

(* privKey, pubKey and msg are raw bytes. *)
let sign privKey pubKey msg =

  (* bool sign_Z(const RawBytes_Z* privKey, const RawBytes_Z* pubKey,
   *             const RawBytes_Z* message, RawBytes_Z* signature)
   *)
  let sign_Z = foreign "sign_Z" (ptr rawBytes_Z @-> ptr rawBytes_Z @-> ptr rawBytes_Z @-> ptr rawBytes_Z @-> returning bool) in


  (* Create container for Schnorr inputs *)
  let privKS = make rawBytes_Z in
  let pubKS = make rawBytes_Z in
  let msgS = make rawBytes_Z in
  let signS = make rawBytes_Z in
  (* and allocate data *)
  let privKD = allocate_n char ~count:privkey_len in
  let pubKD = allocate_n char ~count:pubkey_len in
  let msgD = allocate_n char ~count:(String.length msg) in
  let signD = allocate_n char ~count:signature_len in
  (* and fill the fields. *)
  let _ = setf privKS rawBytes_data privKD in
  let _ = setf privKS rawBytes_len privkey_len in
  let _ = setf pubKS rawBytes_data pubKD in
  let _ = setf pubKS rawBytes_len pubkey_len in
  let _ = setf msgS rawBytes_data msgD in
  let _ = setf msgS rawBytes_len (String.length msg) in
  let _ = setf signS rawBytes_data signD in
  let _ = setf signS rawBytes_len signature_len in
  (* Copy input data to input structs. *)
  let _ = copy_to_cptr privKD privKey in
  let _ = copy_to_cptr pubKD pubKey in
  let _ = copy_to_cptr msgD msg in
  (* Call the signing C function. *)
  if not (sign_Z (addr privKS) (addr pubKS) (addr msgS) (addr signS)) then None else 
  (* Copy back the signature. *)
  let signS' = copy_from_tstring @@ string_from_ptr signD ~length:signature_len in
  (* Dummy use to avoid GC of memory. *)
  let _ = privKS, privKD, pubKS, pubKD, msgS, msgD, signS, signD in
  Some signS'

(* pubKey, signature and msg are raw bytes. *)
let verify pubKey msg signature =

  (* bool verify_Z(const RawBytes_Z* pubKey, const RawBytes_Z* message,
   *            RawBytes_Z* signature, int *result);
   *)
  let verify_Z = foreign "verify_Z" (ptr rawBytes_Z @-> ptr rawBytes_Z @-> ptr rawBytes_Z @-> ptr int @-> returning bool) in

  (* Create container for Schnorr inputs *)
  let pubKS = make rawBytes_Z in
  let msgS = make rawBytes_Z in
  let signS = make rawBytes_Z in
  (* and allocate data *)
  let pubKD = allocate_n char ~count:pubkey_len in
  let msgD = allocate_n char ~count:(String.length msg) in
  let signD = allocate_n char ~count:signature_len in
  let resD = allocate int 0 in
  (* and fill the fields. *)
  let _ = setf pubKS rawBytes_data pubKD in
  let _ = setf pubKS rawBytes_len pubkey_len in
  let _ = setf msgS rawBytes_data msgD in
  let _ = setf msgS rawBytes_len (String.length msg) in
  let _ = setf signS rawBytes_data signD in
  let _ = setf signS rawBytes_len signature_len in
  (* Copy input data to input structs. *)
  let _ = copy_to_cptr pubKD pubKey in
  let _ = copy_to_cptr msgD msg in
  let _ = copy_to_cptr signD signature in
  (* Call the signing C function. *)
  if not (verify_Z (addr pubKS) (addr msgS) (addr signS) resD) then None else
  (* Dummy use to avoid GC of memory. *)
  let _ = pubKD, msgD, signD, pubKS, msgS, signS, resD in
  Some ((!@resD) = 1)
