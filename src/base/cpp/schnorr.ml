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

(* Copy the contents of string s to pointer p.
 * This does the opposite of `string_from_ptr`.
 *)
let copy_to_cptr p s =
  let f i c =
    (p +@ i) <-@ c
  in
    String.iteri f s

(* Copy the contents of "temporary" string s
 * to a new string. "temporary" strings are those
 * whose underlying memory can be freed "anytime".
 * This arises when the string is created from `string_from_ptr`.
 *)
let copy_from_tstring s =
  let f i =
    s.[i]
  in
  String.init (String.length s) f

let privkey_len = 32
let pubkey_len = 33
let signature_len = 64

(*
 *  typedef struct
 *  {
 *    char* data;
 *    int len;
 *  } RawBytes_Z;
 *)
type rawBytes_Z
let rawBytes_Z : rawBytes_Z structure typ = structure "rawBytes_Z"
let rawBytes_data  = field rawBytes_Z "data" (ptr char)
let rawBytes_len = field rawBytes_Z "len" (int)
let () = seal rawBytes_Z

let genKeyPair () =

  (* void genKeyPair_Z(RawBytes_Z* privKey, RawBytes_Z* pubKey); *)
  let genKeyPair_Z = foreign "genKeyPair_Z" (ptr rawBytes_Z @-> ptr rawBytes_Z @-> returning void) in

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
  let _ = genKeyPair_Z (addr privK) (addr pubK) in
  (* Read the keys into OCaml strings. *)
  let privK' = copy_from_tstring @@ string_from_ptr dataPrivKey ~length:privkey_len in
  let pubK' = copy_from_tstring @@ string_from_ptr dataPubKey ~length:pubkey_len in
  (* Dummy use to avoid GC of memory. *)
  let _ = dataPrivKey, dataPubKey, privK, pubK in
  (privK', pubK')

(* privKey, pubKey and msg are raw bytes. *)
let sign privKey pubKey msg =

  (* void sign_Z(const RawBytes_Z* privKey, const RawBytes_Z* pubKey,
   *             const RawBytes_Z* message, RawBytes_Z* signature)
   *)
  let sign_Z = foreign "sign_Z" (ptr rawBytes_Z @-> ptr rawBytes_Z @-> ptr rawBytes_Z @-> ptr rawBytes_Z @-> returning void) in


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
  let _ = sign_Z (addr privKS) (addr pubKS) (addr msgS) (addr signS) in
  (* Copy back the signature. *)
  let signS' = copy_from_tstring @@ string_from_ptr signD ~length:signature_len in
  (* Dummy use to avoid GC of memory. *)
  let _ = privKS, privKD, pubKS, pubKD, msgS, msgD, signS, signD in
  signS'

(* pubKey, signature and msg are raw bytes. *)
let verify pubKey msg signature =

  (* int verify_Z(const RawBytes_Z* pubKey, const RawBytes_Z* message,
   *            RawBytes_Z* signature);
   *)
  let verify_Z = foreign "verify_Z" (ptr rawBytes_Z @-> ptr rawBytes_Z @-> ptr rawBytes_Z @-> returning int) in

  (* Create container for Schnorr inputs *)
  let pubKS = make rawBytes_Z in
  let msgS = make rawBytes_Z in
  let signS = make rawBytes_Z in
  (* and allocate data *)
  let pubKD = allocate_n char ~count:pubkey_len in
  let msgD = allocate_n char ~count:(String.length msg) in
  let signD = allocate_n char ~count:signature_len in
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
  let succ = verify_Z (addr pubKS) (addr msgS) (addr signS) in
  (* Dummy use to avoid GC of memory. *)
  let _ = pubKD, msgD, signD, pubKS, msgS, signS in
  succ = 1
