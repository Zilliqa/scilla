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

open Secp256k1
open Core.Result
open Core.Result.Let_syntax
open MonadUtil

let ctx = Context.create [ Sign ; Verify ]

let privkey_len = 32
let pubkey_len = 33
let signature_len = 64

(* Hash the message and return result raw string *)
let prepare_message m =
  Cryptokit.(hash_string (Hash.sha2 256) m)

(* raw byte string -> buffer *)
let buffer_of_raw s =
  let { Cstruct.buffer; _ } = Hex.to_cstruct (Hex.of_string s) in
  buffer

(* buffer -> raw byte string *)
let raw_of_buffer b =
  let cs = Cstruct.of_bigarray b in
  Hex.to_string (Hex.of_cstruct cs)

let resconv r =
  match r with
  | Ok o -> Ok o
  | Error s -> fail0 s

let resopt m =
  match m with
  | Some m' -> pure m'
  | None -> fail0 "ECDSA: Failed to handle message"

let pk_from_sk sk =
  let sk' = buffer_of_raw sk in
  let%bind sk'' = resconv @@ Key.read_sk ctx sk' in
  let%bind pk = resopt @@ Key.neuterize ctx sk'' in
  let pk' = Key.to_bytes ~compress:true ctx pk in
  pure @@ raw_of_buffer pk'

let sign sk msg =
  let sk' = buffer_of_raw sk in
  let msg' = buffer_of_raw (prepare_message msg) in
  let%bind sk'' = resconv @@ Key.read_sk ctx sk' in
  let%bind msg'' = resopt @@ Sign.msg_of_bytes msg' in
  let%bind signature = resconv @@ Sign.sign ctx ~sk:sk'' ~msg:msg'' in
  let signature' = Sign.to_bytes ctx signature in
  pure @@ raw_of_buffer signature'

let verify pk msg signature =
  let pk' = buffer_of_raw pk in
  let%bind pk'' = resconv @@ Key.read_pk ctx pk' in
  let msg' = buffer_of_raw (prepare_message msg) in
  let%bind msg'' = resopt @@ Sign.msg_of_bytes msg' in
  let signature' = buffer_of_raw signature in
  let%bind signature'' = resconv @@ Sign.read ctx signature' in
  resconv @@ Sign.verify ctx ~pk:pk'' ~msg:msg'' ~signature:signature''
