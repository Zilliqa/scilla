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
open Core_kernel
open Result
open OUnit2
open Scilla_base
open Scilla_crypto

let t1 =
  test_case (fun _ ->
      let open Schnorr in
      try
        let privK, pubK = Option.value_exn (genKeyPair ()) in
        let msg = "Hello world\n" in
        let signature = Option.value_exn (sign privK pubK msg) in
        let succ = Option.value_exn (verify pubK msg signature) in
        assert_bool "Signature verification failed" succ
      with (* Check if Option.value_exn() failed. *)
      | Invalid_argument _ ->
        assert_failure "Schnorr function errored when called from testsuite")

let t2 =
  test_case (fun _ ->
      let open Schnorr in
      try
        let privK, pubK = Option.value_exn (genKeyPair ()) in
        let msg = "Hello world\n" in
        let signature = Option.value_exn (sign privK pubK msg) in
        let succ = Option.value_exn (verify pubK (msg ^ "\n") signature) in
        assert_bool "Signature incorrectly verified" (not succ)
      with (* Check if Option.value_exn () failed. *)
      | Invalid_argument _ ->
        assert_failure "Schnorr function errored when called from testsuite")

let schnorr_tests = "schnorr_tests" >::: [ t1; t2 ]

let t1' =
  test_case (fun _ ->
      let open Secp256k1Wrapper in
      match Schnorr.genKeyPair () with
      | None ->
          assert_failure "Schnorr function errored when called from testsuite"
      | Some (privK, pubK) ->
          let msg = "Hello world\n" in
          (* Verify public key's match b/w implementations. *)
          let pubK' = pk_from_sk privK in
          let pubK'' = match pubK' with Ok pubK'' -> pubK'' | Error _ -> "" in
          assert_bool "Public key mis-match b/w Schnorr and ECDSA"
            String.(pubK = pubK'');
          let succ =
            match sign privK msg with
            | Ok signature -> (
                match verify pubK msg signature with
                | Ok succ -> succ
                | Error s ->
                    let s' = ErrorUtils.sprint_scilla_error_list s in
                    Printf.fprintf stderr "Error: %s\n" s';
                    false )
            | Error s ->
                let s' = ErrorUtils.sprint_scilla_error_list s in
                Printf.fprintf stderr "Error: %s\n" s';
                false
          in
          assert_bool "Signature verification failed" succ)

let t2' =
  test_case (fun _ ->
      let open Secp256k1Wrapper in
      match Schnorr.genKeyPair () with
      | None ->
          assert_failure "Schnorr function errored when called from testsuite"
      | Some (privK, pubK) ->
          let msg = "Hello world\n" in
          (* Verify public key's match b/w implementations. *)
          let pubK' = pk_from_sk privK in
          let pubK'' = match pubK' with Ok pubK'' -> pubK'' | Error _ -> "" in
          assert_bool "Public key mis-match b/w Schnorr and ECDSA"
            String.(pubK = pubK'');
          let succ =
            match sign privK msg with
            | Ok signature -> (
                match verify pubK (msg ^ "\n") signature with
                | Ok succ -> succ
                | Error s ->
                    let s' = ErrorUtils.sprint_scilla_error_list s in
                    Printf.fprintf stderr "Error: %s\n" s';
                    false )
            | Error s ->
                let s' = ErrorUtils.sprint_scilla_error_list s in
                Printf.fprintf stderr "Error: %s\n" s';
                false
          in
          assert_bool "Signature incorrectly verified" (not succ))

let ecrecover msg signature v =
  let keccak_256 m = Cryptokit.(hash_string (Hash.keccak 256) m) in
  let msg = keccak_256 msg in
  let msg = "\x19" ^ "Ethereum Signed Message:\n32" ^ msg in
  let msgHash = keccak_256 msg in
  match (Secp256k1Wrapper.recover_pk msgHash signature v) with
  | Ok pk' ->
    let pk = Caml.String.sub pk' 1 64 in
    let addrr = Caml.String.sub (keccak_256 pk) 12 20 in
    addrr
  | Error e -> assert_failure (ErrorUtils.sprint_scilla_error_list e)

let t3' =
  test_case (fun _ ->
      let hash_string m = Cryptokit.(hash_string (Hash.sha2 256) m) in
      let header_hash = Fn.compose hash_string hash_string in
      let open Secp256k1Wrapper in
      let open Literal.GlobalLiteral in
      let open Stdint in
      let header = header_hash @@ Bystr.to_raw_bytes @@ Bystr.parse_hex
        "0x000000009b91561700000000f48a4057bef268cc3fdb034e69dc2e942907e08ac4a420d1b196b8c28ebf5bf2000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002a8be0a1605a63a31704aec4eb4f1023f1ecc2934bd86f119ab77526f9477af9a57e1a5f508e0000410782720ab189fffd84057b226c6561646572223a332c227672665f76616c7565223a22424f4f336f58796b32524970655651593338547133714a423832737a4a68366e4f6f724a55702f4a4d582b474c707a347a497347394c4a6c34784a6f34657448674f56357169364d484b6674714f69724f755a495a69593d222c227672665f70726f6f66223a22635953525746506f69394748414247526255646836612b35506f4f317776354a557a53417457786845637071757430536a595873344c7453353574534a74334174493059616d4c67524a797a524f68564756626d34673d3d222c226c6173745f636f6e6669675f626c6f636b5f6e756d223a33363433322c226e65775f636861696e5f636f6e666967223a7b2276657273696f6e223a312c2276696577223a342c226e223a382c2263223a322c22626c6f636b5f6d73675f64656c6179223a31303030303030303030302c22686173685f6d73675f64656c6179223a31303030303030303030302c22706565725f68616e647368616b655f74696d656f7574223a31303030303030303030302c227065657273223a5b7b22696e646578223a312c226964223a2231323035303238313732393138353430623262353132656165313837326132613265336132386439383963363064393564616238383239616461376437646437303664363538227d2c7b22696e646578223a342c226964223a2231323035303236373939333061343261616633633639373938636138613366313265313334633031393430353831386437383364313137343865303339646538353135393838227d2c7b22696e646578223a332c226964223a2231323035303234383261636236353634623139623930363533663665396338303632393265386161383366373865376139333832613234613665666534316330633036663339227d2c7b22696e646578223a352c226964223a2231323035303234363864643138393965643264316363326238323938383261313635613065636236613734356166306337326562323938326436366234333131623465663733227d2c7b22696e646578223a382c226964223a2231323035303339333432313434356239343231626434636339306437626338386339333031353538303437613736623230633539653763353131656537643232393938326231227d2c7b22696e646578223a322c226964223a2231323035303338623861663632313065636664636263616232323535326566386438636634316336663836663963663961623533643836353734316366646238333366303662227d2c7b22696e646578223a372c226964223a2231323035303331653037373966356335636362323631323335326665346132303066393964336537373538653730626135336636303763353966663232613330663637386666227d2c7b22696e646578223a362c226964223a2231323035303265623162616162363032633538393932383235363163646161613761616262636464306363666362633365373937393361633234616366393037373866333561227d5d2c22706f735f7461626c65223a5b322c382c352c352c382c372c312c342c352c362c352c342c372c372c332c332c342c362c312c322c342c382c352c342c372c342c362c362c322c322c312c312c382c382c362c362c362c372c382c372c342c382c352c312c332c332c382c352c332c362c332c362c372c352c362c322c332c312c322c362c352c322c312c342c322c312c382c342c382c332c382c372c372c352c312c372c342c342c312c352c322c352c362c312c322c382c332c332c312c332c312c342c312c372c382c362c382c322c352c312c342c352c332c322c322c322c382c332c332c332c362c372c342c372c342c322c372c352c362c375d2c226d61785f626c6f636b5f6368616e67655f76696577223a36303030307d7df8fc7a1f6a856313c591a3a747f4eca7218a820b"
      in
      (* "0x000000009b91561700000000f2e3823838bcdfafe08c6e9ff2cd1f86ae65dfc1052d15c9441aa6c07097015f0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000051d45928d93800ffebc80373df7dbe735a2e9c5998b883e69494921fedbbe858057f1a5f5a8e0000216854be71737efbfd10017b226c6561646572223a362c227672665f76616c7565223a224244592f6c4875783448587341376f394962357877634a4d39727466656a454c786d6637766a48583574507a64724d356a47304a305265756d4338632f6e64725131306b4751622f50537a3255667a6267723430666a413d222c227672665f70726f6f66223a2242437877375a6d49446243476a3839576e715744564f6b714f7239645574344f4f2f777176366d4c3165656377474d542b3078664476544642614f3951534451437879725551344c316835392b3130486671386176673d3d222c226c6173745f636f6e6669675f626c6f636b5f6e756d223a33363433322c226e65775f636861696e5f636f6e666967223a6e756c6c7d0000000000000000000000000000000000000000" in *)
      let signatures = [
(*      "0x6d905d095fc1b47eb30b8f176c94b01b64502790f3c20db2a5b7e555791bdbf8458a24c73c8f690ad8481e3754c3297991f9f4148d03316e2e5a6c6af9e7bbc900";
        "0xa81911a6eb529979ee021124c242a828657865fc7a899ab55fb7d2a1623341a67ae83c90a40bec8bf54fa3c5caad445062568d3c4c4099e84ad6c774e70d7d7d00";
        "0x0b5d0a2ebc3685a6deeb96cfabf71e4a9d05606a5918e4765be6cc4cad38c3954e248d97b2fa378b954f323335c93298913c61048cf0b396467df1c6b6bbba4d01";
        "0xcd68657ad3c6d5a979093ff1345eefb269e86498b519418fa0e5e756ce01a9b939d46b405d2a167883bda0a40023cce39659890c97a59e3f772b6fc3c2e0322100";
        "0x4d425ad3bfef2de8b1d523cbfd754d4ea2c8e8fa5f6904b0389f4e5d62039083454e2a24da6e1c90334fd760eaa1921bdac10158758f682cb401e1df16ee0ec801";
        "0xb8e8eb23200a1571c6ad516528ce3f1280506eed9f1e380b9a5e770f1e89d8414182c7b83dc8f230d288c4745d0a8102cfc015d422af77d139bc606054363ecf01";
        "0x38fc63be97c354539b517bd8abd97d86fa3484894663aa7dcf578cfad7368df577206e28daf800cba179314f34f1a6dec569ff12e15c22b598028cce64bfa54601";
*)      (* From test/EthCrossChainManagerTest.js *)
        "0x7d588d79ac9f0931c69150de6bfe5289f0147893781bffbcc32b5e07bd687d1048dda039ffc1e87de2e98610dc876e97411d604948473904b12b64bed8880bcc00";
        "0xea8be33bb197c82690987e22e970221de11dfa019f470d784ef211edb6c9a3fd75bf74904adea08ed37a635c4dc58ccc21369afc1abcab4696a42be1097468a400";
        "0x289be668444122fd1d48c62781ded43e6fbda9bdd587dc7ee1bd326390d70e3f0e174fbd4854ed96c697dcee93feabbf7cdf290ebee93d4f5156d75d62b80ba301";
        "0xe79df9e679af49c403bbf05a24af2307adc96b641f4501fdb96e6704d27b2a87278e15bfee5909d4fa62dd45907cba23f833b3e96378d140d56722d1f59821e400";
        "0x6d8349493021e2cd6af96524357867b6be9d24ef33aaf66c430d5f91c33253304380ee17c6839fed964e7ba4910dd26533125b548cff6450140b10caec1b08fe01";

      ]
      in
      List.iter signatures ~f:(fun s ->
          (* Recoverable signatures have an extra byte. *)
          let bs = Bystr.parse_hex s in
          let rs = Bystr.to_raw_bytes @@ Bystr.sub ~pos:0 ~len:signature_len bs in
          let v = Bystr.sub ~pos:signature_len ~len:1 bs in
          let v_int =
            Uint8.to_int
            @@ Uint8.of_bytes_big_endian
                 (Bytes.of_string (Bystr.to_raw_bytes v))
                 0
          in
          let addr = ecrecover (hash_string header) rs v_int in
          printf "\n%s" ("0x" ^ Hex.show @@ Hex.of_string addr);
        ))

let ecdsa_tests = "ecdsa_tests" >::: [ t1'; t2'; t3' ]

module All = struct
  let tests _ = "signature_tests" >::: [ schnorr_tests; ecdsa_tests ]
end
