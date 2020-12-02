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

let t3' =
  test_case (fun _ ->
      let hash_string m = Cryptokit.(hash_string (Hash.sha2 256) m) in
      let open Secp256k1Wrapper in
      let open Literal.GlobalLiteral in
      let open Stdint in
      let header = hash_string @@ Bystr.to_raw_bytes @@ Bystr.parse_hex
         "0x000000009b91561700000000f48a4057bef268cc3fdb034e69dc2e942907e08ac4a420d1b196b8c28ebf5bf2000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002a8be0a1605a63a31704aec4eb4f1023f1ecc2934bd86f119ab77526f9477af9a57e1a5f508e0000410782720ab189fffd84057b226c6561646572223a332c227672665f76616c7565223a22424f4f336f58796b32524970655651593338547133714a423832737a4a68366e4f6f724a55702f4a4d582b474c707a347a497347394c4a6c34784a6f34657448674f56357169364d484b6674714f69724f755a495a69593d222c227672665f70726f6f66223a22635953525746506f69394748414247526255646836612b35506f4f317776354a557a53417457786845637071757430536a595873344c7453353574534a74334174493059616d4c67524a797a524f68564756626d34673d3d222c226c6173745f636f6e6669675f626c6f636b5f6e756d223a33363433322c226e65775f636861696e5f636f6e666967223a7b2276657273696f6e223a312c2276696577223a342c226e223a382c2263223a322c22626c6f636b5f6d73675f64656c6179223a31303030303030303030302c22686173685f6d73675f64656c6179223a31303030303030303030302c22706565725f68616e647368616b655f74696d656f7574223a31303030303030303030302c227065657273223a5b7b22696e646578223a312c226964223a2231323035303238313732393138353430623262353132656165313837326132613265336132386439383963363064393564616238383239616461376437646437303664363538227d2c7b22696e646578223a342c226964223a2231323035303236373939333061343261616633633639373938636138613366313265313334633031393430353831386437383364313137343865303339646538353135393838227d2c7b22696e646578223a332c226964223a2231323035303234383261636236353634623139623930363533663665396338303632393265386161383366373865376139333832613234613665666534316330633036663339227d2c7b22696e646578223a352c226964223a2231323035303234363864643138393965643264316363326238323938383261313635613065636236613734356166306337326562323938326436366234333131623465663733227d2c7b22696e646578223a382c226964223a2231323035303339333432313434356239343231626434636339306437626338386339333031353538303437613736623230633539653763353131656537643232393938326231227d2c7b22696e646578223a322c226964223a2231323035303338623861663632313065636664636263616232323535326566386438636634316336663836663963663961623533643836353734316366646238333366303662227d2c7b22696e646578223a372c226964223a2231323035303331653037373966356335636362323631323335326665346132303066393964336537373538653730626135336636303763353966663232613330663637386666227d2c7b22696e646578223a362c226964223a2231323035303265623162616162363032633538393932383235363163646161613761616262636464306363666362633365373937393361633234616366393037373866333561227d5d2c22706f735f7461626c65223a5b322c382c352c352c382c372c312c342c352c362c352c342c372c372c332c332c342c362c312c322c342c382c352c342c372c342c362c362c322c322c312c312c382c382c362c362c362c372c382c372c342c382c352c312c332c332c382c352c332c362c332c362c372c352c362c322c332c312c322c362c352c322c312c342c322c312c382c342c382c332c382c372c372c352c312c372c342c342c312c352c322c352c362c312c322c382c332c332c312c332c312c342c312c372c382c362c382c322c352c312c342c352c332c322c322c322c382c332c332c332c362c372c342c372c342c322c372c352c362c375d2c226d61785f626c6f636b5f6368616e67655f76696577223a36303030307d7df8fc7a1f6a856313c591a3a747f4eca7218a820b" in
      let signatures = [
        "0x7d588d79ac9f0931c69150de6bfe5289f0147893781bffbcc32b5e07bd687d1048dda039ffc1e87de2e98610dc876e97411d604948473904b12b64bed8880bcc00";
        "0xea8be33bb197c82690987e22e970221de11dfa019f470d784ef211edb6c9a3fd75bf74904adea08ed37a635c4dc58ccc21369afc1abcab4696a42be1097468a400";
        "0x289be668444122fd1d48c62781ded43e6fbda9bdd587dc7ee1bd326390d70e3f0e174fbd4854ed96c697dcee93feabbf7cdf290ebee93d4f5156d75d62b80ba301";
        "0xe79df9e679af49c403bbf05a24af2307adc96b641f4501fdb96e6704d27b2a87278e15bfee5909d4fa62dd45907cba23f833b3e96378d140d56722d1f59821e400";
        "0x6d8349493021e2cd6af96524357867b6be9d24ef33aaf66c430d5f91c33253304380ee17c6839fed964e7ba4910dd26533125b548cff6450140b10caec1b08fe01";
      ]
      in
      let _pubkeys =
        [
          "0x1205041e0779f5c5ccb2612352fe4a200f99d3e7758e70ba53f607c59ff22a30f678ff757519efff911efc7ed326890a2752b9456cc0054f9b63215f1d616e574d6197";
          "0x120504468dd1899ed2d1cc2b829882a165a0ecb6a745af0c72eb2982d66b4311b4ef73cff28a6492b076445337d8037c6c7be4d3ec9c4dbe8d7dc65d458181de7b5250";
          "0x120504482acb6564b19b90653f6e9c806292e8aa83f78e7a9382a24a6efe41c0c06f39ef0a95ee60ad9213eb0be343b703dd32b12db32f098350cf3f4fc3bad6db23ce";
          "0x120504679930a42aaf3c69798ca8a3f12e134c019405818d783d11748e039de8515988754f348293c65055f0f1a9a5e895e4e7269739e243a661fff801941352c38712";
          "0x1205048172918540b2b512eae1872a2a2e3a28d989c60d95dab8829ada7d7dd706d658df044eb93bbe698eff62156fc14d6d07b7aebfbc1a98ec4180b4346e67cc3fb0";
          "0x1205048b8af6210ecfdcbcab22552ef8d8cf41c6f86f9cf9ab53d865741cfdb833f06b72fcc7e7d8b9e738b565edf42d8769fd161178432eadb2e446dd0a8785ba088f";
          "0x12050493421445b9421bd4cc90d7bc88c9301558047a76b20c59e7c511ee7d229982b142bbf593006e8099ad4a2e3a2a9067ce46b7d54bab4b8996e7abc3fcd8bf0a5f";
          "0x120504eb1baab602c5899282561cdaaa7aabbcdd0ccfcbc3e79793ac24acf90778f35a059fca7f73aeb60666178db8f704b58452b7a0b86219402c0770fcb52ac9828c";
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
          match recover_pk header rs v_int with
          | Ok pk ->
            printf "\n%d %s" v_int ("0x" ^ Hex.show @@ Hex.of_string pk);
            (match verify pk header rs with
            | Ok res -> printf ". Verification: %b" res
            | Error _ -> assert_failure ("Error verifying signature.")
            )
          | Error _ ->
              assert_failure ("Error recovering public key from signature " ^ s)
        ))

let ecdsa_tests = "ecdsa_tests" >::: [ t1'; t2'; t3' ]

module All = struct
  let tests _ = "signature_tests" >::: [ schnorr_tests; ecdsa_tests ]
end
