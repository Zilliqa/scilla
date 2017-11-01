open Secp256k1
open OUnit2

let signature_of_string ctx s =
  let { Cstruct.buffer } = Hex.to_cstruct s in
  Sign.of_der_exn ctx buffer

let pk_of_string ctx s =
  let { Cstruct.buffer } = Hex.to_cstruct s in
  Public.of_bytes_exn ctx buffer

let sk_of_string ctx s =
  let { Cstruct.buffer } = Hex.to_cstruct s in
  Secret.of_bytes_exn ctx buffer

let buffer_of_string s =
  let { Cstruct.buffer } = Hex.to_cstruct s in
  buffer

let sanitize_hex s_hex = Hex.(of_string (to_string (`Hex s_hex)))

let ctx = Context.create [ Sign ; Verify ]

let hex_printer (`Hex str) = str

let test_signature_of_string octx =
  let sign_orig_hex = sanitize_hex
      "3044022079BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F817980220294F14E883B3F525B5367756C2A11EF6CF84B730B36C17CB0C56F0AAB2C98589" in
  let signature = signature_of_string ctx sign_orig_hex in
  let sign = Sign.to_der ctx signature in
  let sign_hex = Hex.of_cstruct (Cstruct.of_bigarray sign) in
  assert_equal ~printer:hex_printer sign_orig_hex sign_hex

let test_valid_signature octx =
  let ctx = Context.create [Verify] in
  let msg = buffer_of_string
      (`Hex "CF80CD8AED482D5D1527D7DC72FCEFF84E6326592848447D2DC0B0E87DFC9A90") in
  let signature = signature_of_string ctx
      (`Hex "3044022079BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F817980220294F14E883B3F525B5367756C2A11EF6CF84B730B36C17CB0C56F0AAB2C98589") in
  let pubkey = pk_of_string ctx
      (`Hex "040A629506E1B65CD9D2E0BA9C75DF9C4FED0DB16DC9625ED14397F0AFC836FAE595DC53F8B0EFE61E703075BD9B143BAC75EC0E19F82A2208CAEB32BE53414C40") in
  assert_equal true (Sign.verify ctx ~signature ~pubkey msg)

let test_invalid_signature octx =
  let ctx = Context.create [Verify] in
  let msg = buffer_of_string
      (`Hex "CF80CD8AED482D5D1527D7DC72FCEFF84E6326592848447D2DC0B0E87DFC9A91") in
  let signature = signature_of_string ctx
      (`Hex "3044022079BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F817980220294F14E883B3F525B5367756C2A11EF6CF84B730B36C17CB0C56F0AAB2C98589") in
  let pubkey = pk_of_string ctx
      (`Hex "040a629506e1b65cd9d2e0ba9c75df9c4fed0db16dc9625ed14397f0afc836fae595dc53f8b0efe61e703075bd9b143bac75ec0e19f82a2208caeb32be53414c40") in
  assert_equal false (Sign.verify ctx ~signature ~pubkey msg)

let test_public_module octx =
  let pubtrue_hex =
    `Hex "04c591a8ff19ac9c4e4e5793673b83123437e975285e7b442f4ee2654dffca5e2d2103ed494718c697ac9aebcfd19612e224db46661011863ed2fc54e71861e2a6" in
  let pubtrue = buffer_of_string pubtrue_hex in
  let pub = Public.of_bytes_exn ctx pubtrue in
  let pub_serialized =
    Public.to_bytes ~compress:false ctx pub |>
    Cstruct.of_bigarray |>
    Hex.of_cstruct in
  assert_equal ~printer:hex_printer pubtrue_hex pub_serialized

let test_pubkey_creation octx =
  let seckey = buffer_of_string (`Hex "67E56582298859DDAE725F972992A07C6C4FB9F62A8FFF58CE3CA926A1063530") in
  let pubtrue = `Hex "04c591a8ff19ac9c4e4e5793673b83123437e975285e7b442f4ee2654dffca5e2d2103ed494718c697ac9aebcfd19612e224db46661011863ed2fc54e71861e2a6" in
  let seckey = Secret.of_bytes_exn ctx seckey in
  let pubkey = Public.of_secret ctx seckey in
  let pubkey_serialized =
    Public.to_bytes ~compress:false ctx pubkey |>
    Cstruct.of_bigarray |>
    Hex.of_cstruct in
  assert_equal ~printer:hex_printer pubtrue pubkey_serialized

let test_sign octx =
  let ctx = Context.create [Sign] in
  let msg = buffer_of_string (`Hex "CF80CD8AED482D5D1527D7DC72FCEFF84E6326592848447D2DC0B0E87DFC9A90") in
  let seckey = sk_of_string ctx (`Hex "67E56582298859DDAE725F972992A07C6C4FB9F62A8FFF58CE3CA926A1063530") in
  let validsign = signature_of_string ctx (`Hex "30440220182a108e1448dc8f1fb467d06a0f3bb8ea0533584cb954ef8da112f1d60e39a202201c66f36da211c087f3af88b50edf4f9bdaa6cf5fd6817e74dca34db12390c6e9") in
  let sign = Sign.sign ctx ~seckey msg in
  assert_equal 0 (Sign.compare sign validsign)

let test_recover octx =
  let ctx = Context.create [Sign] in
  let msg = buffer_of_string (`Hex "CF80CD8AED482D5D1527D7DC72FCEFF84E6326592848447D2DC0B0E87DFC9A90") in
  let seckey = sk_of_string ctx (`Hex "67E56582298859DDAE725F972992A07C6C4FB9F62A8FFF58CE3CA926A1063530") in
  let pubkey = Public.of_secret ctx seckey in
  let recoverable_sign = RecoverableSign.sign ctx seckey msg in
  let usual_sign = RecoverableSign.convert ctx recoverable_sign in
  let verify_ctx = Context.create [Verify] in
  assert (Sign.verify verify_ctx pubkey usual_sign msg);
  let (compact, recid) = RecoverableSign.to_compact verify_ctx recoverable_sign in
  let parsed = RecoverableSign.of_compact_exn verify_ctx compact ~recid in
  assert_equal 0 (RecoverableSign.compare parsed recoverable_sign);
  let recovered = RecoverableSign.recover verify_ctx recoverable_sign msg in
  assert_equal 0 (Public.compare recovered pubkey)

let suite =
  "secp256k1" >::: [
    "signature_of_string" >:: test_signature_of_string ;
    "valid_signature" >:: test_valid_signature ;
    "invalid_signature" >:: test_invalid_signature ;
    "public_module" >:: test_public_module ;
    "pubkey_creation" >:: test_pubkey_creation ;
    "sign" >:: test_sign ;
    "recover" >:: test_recover ;
  ]

let () = run_test_tt_main suite
