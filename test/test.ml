open Printf;;
open Secp256k1;;


(* Valid signature *)
let msg = "CF80CD8AED482D5D1527D7DC72FCEFF84E6326592848447D2DC0B0E87DFC9A90" in
let sign = "3044022079BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F817980220294F14E883B3F525B5367756C2A11EF6CF84B730B36C17CB0C56F0AAB2C98589" in
let pub = "040A629506E1B65CD9D2E0BA9C75DF9C4FED0DB16DC9625ED14397F0AFC836FAE595DC53F8B0EFE61E703075BD9B143BAC75EC0E19F82A2208CAEB32BE53414C40" in
let ctx = Secp256k1.context_create Secp256k1.CONTEXT_VERIFY in
let res = Secp256k1.ecdsa_verify ctx sign msg pub in
	match res with
	| true -> Printf.printf "Is true, right!\n%!"
	| false -> Printf.printf "Oh snap, is false\n%!"
;;



(* Invalid signature *)		
let msg = "CF80CD8AED482D5D1527D7DC72FCEFF84E6326592848447D2DC0B0E87DFC9A91" in
let sign = "3044022079BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F817980220294F14E883B3F525B5367756C2A11EF6CF84B730B36C17CB0C56F0AAB2C98589" in
let pub = "040a629506e1b65cd9d2e0ba9c75df9c4fed0db16dc9625ed14397f0afc836fae595dc53f8b0efe61e703075bd9b143bac75ec0e19f82a2208caeb32be53414c40" in
let ctx = Secp256k1.context_create Secp256k1.CONTEXT_VERIFY in
let res = Secp256k1.ecdsa_verify ctx sign msg pub in
	match res with
	| true -> Printf.printf "Oh snap, is true\n%!"
	| false -> Printf.printf "Is false, right!\n%!"
;;



(* Pubkey creation *)
let ctx = Secp256k1.context_create Secp256k1.CONTEXT_SIGN in
let sec = "67E56582298859DDAE725F972992A07C6C4FB9F62A8FFF58CE3CA926A1063530" in
let pubtrue = "04c591a8ff19ac9c4e4e5793673b83123437e975285e7b442f4ee2654dffca5e2d2103ed494718c697ac9aebcfd19612e224db46661011863ed2fc54e71861e2a6" in
let pub = Secp256k1.ec_pubkey_create ctx sec in
match pub with 
| Some (pub) -> 
	if pub = pubtrue then 
		Printf.printf "Pubkey match\n%!"
	else
		Printf.printf "Wrong pubkey \n%s\n%s\n%!" pub pubtrue
| None -> Printf.printf "Error\n%!"
;;



(* Sign positive *)
let ctx = Secp256k1.context_create Secp256k1.CONTEXT_SIGN in
let msg = "CF80CD8AED482D5D1527D7DC72FCEFF84E6326592848447D2DC0B0E87DFC9A90" in
let sec = "67E56582298859DDAE725F972992A07C6C4FB9F62A8FFF58CE3CA926A1063530" in
let validsign = "30440220182a108e1448dc8f1fb467d06a0f3bb8ea0533584cb954ef8da112f1d60e39a202201c66f36da211c087f3af88b50edf4f9bdaa6cf5fd6817e74dca34db12390c6e9" in
let sign = Secp256k1.ecdsa_sign ctx msg sec in
match sign with 
| Some (signature) -> 
	if signature = validsign then 
		Printf.printf "Valid signature!\n%!"		
	else
		Printf.printf "Wrong signature \n%s\n%s\n%!" signature validsign
| None -> Printf.printf "Fail\n%!"
;;

