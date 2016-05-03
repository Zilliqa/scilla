open Printf;;
open Secp256k1;;

let msg = "CF80CD8AED482D5D1527D7DC72FCEFF84E6326592848447D2DC0B0E87DFC9A90" in
let sign = "3044022079BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F817980220294F14E883B3F525B5367756C2A11EF6CF84B730B36C17CB0C56F0AAB2C98589" in
let pub = "040A629506E1B65CD9D2E0BA9C75DF9C4FED0DB16DC9625ED14397F0AFC836FAE595DC53F8B0EFE61E703075BD9B143BAC75EC0E19F82A2208CAEB32BE53414C40" in
let ctx = Secp256k1.context_create Secp256k1.CONTEXT_VERIFY in
let res = Secp256k1.ecdsa_verify ctx sign msg pub in
	match res with
	| true -> Printf.printf "Is true, right!"
	| false -> Printf.printf "Oh snap, is false"
;;


(*
let ctx = Secp256k1.context_create Secp256k1.CONTEXT_SIGN in
	Printf.printf "Created context\n%!";
	let ctxdup = Secp256k1.context_clone ctx in
	Printf.printf "Context duplicated\n%!";
	let octx = Secp256k1.context_randomize ctx "A441B25FE9A3CF56661190A0B93B9DEC7D04127288CC87250967CF3B52894D11" in
	match octx with 
	| None -> Printf.printf "Randomize failed\n%!"
	| Some (ctx2) -> 
		Printf.printf "Randomized!\n%!";
		let pk = Secp256k1.ec_pubkey_create ctx2 "hola" in
		match pk with
		| None -> Printf.printf "Noneee\n%!"
		| Some (pk) -> 
			match Secp256k1.ec_pubkey_to_string ctx2 pk false with
			| Some (p) -> 
				Printf.printf "Created %s\n%!" p
			| None -> Printf.printf "Failed"
			;
		;;
*)		
