open Printf;;
open Secp256k1;;

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