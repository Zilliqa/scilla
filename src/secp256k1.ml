type t;;
type pubkey = string;;
type ecdsa_signature = string;;
	
type cflag = CONTEXT_VERIFY | CONTEXT_SIGN | CONTEXT_SIGNVERIFY | CONTEXT_NONE ;;

(* Context *)
external context_create 			: cflag -> t = "ml_secp256k1_context_create";;
external context_destroy 			: t -> unit = "ml_secp256k1_context_destroy";;
external context_clone 				: t -> t = "ml_secp256k1_context_clone";;
external context_randomize	: t 
	-> string 			(* seed *)
	-> t option
	= "ml_secp256k1_context_randomize";;


(* Public key *)	
external ec_pubkey_create : t 
	-> string 				(* seckey *)
	-> pubkey option
	= "ml_secp256k1_ec_pubkey_create";;

		
(* ECDSA *)	
external ecdsa_verify	: t 
	-> ecdsa_signature 	(* signature *)
	-> string 			(* message *)
	-> pubkey 			(* pubkey *)
	-> bool 
	= "ml_secp256k1_ecdsa_verify";;


external ecdsa_sign : t 
	-> string 			(* message *)
	-> string 			(* seckey *)
	-> ecdsa_signature option
	= "ml_secp256k1_ecdsa_sign";;
		