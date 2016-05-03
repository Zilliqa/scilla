type t;;
type pubkey;;
type ecdsa_signature;;
	
type cflag = CONTEXT_VERIFY | CONTEXT_SIGN | CONTEXT_SIGNVERIFY | CONTEXT_NONE ;;

(* Context *)
external context_create 			: cflag -> t = "ml_secp256k1_context_create";;
external context_destroy 			: t -> unit = "ml_secp256k1_context_destroy";;
external context_clone 				: t -> t = "ml_secp256k1_context_clone";;
external context_randomize	: t 
	-> string 			(* Seed *)
	-> t option
	= "ml_secp256k1_context_randomize";;


(* Public key *)
external ec_pubkey_serialize : t
	-> pubkey
	-> bool 			(* compressed *) 
	-> string option
	= "ml_secp256k1_ec_pubkey_serialize";;
	
	
external ec_pubkey_parse : t 
	-> string
	-> pubkey option
	= "ml_secp256k1_ec_pubkey_parse";;

external ec_pubkey_create : t 
	-> string 				(* seckey *)
	-> pubkey option
	= "ml_secp256k1_ec_pubkey_create";;

		
(* ECDSA *)	
external ecdsa_signature_serialize_compact :  t 
	-> ecdsa_signature 
	-> string 
	= "ml_secp256k1_ecdsa_signature_serialize_compact";;
	
	
external ecdsa_signature_serialize_der :  t 
	-> ecdsa_signature 
	-> string option
	= "ml_secp256k1_ecdsa_signature_serialize_der";;


external ecsda_verify	: t 
	-> ecdsa_signature 	(* signature *)
	-> string 			(* message *)
	-> pubkey 			(* pubkey *)
	-> bool 
	= "ml_secp256k1_ecdsa_verify";;


external ecsda_sign : t 
	-> string 			(* message *)
	-> string 			(* seckey *)
	-> ecdsa_signature option
	= "ml_secp256k1_ecdsa_sign";;
		
			
		
(*external seckey_verify
external privkey_tweak_mul
external privkey_tweak_add
external pubkey_tweak_mul
external pubkey_tweak_add
external create_ecdh_secret
external schnorr_sign*)