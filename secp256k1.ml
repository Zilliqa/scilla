module Secp256k1 = struct
	type t;;
	type pubkey;;
	type ecsda_signature;;
	type nonce_function;;
	
	type cflag = 
		  CONTEXT_VERIFY
		| CONTEXT_SIGN
		| CONTEXT_NONE
	;;
	
	external context_create 	: cflag -> t = "ml_secp256k1_context_create";;
	external context_destroy 	: t -> unit = "ml_secp256k1_context_destroy";;
	external context_clone 		: t -> t = "ml_secp256k1_context_clone";;
	external context_randomize	: t -> string -> t = "ml_secp256k1_context_randomize";;
	
	external ecsda_verify	: t 
		-> ecsda_signature 	(* signature *)
		-> string 			(* message *)
		-> pubkey 			(* pubkey *)
		-> bool 
		= "ml_secp256k1_ecsda_verify";;


	external ecsda_sign : t 
		-> ecsda_signature 
		-> string 
		-> string 
		-> nonce_function 
		-> string 
		-> string 
		= "ml_secp256k1_ecsda_sign_bytecode" "ml_secp256k1_ecsda_sign";;
		
	external ec_pubkey_create : t 
		-> string 				(* seckey *)
		-> pubkey option
		= "ml_secp256k1_ec_pubkey_create";;
				
	(*external seckey_verify
	external privkey_tweak_mul
	external privkey_tweak_add
	external pubkey_tweak_mul
	external pubkey_tweak_add
	external create_ecdh_secret
	external schnorr_sign*)
end