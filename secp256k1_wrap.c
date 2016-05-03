#include <caml/mlvalues.h>
#include <caml/fail.h>

#include <stdio.h>
#include <secp256k1.h>


/* Create a new secp256k1 context */
CAMLprim value
ml_secp256k1_context_create (value ml_flags)
{
	/* Parse context flag */
	int flags = 0;
    switch (Int_val (ml_flags)) {
		case 0:
			flags = SECP256K1_CONTEXT_VERIFY;
			break;
		case 1:
			flags = SECP256K1_CONTEXT_SIGN;
			break;
		case 2:
			flags = SECP256K1_CONTEXT_NONE;
			break;
	};

	/* Create and return context */
	secp256k1_context *ctx = secp256k1_context_create (flags);	
	return (value) ctx;
}


/* Randomize the context */
CAMLprim value 
ml_secp256k1_context_randomize (value ml_context, value ml_seed) 
{
	secp256k1_context *ctx = (secp256k1_context *) (ml_context);	
	secp256k1_context_randomize (ctx, (unsigned char *) String_val(ml_seed));
	return (value) ctx;
}


/* Clone the context */
CAMLprim value 
ml_secp256k1_context_clone (value ml_context) 
{
	secp256k1_context *ctx = (secp256k1_context *) (ml_context);	
	secp256k1_context *ctx2 = secp256k1_context_clone (ctx);
	return (value) ctx2;
}

/* Destroy the context */
CAMLprim value 
ml_secp256k1_context_destroy (value ml_context) 
{
	secp256k1_context *ctx = (secp256k1_context *) (ml_context);	
	secp256k1_context_destroy (ctx);
	return Val_unit;
}


/* Create public key */
CAMLprim value 
ml_secp256k1_ec_pubkey_create (value ml_context, value ml_seckey) {
	secp256k1_context *ctx = (secp256k1_context *) (ml_context);	
	secp256k1_pubkey pubkey;
	
	int r = secp256k1_ec_pubkey_create (ctx, &pubkey, (unsigned char *) String_val (ml_seckey));
	
	if (r) 
		return Val_some ((value) ((secp256k1_pubkey *) &pubkey));
	else
		return Val_int (0);	
}


/* Verify an ecdsa signature */
CAMLprim value
ml_secp256_ecdsa_verify (value ml_context, value ml_signature, value ml_msg, value ml_pubkey) {
	secp256k1_context *ctx = (secp256k1_context *) (ml_context);	
	secp256k1_pubkey *pubkey = (secp256k1_pubkey *) (ml_pubkey);
	secp256k1_ecdsa_signature *sign = (secp256k1_ecdsa_signature *) (ml_signature);
	unsigned char *msg = (unsigned char *) (ml_msg);
	
	int r = secp256k1_ecdsa_verify (ctx, sign, msg, pubkey);
	return Val_int (r);
}