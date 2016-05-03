#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>

#include <stdio.h>
#include <secp256k1.h>


/* Helper */
#define Val_none Val_int(0)

static value
Val_some (value v)
{   
    CAMLparam1 (v);
    CAMLlocal1 (some);
    some = caml_alloc (1, 0);
    Store_field (some, 0, v);
    CAMLreturn (some);
}


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
			flags = SECP256K1_CONTEXT_SIGN | SECP256K1_CONTEXT_VERIFY;
			break;
			
		default:
			flags = SECP256K1_CONTEXT_NONE;
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
	int r = secp256k1_context_randomize (ctx, (unsigned char *) String_val(ml_seed));

	if (r) 
		return Val_some ((value) (ctx));
	else
		return Val_none;	
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
		return Val_none;	
}


/* Verify an ecdsa signature */
CAMLprim value
ml_secp256k1_ecdsa_verify (value ml_context, value ml_signature, value ml_msg, value ml_pubkey) {
	secp256k1_context *ctx = (secp256k1_context *) (ml_context);	
	secp256k1_pubkey *pubkey = (secp256k1_pubkey *) (ml_pubkey);
	secp256k1_ecdsa_signature *sign = (secp256k1_ecdsa_signature *) (ml_signature);
	unsigned char *msg = (unsigned char *) (ml_msg);
	
	int r = secp256k1_ecdsa_verify (ctx, sign, msg, pubkey);
	return Val_int (r);
}


/* Sign a message with ECDSA */
CAMLprim value
ml_secp256k1_ecdsa_sign (value ml_context, value ml_msg, value ml_seckey) {
	unsigned char *msg = (unsigned char *) (ml_msg);
	unsigned char *seckey = (unsigned char *) (ml_seckey);
	secp256k1_context *ctx = (secp256k1_context *) (ml_context);	
	secp256k1_ecdsa_signature sign;
	
	int r = secp256k1_ecdsa_sign (ctx, &sign, msg, seckey, NULL, NULL);

	if (r) 
		return Val_some ((value) ((secp256k1_ecdsa_signature *) &sign));
	else
		return Val_none;	
}


/* Convert a signature object to string */
CAMLprim value
ml_secp256k1_ecdsa_signature_to_string (value ml_signature) {
	secp256k1_ecdsa_signature *sign = (secp256k1_ecdsa_signature *) (ml_signature);
	
	return caml_copy_string ((const char *) sign->data);
}
