#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>

#include <malloc.h>
#include <string.h>
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

unsigned char *hex_to_binary (char *hex) {
	unsigned len = strlen (hex);
	unsigned char *data = (unsigned char *) malloc (len / 2);	
	char *pos = hex;
    
	size_t count = 0;

    for(count = 0; count < len; count++) {
        sscanf(pos, "%2hhx", &data[count]);
        pos += 2;
    }
	
	return data;
}


char *binary_to_hex (unsigned char *bin, size_t len) {
	size_t count = 0;
	char *hex = (char *) malloc (len * 2);
	char *pos = hex;
    
	for(count = 0; count < len; count ++) {
		sprintf(pos, "%02x", bin[count]);
		pos += 2;
	}

	return hex;
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
	unsigned char *sdata = hex_to_binary (String_val(ml_seed));
	int r = secp256k1_context_randomize (ctx, sdata);

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
	/* Create the publickey */
	secp256k1_context *ctx = (secp256k1_context *) (ml_context);	
	secp256k1_pubkey pubkey;
	unsigned char *seckey = hex_to_binary (String_val(ml_seckey));
	
	int r = secp256k1_ec_pubkey_create (ctx, &pubkey, seckey);
	
	if (!r) return Val_none;
	
	/* Convert to string */
	size_t size = 65;
	unsigned char *output = malloc (size);
	r = secp256k1_ec_pubkey_serialize(ctx, output, &size, &pubkey, SECP256K1_EC_UNCOMPRESSED);

	if (r) 
		return Val_some (caml_copy_string (binary_to_hex (output, size)));
	else
		return Val_none;	
}


/* Verify an ecdsa signature */
CAMLprim value
ml_secp256k1_ecdsa_verify (value ml_context, value ml_signature, value ml_msg, value ml_pubkey) {
	secp256k1_context *ctx = (secp256k1_context *) (ml_context);	
	
	/* Transform pubkey string to pubkey */
	secp256k1_pubkey pubkey;
	int r = secp256k1_ec_pubkey_parse(ctx, &pubkey, hex_to_binary (ml_pubkey), 65);
	
	/* Transform msg to binary */
	unsigned char *msg = (unsigned char *) (hex_to_binary (ml_msg));
	
	/* Transform signature to ecdsa signature */
	secp256k1_ecdsa_signature sign; 
	secp256k1_ecdsa_signature_parse_compact (ctx, &sign, hex_to_binary (ml_signature));

	r = secp256k1_ecdsa_verify (ctx, &sign, msg, &pubkey);
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


