#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/custom.h>
#include <caml/bigarray.h>

#include <string.h>
#include <secp256k1.h>
#include <secp256k1_recovery.h>

/* Accessing the secp256k1_context * part of an OCaml custom block */
#define Context_val(v) (*((secp256k1_context **) Data_custom_val(v)))

CAMLprim void
ml_secp256k1_context_destroy (value ml_ctx)
{
    CAMLparam1 (ml_ctx);
    secp256k1_context_destroy (Context_val (ml_ctx));
}

static struct custom_operations secp256k1_context_ops = {
    .identifier = "secp256k1_context",
    .finalize = ml_secp256k1_context_destroy,
    .compare = custom_compare_default,
    .compare_ext = custom_compare_ext_default,
    .hash = custom_hash_default,
    .serialize = custom_serialize_default,
    .deserialize = custom_deserialize_default
};

static value alloc_context (secp256k1_context *ctx) {
    value ml_ctx = alloc_custom (&secp256k1_context_ops, sizeof(secp256k1_context *),0, 1);
    Context_val(ml_ctx) = ctx;
    return ml_ctx;
}

/* Create a new secp256k1 context */
CAMLprim value
ml_secp256k1_context_create (value ml_flags)
{
    CAMLparam1 (ml_flags);

    /* Parse context flag */
    int flags = SECP256K1_CONTEXT_NONE;
    switch (Int_val (ml_flags)) {
    case 1:
        flags = SECP256K1_CONTEXT_VERIFY;
        break;
    case 2:
        flags = SECP256K1_CONTEXT_SIGN;
        break;
    case 3:
        flags = SECP256K1_CONTEXT_SIGN | SECP256K1_CONTEXT_VERIFY;
        break;
    };

    /* Create and return context */
    secp256k1_context *ctx = secp256k1_context_create (flags);

    if (!ctx)
        caml_failwith ("ml_secp256k1_context_create");

    CAMLreturn (alloc_context(ctx));
}

/* Randomize the context */
CAMLprim value
ml_secp256k1_context_randomize (value ml_context, value ml_seed)
{
    CAMLparam2 (ml_context, ml_seed);
    int ret = secp256k1_context_randomize (Context_val (ml_context),
                                           (const unsigned char*) String_val(ml_seed));

    CAMLreturn (Val_bool (ret));
}


/* Clone the context */
CAMLprim value
ml_secp256k1_context_clone (value ml_context)
{
    CAMLparam1 (ml_context);
    secp256k1_context *ctx = secp256k1_context_clone (Context_val (ml_context));

    if (!ctx)
        caml_failwith ("ml_secp256k1_context_clone");

    CAMLreturn (alloc_context(ctx));
}

CAMLprim value
ml_secp256k1_ec_seckey_verify (value ml_context, value ml_seckey) {
    CAMLparam2(ml_context, ml_seckey);
    int ret = secp256k1_ec_seckey_verify(Context_val (ml_context),
                                         Caml_ba_data_val(ml_seckey));

    CAMLreturn(Val_bool (ret));
}

/* Create public key */
CAMLprim value
ml_secp256k1_ec_pubkey_create (value ml_context, value ml_seckey) {
    CAMLparam2 (ml_context, ml_seckey);
    CAMLlocal1 (result);

    /* Create the publickey */
    secp256k1_pubkey pubkey;
    int ret;

    ret = secp256k1_ec_pubkey_create (Context_val (ml_context),
                                      &pubkey,
                                      Caml_ba_data_val(ml_seckey));

    if (!ret)
        caml_failwith ("ml_secp256k1_ec_pubkey_create");

    result = caml_ba_alloc_dims(CAML_BA_UINT8 | CAML_BA_C_LAYOUT, 1,
                                NULL,
                                sizeof(secp256k1_pubkey));
    memcpy(Caml_ba_data_val(result), pubkey.data, sizeof(secp256k1_pubkey));

    CAMLreturn (result);
}

CAMLprim value
ml_secp256k1_ec_pubkey_serialize (value ml_context, value ml_pubkey, value ml_compressed) {
    CAMLparam3 (ml_context, ml_pubkey, ml_compressed);
    CAMLlocal1 (result);

    size_t size = 65;
    unsigned int flags =
        Bool_val(ml_compressed) ? SECP256K1_EC_COMPRESSED : SECP256K1_EC_UNCOMPRESSED;
    unsigned char pubkey[size];

    int ret = secp256k1_ec_pubkey_serialize(Context_val (ml_context),
                                            pubkey,
                                            &size,
                                            Caml_ba_data_val(ml_pubkey),
                                            flags);

    if (!ret)
        caml_failwith ("ml_secp256k1_ec_pubkey_serialize");

    result = caml_ba_alloc_dims(CAML_BA_UINT8 | CAML_BA_C_LAYOUT, 1, NULL, size);
    memcpy(Caml_ba_data_val(result), pubkey, size);

    CAMLreturn (result);
}

CAMLprim value
ml_secp256k1_ec_pubkey_parse(value ml_context, value ml_buf) {
    CAMLparam2 (ml_context, ml_buf);
    CAMLlocal1 (result);

    secp256k1_pubkey pubkey;

    int ret = secp256k1_ec_pubkey_parse(Context_val (ml_context),
                                        &pubkey,
                                        Caml_ba_data_val(ml_buf),
                                        Caml_ba_array_val(ml_buf)->dim[0]);

    if (!ret)
        caml_failwith ("ml_secp256k1_ec_pubkey_parse");

    result = caml_ba_alloc_dims(CAML_BA_UINT8 | CAML_BA_C_LAYOUT, 1,
                                NULL,
                                sizeof(secp256k1_pubkey));
    memcpy(Caml_ba_data_val(result), pubkey.data, sizeof(secp256k1_pubkey));

    CAMLreturn (result);
}

CAMLprim value
ml_secp256k1_ecdsa_signature_parse (value ml_context, value ml_buf, value ml_compact) {
    CAMLparam3 (ml_context, ml_buf, ml_compact);
    CAMLlocal1 (result);
    int ret;

    secp256k1_ecdsa_signature sign;
    if (Bool_val (ml_compact))
        ret = secp256k1_ecdsa_signature_parse_compact (Context_val (ml_context),
                                                       &sign,
                                                       Caml_ba_data_val(ml_buf));
    else
        ret = secp256k1_ecdsa_signature_parse_der (Context_val (ml_context),
                                                   &sign,
                                                   Caml_ba_data_val(ml_buf),
                                                   Caml_ba_array_val(ml_buf)->dim[0]);

    if (!ret)
        caml_failwith ("ml_secp256k1_ecdsa_signature_parse");

    result = caml_ba_alloc_dims(CAML_BA_UINT8 | CAML_BA_C_LAYOUT, 1,
                                NULL,
                                sizeof(secp256k1_ecdsa_signature));
    memcpy(Caml_ba_data_val(result), sign.data, sizeof(secp256k1_ecdsa_signature));

    CAMLreturn (result);
}

/* Verify an ecdsa signature */
CAMLprim value
ml_secp256k1_ecdsa_verify (value ml_context, value ml_pubkey, value ml_msg, value ml_signature) {
    CAMLparam4 (ml_context, ml_signature, ml_msg, ml_pubkey);

    int ret = secp256k1_ecdsa_verify (Context_val (ml_context),
                                      Caml_ba_data_val(ml_signature),
                                      Caml_ba_data_val(ml_msg),
                                      Caml_ba_data_val(ml_pubkey));

    CAMLreturn(Val_bool (ret));
}


/* Sign a message with ECDSA */
CAMLprim value
ml_secp256k1_ecdsa_sign (value ml_context, value ml_seckey, value ml_msg) {
    CAMLparam3(ml_context, ml_msg, ml_seckey);
    CAMLlocal1 (result);

    secp256k1_ecdsa_signature sign;
    int ret;

    ret = secp256k1_ecdsa_sign (Context_val (ml_context),
                                &sign,
                                Caml_ba_data_val(ml_msg),
                                Caml_ba_data_val(ml_seckey),
                                NULL, NULL);

    if (!ret)
        caml_failwith ("ml_secp256k1_ecdsa_sign");

    result = caml_ba_alloc_dims(CAML_BA_UINT8 | CAML_BA_C_LAYOUT, 1,
                                NULL,
                                sizeof(secp256k1_ecdsa_signature));
    memcpy(Caml_ba_data_val(result), sign.data, sizeof(secp256k1_ecdsa_signature));

    CAMLreturn (result);
}

CAMLprim value
ml_secp256k1_ecdsa_signature_serialize(value ml_context, value ml_signature, value ml_compact) {
    CAMLparam3 (ml_context, ml_signature, ml_compact);
    CAMLlocal1 (result);

    int ret;
    size_t size = 72;
    unsigned char buf[size];
    if (Bool_val (ml_compact))
        ret = secp256k1_ecdsa_signature_serialize_compact(Context_val (ml_context),
                                                          buf,
                                                          Caml_ba_data_val(ml_signature));
    else
        ret = secp256k1_ecdsa_signature_serialize_der(Context_val (ml_context),
                                                      buf,
                                                      &size,
                                                      Caml_ba_data_val(ml_signature));

    if (!ret)
        caml_failwith ("ml_secp256k1_ecdsa_signature_serialize");

    result = caml_ba_alloc_dims(CAML_BA_UINT8 | CAML_BA_C_LAYOUT, 1,
                                NULL,
                                size);
    memcpy(Caml_ba_data_val(result), buf, size);

    CAMLreturn (result);
}

CAMLprim value
ml_secp256k1_ecdsa_recoverable_signature_parse_compact (value ml_context, value ml_buf, value ml_recid) {
    CAMLparam3 (ml_context, ml_buf, ml_recid);
    CAMLlocal1 (result);
    int ret;

    secp256k1_ecdsa_recoverable_signature sign;
    ret = secp256k1_ecdsa_recoverable_signature_parse_compact (Context_val (ml_context),
                                                               &sign,
                                                               Caml_ba_data_val(ml_buf),
                                                               Int_val(ml_recid));

    if (!ret)
        caml_failwith ("ml_secp256k1_ecdsa_recoverable_signature_parse_compact");

    result = caml_ba_alloc_dims(CAML_BA_UINT8 | CAML_BA_C_LAYOUT, 1,
                                NULL,
                                sizeof(secp256k1_ecdsa_recoverable_signature));
    memcpy(Caml_ba_data_val(result), sign.data, sizeof(secp256k1_ecdsa_recoverable_signature));

    CAMLreturn (result);
}

CAMLprim value
ml_secp256k1_ecdsa_sign_recoverable (value ml_context, value ml_seckey, value ml_msg) {
    CAMLparam3(ml_context, ml_seckey, ml_msg);
    CAMLlocal1 (result);

    secp256k1_ecdsa_recoverable_signature sign;
    int ret;

    ret = secp256k1_ecdsa_sign_recoverable (Context_val (ml_context),
                                            &sign,
                                            Caml_ba_data_val(ml_msg),
                                            Caml_ba_data_val(ml_seckey),
                                            NULL, NULL);

    if (!ret)
        caml_failwith ("ml_secp256k1_ecdsa_sign_recoverable");

    result = caml_ba_alloc_dims(CAML_BA_UINT8 | CAML_BA_C_LAYOUT, 1,
                                NULL,
                                sizeof(secp256k1_ecdsa_recoverable_signature));
    memcpy(Caml_ba_data_val(result), sign.data, sizeof(secp256k1_ecdsa_recoverable_signature));

    CAMLreturn (result);
}
