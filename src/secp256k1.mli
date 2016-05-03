type t
type pubkey
type ecdsa_signature
type cflag =
    CONTEXT_VERIFY
  | CONTEXT_SIGN
  | CONTEXT_SIGNVERIFY
  | CONTEXT_NONE
external context_create : cflag -> t = "ml_secp256k1_context_create"
external context_destroy : t -> unit = "ml_secp256k1_context_destroy"
external context_clone : t -> t = "ml_secp256k1_context_clone"
external context_randomize : t -> string -> t option
  = "ml_secp256k1_context_randomize"
external ec_pubkey_serialize : t -> pubkey -> bool -> string option
  = "ml_secp256k1_ec_pubkey_serialize"
external ec_pubkey_parse : t -> string -> pubkey option
  = "ml_secp256k1_ec_pubkey_parse"
external ec_pubkey_create : t -> string -> pubkey option
  = "ml_secp256k1_ec_pubkey_create"
external ecdsa_signature_serialize_compact : t -> ecdsa_signature -> string
  = "ml_secp256k1_ecdsa_signature_serialize_compact"
external ecdsa_signature_serialize_der :
  t -> ecdsa_signature -> string option
  = "ml_secp256k1_ecdsa_signature_serialize_der"
external ecsda_verify : t -> ecdsa_signature -> string -> pubkey -> bool
  = "ml_secp256k1_ecdsa_verify"
external ecsda_sign : t -> string -> string -> ecdsa_signature option
  = "ml_secp256k1_ecdsa_sign"
