type buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

module Context : sig
  type flag =
    | Sign
    | Verify
    (** which parts of the context to initialize. *)

  type t
  (** Opaque data structure that holds context information
      (precomputed tables etc.).

      Do not create a new context object for each operation, as
      construction is far slower than all other API calls (~100 times
      slower than an ECDSA verification).

      A constructed context can safely be used from multiple threads
      simultaneously, but API call that take a non-const pointer to a
      context need exclusive access to it. In particular this is the
      case for secp256k1_context_destroy and
      secp256k1_context_randomize.

      Regarding randomization, either do it once at creation time (in
      which case you do not need any locking for the other calls), or
      use a read-write lock. *)

  val create : flag list -> t
    (** Create a secp256k1 context object. *)

  val clone : t -> t
    (** Copies a secp256k1 context object. *)

  val randomize : t -> buffer -> bool
  (** While secp256k1 code is written to be constant-time no matter
      what secret values are, it's possible that a future compiler may
      output code which isn't, and also that the CPU may not emit the
      same radio frequencies or draw the same amount power for all
      values.

      This function provides a seed which is combined into the
      blinding value: that blinding value is added before each
      multiplication (and removed afterwards) so that it does not
      affect function results, but shields against attacks which rely
      on any input-dependent behaviour.

      You should call this after secp256k1_context_create or
      secp256k1_context_clone, and may call this repeatedly
      afterwards. *)

end

module Secret : sig
  type t
  (** Opaque type of a valid ECDSA secret key. *)

  val compare : t -> t -> int

  val of_bytes : Context.t -> buffer -> t option
  val of_bytes_exn : Context.t -> buffer -> t
  (** Verify an ECDSA secret key. Buffer must be 32 bytes long. *)

end

module Public : sig
  type t
  (** Opaque data structure that holds a parsed and valid public
      key. *)

  val compare : t -> t -> int

  val of_bytes : Context.t -> buffer -> t option
  val of_bytes_exn : Context.t -> buffer -> t
  (** Parse a variable-length public key. This function supports
      parsing compressed (33 bytes, header byte 0x02 or 0x03),
      uncompressed (65 bytes, header byte 0x04), or hybrid (65 bytes,
      header byte 0x06 or 0x07) format public keys. *)

  val to_bytes : ?compress:bool -> Context.t -> t -> buffer
  (** Serialize a pubkey object into a serialized byte sequence. *)

  val of_secret : Context.t -> Secret.t -> t
  (** Compute the public key for a secret key. *)
end

module Sign : sig
  type t
  (** Opaque data structure that holds a parsed ECDSA signature. *)

  val compare : t -> t -> int

  val of_compact : Context.t -> buffer -> t option
  val of_compact_exn : Context.t -> buffer -> t
  (** Parse an ECDSA signature in compact (64 bytes) format. Buffer
      must be 64 bytes long. *)

  val of_der : Context.t -> buffer -> t option
  val of_der_exn : Context.t -> buffer -> t
  (** Parse a DER ECDSA signature. *)

  val to_compact : Context.t -> t -> buffer
  (** Serialize an ECDSA signature in compact (64 byte) format. *)

  val to_der : Context.t -> t -> buffer
  (** Serialize an ECDSA signature in DER format. *)

  val sign : Context.t -> seckey:Secret.t -> msg:buffer -> t
  (** Create an ECDSA signature. The created signature is always in
      lower-S form. Buffer must contain a 32-byte message hash. *)

  val verify : Context.t -> pubkey:Public.t -> msg:buffer -> signature:t -> bool
  (** Verify an ECDSA signature. To avoid accepting malleable
      signatures, only ECDSA signatures in lower-S form are
      accepted. *)
end

module RecoverableSign : sig
  type t
  (** Opaque data structure that holds a parsed ECDSA recoverable
      signature. *)

  val of_compact : Context.t -> buffer -> int -> t option
  val of_compact_exn : Context.t -> buffer -> int -> t
  (** Parse an ECDSA recoverable signature in compact (64 bytes)
      format. Buffer must be 64 bytes.  The third argument is the
      recovery id. *)

  val sign : Context.t -> seckey:Secret.t -> msg:buffer -> t
  (** Create an ECDSA recoverable signature. Buffer must contain
      a 32-byte message hash. *)
end
