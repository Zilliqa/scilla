exception Invalid_hex of string

val privkey_len : int
val pubkey_len : int
val signature_len : int

(* Generate a private key / public key pair. 
 * The output is a hex string pair representing 
 * privkey_len / pubkey_len bytes of the keys
 *)
val genKeyPair : unit -> (string * string)

(* Given private key, public key and message,
 * sign the message and return the signature.
 *)
val sign : string -> string -> string -> string

(* Given public key, message and a signature, verify
 * that the message was indeed signed by the public key.
 *)
val verify : string -> string -> string -> bool
