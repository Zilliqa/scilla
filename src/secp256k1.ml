open StdLabels

module BA = struct
  include Bigarray.Array1

  let length = size_in_bytes

  let rec compare_rec a b i len_a len_b =
    if i=len_a && i=len_b then 0
    else if i=len_a then -1
    else if i=len_b then 1
    else
      match Char.compare (get a i) (get b i) with
      | 0 -> compare_rec a b (i+1) len_a len_b
      | n -> n

  let compare a b =
    compare_rec a b 0 (length a) (length b)
end

type buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

module Context = struct

  type flag = Sign | Verify

  let int_of_flag = function
    | Verify -> 1
    | Sign -> 2

  type t

  external create : int -> t = "ml_secp256k1_context_create"
  external clone : t -> t = "ml_secp256k1_context_clone"
  external randomize : t -> buffer -> bool = "ml_secp256k1_context_randomize"

  let create flags =
    let flags =
      List.fold_left flags ~init:0 ~f:(fun a f -> a lor (int_of_flag f)) in
    create flags

  let randomize ctx buf =
    if BA.length buf <> 32 then
      invalid_arg "Context.randomize: input must be 32 bytes long" ;
    randomize ctx buf
end

module Secret = struct
  external verify :
    Context.t -> buffer -> bool = "ml_secp256k1_ec_seckey_verify"

  type t = buffer

  let compare = BA.compare

  let of_bytes ctx buf =
    if BA.length buf <> 32 then
      invalid_arg "Secret.of_bytes: inputs must be 32 bytes long" ;
    match verify ctx buf with
    | true -> Some buf
    | false -> None

  let of_bytes_exn ctx buf =
    match of_bytes ctx buf with
    | None -> failwith "Secret.of_bytes_exn"
    | Some sk -> sk
end

module Public = struct
  type t = buffer

  let compare = BA.compare

  external of_bytes :
    Context.t -> buffer -> t = "ml_secp256k1_ec_pubkey_parse"

  external to_bytes :
    Context.t -> t -> bool -> buffer = "ml_secp256k1_ec_pubkey_serialize"

  external of_secret :
    Context.t -> Secret.t -> t = "ml_secp256k1_ec_pubkey_create"

  let of_bytes ctx buf =
    if BA.(length buf <> 33 && length buf <> 65) then
      invalid_arg "Public.of_bytes: input must be either 33 or 65 bytes long" ;
    try Some (of_bytes ctx buf) with _ -> None

  let of_bytes_exn ctx buf =
    match of_bytes ctx buf with
    | None -> failwith "Public.of_bytes_exn"
    | Some pk -> pk

  let to_bytes ?(compress=true) ctx t =
    to_bytes ctx t compress
end

module Sign = struct
  type t = buffer

  let compare = BA.compare

  external parse :
    Context.t -> buffer -> bool -> t = "ml_secp256k1_ecdsa_signature_parse"
  external serialize :
    Context.t -> t -> bool -> buffer = "ml_secp256k1_ecdsa_signature_serialize"

  let to_compact ctx t = serialize ctx t true
  let to_der ctx t = serialize ctx t false

  let of_compact ctx buf =
    try Some (parse ctx buf true) with _ -> None

  let of_compact_exn ctx buf =
    match of_compact ctx buf with
    | None -> failwith "Sign.of_compact_exn"
    | Some signature -> signature

  let of_der ctx buf =
    try Some (parse ctx buf false) with _ -> None

  let of_der_exn ctx buf =
    match of_der ctx buf with
    | None -> failwith "Sign.of_der_exn"
    | Some signature -> signature

  external sign :
    Context.t -> Secret.t -> buffer -> t = "ml_secp256k1_ecdsa_sign"

  external verify :
    Context.t -> Public.t -> buffer -> t -> bool = "ml_secp256k1_ecdsa_verify"

  let sign ctx ~seckey ~msg =
    if BA.length msg <> 32 then
      invalid_arg "Sign.sign: msg must be 32 bytes long" ;
    sign ctx seckey msg

  let verify ctx ~pubkey ~msg ~signature =
    if BA.length msg <> 32 then
      invalid_arg "Sign.verify: msg must be 32 bytes long" ;
    verify ctx pubkey msg signature
end
