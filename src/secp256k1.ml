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

  let create len =
    Bigarray.(create char c_layout len)
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

  let length = 32

  type t = buffer

  let compare = BA.compare

  let read ctx ?(pos=0) buf =
    let buflen = BA.length buf in
    if pos < 0 || pos > buflen - length then
      invalid_arg "Secret.of_bytes: pos < 0 or pos + 32 > buflen" ;
    let buf = BA.sub buf pos length in
    match verify ctx buf with
    | true ->
      let t = BA.create 32 in
      BA.blit buf t ;
      Some buf
    | false -> None

  let read_exn ctx ?pos buf =
    match read ctx buf with
    | None -> failwith "Secret.of_bytes_exn"
    | Some sk -> sk

  let write buf ?(pos=0) t =
    let buflen = BA.length buf in
    if pos < 0 || pos > buflen - length then
      invalid_arg "Secret.write: pos < 0 or pos + 32 > buflen" ;
    let buf = BA.sub buf pos length in
    BA.blit t buf

  let copy t =
    let t' = BA.create length in
    BA.blit t t' ;
    t'

  let to_bytes = copy

  external negate_inplace :
    Context.t -> buffer -> unit = "ml_secp256k1_ec_privkey_negate"
  external add_tweak_inplace :
    Context.t -> buffer -> buffer -> unit = "ml_secp256k1_ec_privkey_tweak_add"
  external mul_tweak_inplace :
    Context.t -> buffer -> buffer -> unit = "ml_secp256k1_ec_privkey_tweak_mul"

  let negate ctx t =
    let t' = copy t in
    negate_inplace ctx t' ;
    t'

  let op_tweak f ctx t ?(pos=0) buf =
    let buflen = BA.length buf in
    if pos < 0 || pos > buflen - 32 then
      invalid_arg "Secret.add_tweak: pos < 0 or pos > buflen - 32" ;
    let buf = BA.sub buf pos 32 in
    let t' = copy t in
    f ctx t' buf ;
    t'

  let add_tweak = op_tweak add_tweak_inplace
  let mul_tweak = op_tweak mul_tweak_inplace
end

module Public = struct
  type t = buffer

  let length = 64

  let compare = BA.compare

  external parse :
    Context.t -> buffer -> buffer -> unit = "ml_secp256k1_ec_pubkey_parse"

  external serialize :
    Context.t -> buffer -> t -> unit = "ml_secp256k1_ec_pubkey_serialize"

  external create :
    Context.t -> buffer -> Secret.t -> unit = "ml_secp256k1_ec_pubkey_create"

  let of_secret ctx seckey =
    let buf = BA.create length in
    create ctx buf seckey ;
    buf

  let read ctx ?(pos=0) inbuf =
    let pklen = BA.length inbuf in
    if pos < 0 || pos > pklen - 33 then
      invalid_arg "Public.of_bytes: pos < 0 or pos > buflen - 33" ;
    let inbuf = BA.(sub inbuf pos (length inbuf)) in
    if BA.(length inbuf < 33) then
      invalid_arg "Public.of_bytes: input must be at least 33 bytes long" ;
    let outbuf = BA.create length in
    try
      parse ctx outbuf inbuf ;
      Some outbuf
    with _ -> None

  let read_exn ctx ?pos buf =
    match read ctx ?pos buf with
    | None -> failwith "Public.of_bytes_exn"
    | Some pk -> pk

  let copy t =
    let t' = BA.create length in
    BA.blit t t' ;
    t'

  let to_bytes ?(compress=true) ctx t =
    let buf = BA.create (if compress then 33 else 65) in
    serialize ctx buf t ;
    buf

  let write ?(compress=true) ctx buf ?(pos=0) t =
    let buflen = BA.length buf in
    if pos < 0 || (compress && pos > buflen - 33) || pos > buflen - 65 then
      invalid_arg "Public.write: pos < 0 or pos > buflen - (33|65)" ;
    let len = if compress then 33 else 65 in
    let buf = BA.sub buf pos len in
    serialize ctx buf t

  external negate_inplace :
    Context.t -> buffer -> unit = "ml_secp256k1_ec_pubkey_negate"
  external add_tweak_inplace :
    Context.t -> buffer -> buffer -> unit = "ml_secp256k1_ec_pubkey_tweak_add"
  external mul_tweak_inplace :
    Context.t -> buffer -> buffer -> unit = "ml_secp256k1_ec_pubkey_tweak_mul"
  external combine :
    Context.t -> buffer -> buffer list -> unit = "ml_secp256k1_ec_pubkey_combine"

  let negate ctx t =
    let t' = copy t in
    negate_inplace ctx t' ;
    t'

  let op_tweak f ctx t ?(pos=0) buf =
    let buflen = BA.length buf in
    if pos < 0 || pos > buflen - 32 then
      invalid_arg "Secret.add_tweak: pos < 0 or pos > buflen - 32" ;
    let buf = BA.sub buf pos 32 in
    let t' = copy t in
    f ctx t' buf ;
    t'

  let add_tweak = op_tweak add_tweak_inplace
  let mul_tweak = op_tweak mul_tweak_inplace

  let combine ctx pks =
    let nb_pks = List.length pks in
    if nb_pks = 0 || nb_pks > 1024 then
      invalid_arg "Public.combine: please provide between 1 and 1024 pks" ;
    let ret = BA.create length in
    combine ctx ret pks ;
    ret
end

module Sign = struct
  type t = buffer

  let compare = BA.compare

  let length = 64

  external parse :
    Context.t -> buffer -> buffer -> bool -> unit = "ml_secp256k1_ecdsa_signature_parse"
  external serialize :
    Context.t -> buffer -> t -> bool -> int = "ml_secp256k1_ecdsa_signature_serialize"

  let to_compact ctx t =
    let buf = BA.create length in
    let (_:int) = serialize ctx buf t true in
    buf

  let to_der ctx t =
    let buf = BA.create 72 in
    let len = serialize ctx buf t false in
    BA.sub buf 0 len

  let write compact ctx buf ?(pos=0) t =
    let buf = BA.(sub buf pos (length buf)) in
    serialize ctx buf t compact

  let write_compact = write true
  let write_der = write false

  let of_compact ctx ?(pos=0) inbuf =
    let buflen = BA.length inbuf in
    if pos < 0 || pos > buflen - length then
      invalid_arg "Sign.of_compact: pos < 0 or pos > buflen - 64" ;
    let outbuf = BA.create length in
    try
      parse ctx outbuf (BA.sub inbuf pos length) true ;
      Some outbuf
    with _ -> None

  let of_compact_exn ctx ?pos buf =
    match of_compact ctx buf with
    | None -> failwith "Sign.of_compact_exn"
    | Some signature -> signature

  let of_der ctx ?(pos=0) inbuf =
    let buflen = BA.length inbuf in
    if pos < 0 || pos > buflen - length then
      invalid_arg "Sign.of_der: pos < 0 or pos > buflen - 72" ;
    let buf = BA.create length in
    try
      parse ctx buf BA.(sub inbuf pos (length inbuf)) false ;
      Some buf
    with _ -> None

  let of_der_exn ctx ?pos buf =
    match of_der ctx buf with
    | None -> failwith "Sign.of_der_exn"
    | Some signature -> signature

  external sign :
    Context.t -> buffer -> Secret.t -> buffer -> unit = "ml_secp256k1_ecdsa_sign"

  external verify :
    Context.t -> Public.t -> buffer -> t -> bool = "ml_secp256k1_ecdsa_verify"

  let write_sign ctx ~seckey ~outbuf ?(outpos=0) ~inbuf ?(inpos=0) () =
    let inbuflen = BA.length inbuf in
    if inpos < 0 || inpos > inbuflen - 32 then
      invalid_arg "Sign.sign: inpos < 0 or inpos > msglen - 32" ;
    let outbuflen = BA.length outbuf in
    if outpos < 0 || outpos > outbuflen - length then
      invalid_arg "Sign.write_sign: outpos < 0 or outpos > outbuf - 64" ;
    sign ctx (BA.sub outbuf outpos length) seckey (BA.sub inbuf inpos 32)

  let sign ctx ~seckey ?(pos=0) inbuf =
    let buflen = BA.length inbuf in
    if pos < 0 || pos > buflen - 32 then
      invalid_arg "Sign.sign: pos < 0 || pos > msglen - 32" ;
    let outbuf = BA.create length in
    sign ctx outbuf seckey (BA.sub inbuf pos 32) ;
    outbuf

  let verify ctx ~pubkey ~signature ?(pos=0) msg =
    let msglen = BA.length msg in
    if pos < 0 || pos > msglen - 32 then
      invalid_arg "Sign.verify: msg must be at least 32 bytes long" ;
    verify ctx pubkey (BA.sub msg pos 32) signature
end

module RecoverableSign = struct
  type t = buffer

  let length = 65

  let compare = BA.compare

  external parse :
    Context.t -> buffer -> buffer -> int -> unit = "ml_secp256k1_ecdsa_recoverable_signature_parse_compact"

  let of_compact ctx ~recid ?(pos=0) inbuf =
    let buflen = BA.length inbuf in
    if pos < 0 || pos > buflen - Sign.length then
      invalid_arg "RecoverableSign.of_compact: pos < 0 or pos > buflen - 64" ;
    let buf = BA.create length in
    try
      parse ctx buf (BA.sub inbuf pos Sign.length) recid ;
      Some buf
    with _ -> None

  let of_compact_exn ctx ~recid ?pos inbuf =
    match of_compact ctx ~recid ?pos inbuf with
    | None -> failwith "RecoverableSign.of_compact_exn"
    | Some signature -> signature

  external serialize :
    Context.t -> buffer -> t -> int = "ml_secp256k1_ecdsa_recoverable_signature_serialize_compact"

  let to_compact ctx sign =
    let buf = BA.create 64 in
    let recid = serialize ctx buf sign in
    buf, recid

  let write_compact ctx buf ?(pos=0) sign =
    let buflen = BA.length buf in
    if pos < 0 || pos > buflen - Sign.length then
      invalid_arg "RecoverableSign.write_compact: pos < 0 or pos > buflen - 64" ;
    serialize ctx (BA.sub buf pos Sign.length) sign

  external convert :
    Context.t -> buffer -> t -> unit = "ml_secp256k1_ecdsa_recoverable_signature_convert"

  let convert ctx sign =
    let buf = BA.create Sign.length in
    convert ctx buf sign ;
    buf

  external sign :
    Context.t -> buffer -> Secret.t -> buffer -> unit = "ml_secp256k1_ecdsa_sign_recoverable"

  let write_sign ctx ~seckey ~outbuf ?(outpos=0) ~inbuf ?(inpos=0) () =
    let inbuf_len = BA.length inbuf in
    if inpos < 0 || inpos > inbuf_len - 32 then
      invalid_arg "RecoverableSign.write_sign: inpos < 0 or inpos > inbuflen - 32" ;
    let outbuf_len = BA.length outbuf in
    if outpos < 0 || outpos > outbuf_len - length then
      invalid_arg "RecoverableSign.write_sign: outpos < 0 or outpos > outbuflen - 65" ;
    sign ctx (BA.sub outbuf outpos length) seckey (BA.sub inbuf inpos 32)

  let sign ctx ~seckey ?(pos=0) inbuf =
    let inbuflen = BA.length inbuf in
    if pos < 0 || pos > inbuflen - 32 then
      invalid_arg "RecoverableSign.sign: buf must be at least 32 bytes long" ;
    let outbuf = BA.create length in
    sign ctx outbuf seckey (BA.sub inbuf pos 32) ;
    outbuf

  external recover :
    Context.t -> buffer -> t -> buffer -> unit = "ml_secp256k1_ecdsa_recover"

  let recover ctx sign ?(pos=0) inbuf =
    let inbuflen = BA.length inbuf in
    if pos < 0 || pos > inbuflen - 32 then
      invalid_arg "RecoverableSign.recover: pos < 0 or pos > msglen - 32" ;
    let buf = BA.create Public.length in
    recover ctx buf sign (BA.sub inbuf pos 32) ;
    buf
end
