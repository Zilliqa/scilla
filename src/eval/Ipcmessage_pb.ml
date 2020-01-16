[@@@ocaml.warning "-27-30-39"]

type proto_scilla_val_map_mutable = {
  mutable m : (string * Ipcmessage_types.proto_scilla_val) list;
}

let default_proto_scilla_val_map_mutable () : proto_scilla_val_map_mutable =
  { m = [] }

type proto_scilla_query_mutable = {
  mutable name : string;
  mutable mapdepth : int;
  mutable indices : bytes list;
  mutable ignoreval : bool;
}

let default_proto_scilla_query_mutable () : proto_scilla_query_mutable =
  { name = ""; mapdepth = 0; indices = []; ignoreval = false }

let rec decode_proto_scilla_val_map d =
  let v = default_proto_scilla_val_map_mutable () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
        v.m <- List.rev v.m;
        continue__ := false
    | Some (1, Pbrt.Bytes) ->
        let decode_value d = decode_proto_scilla_val (Pbrt.Decoder.nested d) in
        v.m <-
          Pbrt.Decoder.map_entry d ~decode_key:Pbrt.Decoder.string ~decode_value
          :: v.m
    | Some (1, pk) ->
        Pbrt.Decoder.unexpected_payload
          "Message(proto_scilla_val_map), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({ Ipcmessage_types.m = v.m } : Ipcmessage_types.proto_scilla_val_map)

and decode_proto_scilla_val d =
  let rec loop () =
    let ret : Ipcmessage_types.proto_scilla_val =
      match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "proto_scilla_val"
      | Some (1, _) -> Ipcmessage_types.Bval (Pbrt.Decoder.bytes d)
      | Some (2, _) ->
          Ipcmessage_types.Mval
            (decode_proto_scilla_val_map (Pbrt.Decoder.nested d))
      | Some (n, payload_kind) ->
          Pbrt.Decoder.skip d payload_kind;
          loop ()
    in
    ret
  in
  loop ()

let rec decode_proto_scilla_query d =
  let v = default_proto_scilla_query_mutable () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
        v.indices <- List.rev v.indices;
        continue__ := false
    | Some (1, Pbrt.Bytes) -> v.name <- Pbrt.Decoder.string d
    | Some (1, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(proto_scilla_query), field(1)"
          pk
    | Some (2, Pbrt.Varint) -> v.mapdepth <- Pbrt.Decoder.int_as_varint d
    | Some (2, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(proto_scilla_query), field(2)"
          pk
    | Some (3, Pbrt.Bytes) -> v.indices <- Pbrt.Decoder.bytes d :: v.indices
    | Some (3, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(proto_scilla_query), field(3)"
          pk
    | Some (4, Pbrt.Varint) -> v.ignoreval <- Pbrt.Decoder.bool d
    | Some (4, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(proto_scilla_query), field(4)"
          pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ( {
      Ipcmessage_types.name = v.name;
      Ipcmessage_types.mapdepth = v.mapdepth;
      Ipcmessage_types.indices = v.indices;
      Ipcmessage_types.ignoreval = v.ignoreval;
    }
    : Ipcmessage_types.proto_scilla_query )

let rec encode_proto_scilla_val_map (v : Ipcmessage_types.proto_scilla_val_map)
    encoder =
  let encode_key = Pbrt.Encoder.string in
  let encode_value x encoder =
    Pbrt.Encoder.nested (encode_proto_scilla_val x) encoder
  in
  List.iter
    (fun (k, v) ->
      Pbrt.Encoder.key (1, Pbrt.Bytes) encoder;
      let map_entry = ((k, Pbrt.Bytes), (v, Pbrt.Bytes)) in
      Pbrt.Encoder.map_entry ~encode_key ~encode_value map_entry encoder)
    v.Ipcmessage_types.m;
  ()

and encode_proto_scilla_val (v : Ipcmessage_types.proto_scilla_val) encoder =
  match v with
  | Ipcmessage_types.Bval x ->
      Pbrt.Encoder.key (1, Pbrt.Bytes) encoder;
      Pbrt.Encoder.bytes x encoder
  | Ipcmessage_types.Mval x ->
      Pbrt.Encoder.key (2, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_proto_scilla_val_map x) encoder

let rec encode_proto_scilla_query (v : Ipcmessage_types.proto_scilla_query)
    encoder =
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder;
  Pbrt.Encoder.string v.Ipcmessage_types.name encoder;
  Pbrt.Encoder.key (2, Pbrt.Varint) encoder;
  Pbrt.Encoder.int_as_varint v.Ipcmessage_types.mapdepth encoder;
  List.iter
    (fun x ->
      Pbrt.Encoder.key (3, Pbrt.Bytes) encoder;
      Pbrt.Encoder.bytes x encoder)
    v.Ipcmessage_types.indices;
  Pbrt.Encoder.key (4, Pbrt.Varint) encoder;
  Pbrt.Encoder.bool v.Ipcmessage_types.ignoreval encoder;
  ()
