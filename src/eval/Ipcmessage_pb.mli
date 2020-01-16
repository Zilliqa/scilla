(** Ipcmessage.proto Binary Encoding *)

(** {2 Protobuf Encoding} *)

val encode_proto_scilla_val_map :
  Ipcmessage_types.proto_scilla_val_map -> Pbrt.Encoder.t -> unit
(** [encode_proto_scilla_val_map v encoder] encodes [v] with the given [encoder] *)

val encode_proto_scilla_val :
  Ipcmessage_types.proto_scilla_val -> Pbrt.Encoder.t -> unit
(** [encode_proto_scilla_val v encoder] encodes [v] with the given [encoder] *)

val encode_proto_scilla_query :
  Ipcmessage_types.proto_scilla_query -> Pbrt.Encoder.t -> unit
(** [encode_proto_scilla_query v encoder] encodes [v] with the given [encoder] *)

(** {2 Protobuf Decoding} *)

val decode_proto_scilla_val_map :
  Pbrt.Decoder.t -> Ipcmessage_types.proto_scilla_val_map
(** [decode_proto_scilla_val_map decoder] decodes a [proto_scilla_val_map] value from [decoder] *)

val decode_proto_scilla_val :
  Pbrt.Decoder.t -> Ipcmessage_types.proto_scilla_val
(** [decode_proto_scilla_val decoder] decodes a [proto_scilla_val] value from [decoder] *)

val decode_proto_scilla_query :
  Pbrt.Decoder.t -> Ipcmessage_types.proto_scilla_query
(** [decode_proto_scilla_query decoder] decodes a [proto_scilla_query] value from [decoder] *)
