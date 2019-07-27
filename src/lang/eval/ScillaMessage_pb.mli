(*
  This file is part of scilla.

  Copyright (c) 2018 - present Zilliqa Research Pvt. Ltd.
  
  scilla is free software: you can redistribute it and/or modify it under the
  terms of the GNU General Public License as published by the Free Software
  Foundation, either version 3 of the License, or (at your option) any later
  version.
 
  scilla is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
  A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
 
  You should have received a copy of the GNU General Public License along with
  scilla.  If not, see <http://www.gnu.org/licenses/>.
*)

(** ScillaMessage.proto Binary Encoding *)


(** {2 Protobuf Encoding} *)

val encode_proto_scilla_val_map : ScillaMessageTypes.proto_scilla_val_map -> Pbrt.Encoder.t -> unit
(** [encode_proto_scilla_val_map v encoder] encodes [v] with the given [encoder] *)

val encode_proto_scilla_val : ScillaMessageTypes.proto_scilla_val -> Pbrt.Encoder.t -> unit
(** [encode_proto_scilla_val v encoder] encodes [v] with the given [encoder] *)

val encode_proto_scilla_query : ScillaMessageTypes.proto_scilla_query -> Pbrt.Encoder.t -> unit
(** [encode_proto_scilla_query v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_proto_scilla_val_map : Pbrt.Decoder.t -> ScillaMessageTypes.proto_scilla_val_map
(** [decode_proto_scilla_val_map decoder] decodes a [proto_scilla_val_map] value from [decoder] *)

val decode_proto_scilla_val : Pbrt.Decoder.t -> ScillaMessageTypes.proto_scilla_val
(** [decode_proto_scilla_val decoder] decodes a [proto_scilla_val] value from [decoder] *)

val decode_proto_scilla_query : Pbrt.Decoder.t -> ScillaMessageTypes.proto_scilla_query
(** [decode_proto_scilla_query decoder] decodes a [proto_scilla_query] value from [decoder] *)
