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

[@@@ocaml.warning "-27-30-39"]

type proto_scilla_val_map_mutable = {
  mutable m : (string * ScillaMessageTypes.proto_scilla_val) list;
}

let default_proto_scilla_val_map_mutable () : proto_scilla_val_map_mutable = {
  m = [];
}

type proto_scilla_query_mutable = {
  mutable name : string;
  mutable mapdepth : int;
  mutable indices : bytes list;
  mutable deletemapkey : bool;
}

let default_proto_scilla_query_mutable () : proto_scilla_query_mutable = {
  name = "";
  mapdepth = 0;
  indices = [];
  deletemapkey = false;
}


let rec decode_proto_scilla_val_map d =
  let v = default_proto_scilla_val_map_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.m <- List.rev v.m;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      let decode_value = (fun d ->
        decode_proto_scilla_val (Pbrt.Decoder.nested d)
      ) in
      v.m <- (
        (Pbrt.Decoder.map_entry d ~decode_key:Pbrt.Decoder.string ~decode_value)::v.m;
      );
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(proto_scilla_val_map), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    ScillaMessageTypes.m = v.m;
  } : ScillaMessageTypes.proto_scilla_val_map)

and decode_proto_scilla_val d = 
  let rec loop () = 
    let ret:ScillaMessageTypes.proto_scilla_val = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "proto_scilla_val"
      | Some (1, _) -> ScillaMessageTypes.Bval (Pbrt.Decoder.bytes d)
      | Some (2, _) -> ScillaMessageTypes.Mval (decode_proto_scilla_val_map (Pbrt.Decoder.nested d))
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

let rec decode_proto_scilla_query d =
  let v = default_proto_scilla_query_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.indices <- List.rev v.indices;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.name <- Pbrt.Decoder.string d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(proto_scilla_query), field(1)" pk
    | Some (2, Pbrt.Varint) -> begin
      v.mapdepth <- Pbrt.Decoder.int_as_varint d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(proto_scilla_query), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.indices <- (Pbrt.Decoder.bytes d) :: v.indices;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(proto_scilla_query), field(3)" pk
    | Some (4, Pbrt.Varint) -> begin
      v.deletemapkey <- Pbrt.Decoder.bool d;
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(proto_scilla_query), field(4)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    ScillaMessageTypes.name = v.name;
    ScillaMessageTypes.mapdepth = v.mapdepth;
    ScillaMessageTypes.indices = v.indices;
    ScillaMessageTypes.deletemapkey = v.deletemapkey;
  } : ScillaMessageTypes.proto_scilla_query)

let rec encode_proto_scilla_val_map (v:ScillaMessageTypes.proto_scilla_val_map) encoder = 
  let encode_key = Pbrt.Encoder.string in
  let encode_value = (fun x encoder ->
    Pbrt.Encoder.nested (encode_proto_scilla_val x) encoder;
  ) in
  List.iter (fun (k, v) ->
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    let map_entry = (k, Pbrt.Bytes), (v, Pbrt.Bytes) in
    Pbrt.Encoder.map_entry ~encode_key ~encode_value map_entry encoder
  ) v.ScillaMessageTypes.m;
  ()

and encode_proto_scilla_val (v:ScillaMessageTypes.proto_scilla_val) encoder = 
  begin match v with
  | ScillaMessageTypes.Bval x ->
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.bytes x encoder;
  | ScillaMessageTypes.Mval x ->
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_proto_scilla_val_map x) encoder;
  end

let rec encode_proto_scilla_query (v:ScillaMessageTypes.proto_scilla_query) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.ScillaMessageTypes.name encoder;
  Pbrt.Encoder.key (2, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int_as_varint v.ScillaMessageTypes.mapdepth encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.bytes x encoder;
  ) v.ScillaMessageTypes.indices;
  Pbrt.Encoder.key (4, Pbrt.Varint) encoder; 
  Pbrt.Encoder.bool v.ScillaMessageTypes.deletemapkey encoder;
  ()
