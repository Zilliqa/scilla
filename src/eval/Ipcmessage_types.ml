[@@@ocaml.warning "-27-30-39"]

type proto_scilla_val_map = { m : (string * proto_scilla_val) list }

and proto_scilla_val = Bval of bytes | Mval of proto_scilla_val_map

type proto_scilla_query = {
  name : string;
  mapdepth : int;
  indices : bytes list;
  ignoreval : bool;
}

let rec default_proto_scilla_val_map
    ?(m : (string * proto_scilla_val) list = []) () : proto_scilla_val_map =
  { m }

and default_proto_scilla_val () : proto_scilla_val = Bval (Bytes.create 0)

let rec default_proto_scilla_query ?(name : string = "") ?(mapdepth : int = 0)
    ?(indices : bytes list = []) ?(ignoreval : bool = false) () :
    proto_scilla_query =
  { name; mapdepth; indices; ignoreval }
