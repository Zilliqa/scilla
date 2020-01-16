(** Ipcmessage.proto Types *)

(** {2 Types} *)

type proto_scilla_val_map = { m : (string * proto_scilla_val) list }

and proto_scilla_val = Bval of bytes | Mval of proto_scilla_val_map

type proto_scilla_query = {
  name : string;
  mapdepth : int;
  indices : bytes list;
  ignoreval : bool;
}

(** {2 Default values} *)

val default_proto_scilla_val_map :
  ?m:(string * proto_scilla_val) list -> unit -> proto_scilla_val_map
(** [default_proto_scilla_val_map ()] is the default value for type [proto_scilla_val_map] *)

val default_proto_scilla_val : unit -> proto_scilla_val
(** [default_proto_scilla_val ()] is the default value for type [proto_scilla_val] *)

val default_proto_scilla_query :
  ?name:string ->
  ?mapdepth:int ->
  ?indices:bytes list ->
  ?ignoreval:bool ->
  unit ->
  proto_scilla_query
(** [default_proto_scilla_query ()] is the default value for type [proto_scilla_query] *)
