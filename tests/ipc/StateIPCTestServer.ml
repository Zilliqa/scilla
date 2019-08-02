open Core
open Result.Let_syntax
open MonadUtil
open Syntax
open JSON
open ParserUtil
open TypeUtil
open StateIPCIdl
open ErrorUtils

module IPCTestServer = IPCIdl(Idl.GenServer ())

type hashtable =  (string, value_type) Hashtbl_intf.Hashtbl.t

and value_type =
  | NonMapVal of string
  | MapVal of hashtable

let num_pending_requests = 5
let permission = 0o0755
let balance_label = "_balance"
let table = Hashtbl.create (module String)

let mkdir_rec dir perm =
  let rec p_mkdir dir =
    let p_name = Filename.dirname dir in
    if p_name <> "/" && p_name <> "."
    then p_mkdir p_name;
    (try Unix.mkdir dir ~perm with Unix.Unix_error(Unix.EEXIST, _, _) -> ()) in
  p_mkdir dir

let finally f g conn =
  try
    f conn;
    g conn;
  with e ->
    g conn;
    raise e

(* Send msg with delimiting character "0xA". *)
let send_delimited oc msg =
  let msg' = msg ^ "\n" in
  Caml.output_string oc msg';
  Caml.flush oc

let binary_rpc conn =
  let ic = Unix.in_channel_of_descr conn in
  let oc = Unix.out_channel_of_descr conn in 
  let request = Jsonrpc.call_of_string (Caml.input_line ic) in
  let response = (Idl.server IPCTestServer.implementation) request in
  send_delimited oc (Jsonrpc.string_of_response response)

let serve_requests sock_addr =
  (try Unix.unlink sock_addr with Unix.Unix_error(Unix.ENOENT, _, _) -> ());
  mkdir_rec (Filename.dirname sock_addr) permission;
  let socket = Unix.socket ~domain: Unix.PF_UNIX ~kind: Unix.SOCK_STREAM ~protocol: 0 in
  Unix.bind socket ~addr: (Unix.ADDR_UNIX sock_addr);
  Unix.listen socket ~backlog: num_pending_requests;
  Printf.fprintf stdout "Listening on %s" sock_addr;
  while true do
    let conn, _ = Unix.accept socket in
    let (_: Thread.t) = Thread.create
      (fun connection ->
        finally binary_rpc Unix.close connection
      ) conn in
    ()
  done

let decode_serialized_value value =
  let decoder = Pbrt.Decoder.of_bytes (Bytes.of_string value) in
  ScillaMessage_pb.decode_proto_scilla_val decoder

let encode_serialized_value value =
  let encoder = Pbrt.Encoder.create () in
  ScillaMessage_pb.encode_proto_scilla_val value encoder;
  Bytes.to_string @@ Pbrt.Encoder.to_bytes encoder

let decode_serialized_query query =
  let decoder = Pbrt.Decoder.of_bytes (Bytes.of_string query) in
  ScillaMessage_pb.decode_proto_scilla_query decoder

let json_exn_wrapper ?filename thunk  =
  try
    thunk ()
  with
    | Yojson.Json_error s
    | Yojson.Basic.Util.Undefined (s, _)
    | Yojson.Basic.Util.Type_error (s, _)
      -> raise (mk_invalid_json s)
    | _ ->
      (match filename with
      | Some f -> raise (mk_invalid_json (Printf.sprintf "Unknown error parsing JSON %s" f))
      | None -> raise (mk_invalid_json (Printf.sprintf "Unknown error parsing JSON"))
      )

let from_file f =
  let thunk () = Yojson.Basic.from_file f in
  json_exn_wrapper thunk ~filename:f

let to_list_exn j =
  let thunk() = Yojson.Basic.Util.to_list j in
  json_exn_wrapper thunk

let get_json_data filename =
  let json = from_file filename in
  (* input json is a list of key/value pairs *)
  let jlist = json |> to_list_exn in
  jlist

let member_exn m j =
  let thunk () = Yojson.Basic.Util.member m j in
  let v = json_exn_wrapper thunk in
  match v with
  | `Null -> raise (mk_invalid_json ("Member '" ^ m ^ "' not found in json"))
  | j -> j

let to_string_exn j =
  let thunk() = Yojson.Basic.Util.to_string j in
  json_exn_wrapper thunk

let parse_typ_exn t =
  match FrontEndParser.parse_type t with
  | Error _ -> raise (mk_invalid_json (sprintf "Invalid type in json: %s\n" t))
  | Ok s -> s

let rec populate_table table json_list =
  match json_list with
  | [] -> ()
  | first :: rest ->
    let name = member_exn "vname" first |> to_string_exn in
    let tstring = member_exn "type" first |> to_string_exn in
    let t = parse_typ_exn tstring in
    let value = member_exn "value" first in
    let _ = (match t with
    | MapType (_, _) ->
      let new_table = Hashtbl.create (module String) in
      let _ = (match value with
      | `List vli -> populate_table new_table vli
      | `Null -> ()
      | _ -> raise (mk_invalid_json ("JSON parsing: error parsing Map"))) in
      Hashtbl.set table ~key:name ~data: (MapVal new_table)
      (* let _ = populate_table new_table in *)
    | _ -> Hashtbl.set table ~key:name ~data: (NonMapVal (value |> to_string_exn))) in
    populate_table table rest


(* let fetch_state_value ~query =
  let query = decode_serialized_query (Bytes.of_string query) in

let update_state_value ~query ~value =
  let query = decode_serialized_query (Bytes.of_string query) in
  match query with
  | { name; indices; _ } ->
    let%bind value' = decode_serialized_value Bytes.of_string value in *)


let start_server ~sock_addr ~state_json_path =
  IPCTestServer.fetchStateValue fetch_state_value;
  IPCTestServer.updateStateValue update_state_value;

  let _ = (match state_json_path with 
  | Some path ->
    let json_list = get_json_data path in
    populate_table table json_list
  | None -> ()) in

  serve_requests sock_addr