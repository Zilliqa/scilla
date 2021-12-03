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

(* This file implements a state server that mimics the behaviour of the
 * actual blockchain state server. It begins with an empty state and hence
 * must be "updated" to have the required state by calling update_state_value
 * before the contract is executed. *)

open Core
open Result.Let_syntax
open Scilla_base
open Scilla_eval
open MonadUtil
open StateIPCIdl
open IPCUtil
module Hashtbl = Caml.Hashtbl

type value_table = (string, value_type) Hashtbl.t

and value_type = NonMapVal of string | MapVal of value_table

type type_table = (string, string) Hashtbl.t

(* State of the full blockchain, i.e., for all addresses.
 * None indicates *this* address and Some indicates "external state". 
 * The blockchain info (BLOCKNUMBER etc) is stored in the *this* key. *)
type blockchain_state =
  ( string option,
    value_table * type_table * StateService.bcinfo_state )
  Hashtbl.t

let thread_pool : (string, blockchain_state) Hashtbl.t = Hashtbl.create 4

let num_pending_requests = 5

let fetch_message = "Fetching state value failed"

let update_message = "Updating state value failed"

let fail a = Error a

let decode_serialized_value value =
  let decoder = Pbrt.Decoder.of_bytes (Bytes.of_string value) in
  Ipcmessage_pb.decode_proto_scilla_val decoder

let encode_serialized_value value =
  let encoder = Pbrt.Encoder.create () in
  Ipcmessage_pb.encode_proto_scilla_val value encoder;
  Bytes.to_string @@ Pbrt.Encoder.to_bytes encoder

let decode_serialized_query query =
  let decoder = Pbrt.Decoder.of_bytes (Bytes.of_string query) in
  Ipcmessage_pb.decode_proto_scilla_query decoder

module M = Idl.IdM
module IDL = Idl.Make (M)

module MakeServer () = struct
  module IPCTestServer = IPCIdl (IDL.GenServer ())

  (* Global state of the server thread. *)
  let table : blockchain_state = Hashtbl.create 8

  let binary_rpc conn =
    let ic = Unix.in_channel_of_descr conn in
    let oc = Unix.out_channel_of_descr conn in
    let request = Jsonrpc.call_of_string (Caml.input_line ic) in
    let response = (IDL.server IPCTestServer.implementation) request |> M.run in
    IPCUtil.send_delimited oc (Jsonrpc.string_of_response response)

  let prepare_server sock_addr =
    (try Unix.unlink sock_addr with Unix.Unix_error (Unix.ENOENT, _, _) -> ());
    let socket =
      Unix.socket ~domain:Unix.PF_UNIX ~kind:Unix.SOCK_STREAM ~protocol:0 ()
    in
    Unix.bind socket ~addr:(Unix.ADDR_UNIX sock_addr);
    Unix.listen socket ~backlog:num_pending_requests;
    let server () =
      while true do
        let conn, _ = Unix.accept socket in
        binary_rpc conn;
        Unix.close conn
      done
    in
    server

  let rec serialize_value value =
    match value with
    | NonMapVal v -> Ipcmessage_types.Bval (Bytes.of_string v)
    | MapVal m ->
        let map_list = Hashtbl.fold (fun k v acc -> (k, v) :: acc) m [] in
        let serialized_map_list =
          List.map map_list ~f:(fun (str, v) -> (str, serialize_value v))
        in
        Ipcmessage_types.Mval { m = serialized_map_list }

  let rec deserialize_value value =
    match value with
    | Ipcmessage_types.Bval v -> NonMapVal (Bytes.to_string v)
    | Ipcmessage_types.Mval m_list ->
        let new_table = Hashtbl.create (List.length m_list.m) in
        List.iter m_list.m ~f:(fun (str, v) ->
            Hashtbl.replace new_table str (deserialize_value v));
        MapVal new_table

  let fetch_state_value_helper addr_opt query =
    let rec recurser value indices =
      match indices with
      | [] -> pure (Some value)
      | head :: tail -> (
          match value with
          | NonMapVal _ -> fail RPCError.{ code = 0; message = fetch_message }
          | MapVal m -> (
              let vopt = Hashtbl.find_opt m head in
              match vopt with Some v -> recurser v tail | None -> pure None))
    in
    let contr_state = Hashtbl.find_opt table addr_opt in
    match contr_state with
    | Some (vt, tt, _bcinfo) -> (
        let query = decode_serialized_query query in
        let t = Hashtbl.find_opt tt query.name in
        match query with
        | { name; indices; ignoreval; _ } -> (
            let string_indices_list =
              name :: List.map indices ~f:Bytes.to_string
            in
            let%bind vopt = recurser (MapVal vt) string_indices_list in
            match vopt with
            | Some v ->
                if ignoreval then pure @@ (true, "", t)
                else
                  pure @@ (true, encode_serialized_value (serialize_value v), t)
            | None -> pure @@ (false, "", t)))
    | None -> pure (false, "", None)

  let set_value_helper addr_opt query value ty_opt =
    let rec recurser_update ?(new_val = None) map indices =
      match indices with
      | [] -> fail RPCError.{ code = 0; message = update_message }
      | [ index ] -> (
          pure
          @@
          match new_val with
          | None -> Hashtbl.remove map index
          | Some v -> Hashtbl.replace map index v)
      | head :: tail -> (
          let vopt = Hashtbl.find_opt map head in
          match vopt with
          | None -> (
              (* Index does not exist. If we are deleting a value, then we can ignore this and all remaining indices *)
              match new_val with
              | None -> pure ()
              | Some _ ->
                  let m = Hashtbl.create 8 in
                  let () = Hashtbl.replace map head (MapVal m) in
                  recurser_update ~new_val m tail)
          | Some v -> (
              match v with
              | NonMapVal _ ->
                  fail RPCError.{ code = 0; message = update_message }
              | MapVal m -> recurser_update ~new_val m tail))
    in
    let query = decode_serialized_query query in
    let vt, tt, _bcinfo =
      match Hashtbl.find_opt table addr_opt with
      | Some (vt, tt, bcinfo) -> (vt, tt, bcinfo)
      | None ->
          let vt, tt, bcinfo =
            (Hashtbl.create 8, Hashtbl.create 8, Hashtbl.create 1)
          in
          Hashtbl.replace table addr_opt (vt, tt, bcinfo);
          (vt, tt, bcinfo)
    in
    (* Update type if provided *)
    let () =
      match ty_opt with
      | Some ty -> Hashtbl.replace tt query.name ty
      | None -> ()
    in
    (* Update value *)
    match query with
    | { name; indices; ignoreval; _ } -> (
        let string_indices_list = List.map indices ~f:Bytes.to_string in
        match ignoreval with
        | true -> recurser_update vt (name :: string_indices_list)
        | false ->
            let new_val = deserialize_value (decode_serialized_value value) in
            recurser_update ~new_val:(Some new_val) vt
              (name :: string_indices_list))

  let fetch_state_value query =
    let%bind f, v, _t = fetch_state_value_helper None query in
    pure (f, v)

  let fetch_ext_state_value caddr query =
    let%bind f, v, t = fetch_state_value_helper (Some caddr) query in
    match t with Some t' -> pure (f, v, t') | None -> pure (false, "", "")

  let update_state_value query value = set_value_helper None query value None

  let set_ext_state_value caddr query value scilla_type =
    set_value_helper (Some caddr) query value (Some scilla_type)

  let set_bcinfo ~query_name ~query_args value =
    let bcit =
      match Hashtbl.find_opt table None with
      | Some (_, _, bcit) -> bcit
      | None ->
          let ((_, _, bcit) as entry) =
            (Hashtbl.create 8, Hashtbl.create 8, Hashtbl.create 1)
          in
          Hashtbl.replace table None entry;
          bcit
    in
    (match Hashtbl.find_opt bcit query_name with
    | Some subm -> Hashtbl.replace subm query_args value
    | None ->
        let subm = Hashtbl.create 1 in
        Hashtbl.replace subm query_args value;
        Hashtbl.add bcit query_name subm);
    pure ()

  let fetch_bcinfo ~query_name ~query_args =
    let _, _, bcit = Hashtbl.find table None in
    match Hashtbl.find_opt bcit query_name with
    | Some subm -> (
        match Hashtbl.find_opt subm query_args with
        | Some value -> pure (true, value)
        | None -> pure (false, ""))
    | None -> pure (false, "")
end

let start_server ~sock_addr =
  (* Check if we already have a thread to serve this socket. *)
  match Hashtbl.find_opt thread_pool sock_addr with
  | Some _ -> () (* There's already a server running. Nothing to do. *)
  | None ->
      let module ServerModule = MakeServer () in
      ServerModule.IPCTestServer.fetch_state_value (fun q ->
          IDL.T.return @@ ServerModule.fetch_state_value q);
      ServerModule.IPCTestServer.update_state_value (fun q v ->
          IDL.T.return @@ ServerModule.update_state_value q v);
      ServerModule.IPCTestServer.set_ext_state_value (fun addr q v t ->
          IDL.T.return @@ ServerModule.set_ext_state_value addr q v t);
      ServerModule.IPCTestServer.set_bcinfo (fun query_name query_args value ->
          IDL.T.return @@ ServerModule.set_bcinfo ~query_name ~query_args value);
      ServerModule.IPCTestServer.fetch_bcinfo (fun query_name query_args ->
          IDL.T.return @@ ServerModule.fetch_bcinfo ~query_name ~query_args);
      ServerModule.IPCTestServer.fetch_ext_state_value (fun a q ->
          IDL.T.return @@ ServerModule.fetch_ext_state_value a q);
      let server = ServerModule.prepare_server sock_addr in
      let _ = Thread.create ~on_uncaught_exn:`Kill_whole_process server () in
      Hashtbl.replace thread_pool sock_addr ServerModule.table

let stop_server ~sock_addr =
  match Hashtbl.find_opt thread_pool sock_addr with
  | Some h ->
      (* Just reset the table of this server. *)
      Hashtbl.clear h
  | None -> ()

(* Nothing to do. *)
