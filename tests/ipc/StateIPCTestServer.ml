open StateIPCIdl
open StateService
open Core

module IPCTestServer = IPCIdl(Idl.GenServer ())

let num_pending_requests = 5
let permission = 0o0755
let balance_label = "_balance"

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

let fetch_state_value ~query = 

(*let update_state_value ~query ~value = *)


(* let start_server ~sock_addr ~state_json_path =
  IPCTestServer.fetchStateValue fetch_state_value;
  IPCTestServer.updateStateValue update_state_value;

  (* Read json and initialize *)
  let _ = (match state_json_path with 
  | Some path ->
    let states = JSON.ContractState.get_json_data path in
    let match_balance ((vname : string), _) : bool = vname = balance_label in
    let no_bal_states = List.filter states ~f:(fun c -> not @@ match_balance c) in

    let fields = List.map field_vals ~f:(fun (s, l) ->
      let t = List.Assoc.find_exn cstate.fields ~equal:(=) s in
      { fname = s; ftyp = t; fval = Some l }
    ) in
    initialize ~sm: Local ~fields
  | None -> ()) in
  serve_requests sock_addr *)