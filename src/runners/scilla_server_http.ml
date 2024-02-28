open Core
open Scilla_eval
open Opium
open Yojson.Safe
open Core
open Scilla_base
open Scilla_server_lib.Api
open IPCUtil
open ErrorUtils

module M = Idl.IdM
module IDL = Idl.Make (M)
module Server = API (IDL.GenServer ())

let mk_handler_no_args callback () =
  try IDL.ErrM.return @@ callback ()
  with FatalError msg ->
    IDL.ErrM.return_err RPCError.{ code = 0; message = msg }

(* Makes a handler that executes the given [callback] with [args] and returns it. **)
let mk_handler callback args =
  (* Force the -jsonerrors flag *)
  let args = "-jsonerrors" :: args in
  try IDL.ErrM.return @@ callback (Some args)
  with FatalError msg ->
    IDL.ErrM.return_err RPCError.{ code = 0; message = msg }

let server_implementation () =
  let runner args =
    let output, _ = Runner.run args ~exe_name:"scilla-runner" in
    Yojson.Basic.pretty_to_string output
  in
  let disambiguator args =
    Disambiguator.run args ~exe_name:"scilla-disambiguator"
  in
  let version () =
    let major, minor, patch = Syntax.scilla_version in
    Printf.sprintf "{ \"scilla_version\": \"%d.%d.%d\" }" major minor patch
  in
  (* Handlers *)
  Server.runner @@ mk_handler runner;
  Server.checker @@ mk_handler (Checker.run ~exe_name:"scilla-checker");
  Server.disambiguator @@ mk_handler disambiguator;
  Server.version @@ mk_handler_no_args version;
  Server.implementation

let run_handler req =
  let open Lwt.Syntax in
  let+ req = Request.to_plain_text req in
  let req = Jsonrpc.call_of_string req in

  let rpc = IDL.server (server_implementation ()) in
  let res =
    try M.run (rpc req)
    with e ->
      print_endline (Exn.to_string e);
      Rpc.failure
        (RPCError.rpc_of_t
           RPCError.
             { code = 0; message = "scilla-server: incorrect invocation" })
  in
  let str = Jsonrpc.string_of_response ~version:Jsonrpc.V2 res in
  
  Response.of_plain_text str
;;

let _ =
  App.empty
  |> App.post "/run" run_handler
  |> App.run_command
;;
