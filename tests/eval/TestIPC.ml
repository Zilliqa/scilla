open Core
open Result.Let_syntax

let () =
  let%bind response = StateIPCClient.test_server_rpc "/tmp/scillaipcsocketserver" "testQuery" in
  print_string response