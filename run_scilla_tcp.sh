#!/bin/bash

echo "Starting server." 1>&2

rm -rf myfifo 1>&2
mkfifo myfifo 1>&2

# netcat listen to 12346 and forward it to/from a local socket (this is the state queries/updates)
nc -lkv 12346 <myfifo | nc -lkUD /tmp/scilla-server.sock > myfifo &


# socat - every connection on 12345, run the script handle_connection and pipe the input to it (this is the initial run/create/etc commands)
socat TCP-LISTEN:12345,reuseaddr,fork EXEC:"/scilla/0/handle_connection.sh",nofork
