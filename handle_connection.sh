#!/bin/bash

echo "Starting server." 1>&2

# Start scilla-server in the background
rm -rf /tmp/scilla-socket/server.sock
/scilla/0/bin/scilla-server -socket /tmp/scilla-socket/server.sock 1>&2 &

# Get the PID of scilla-server
SERVER_PID=$!

echo "forwarding traffic." 1>&2

# Block until we see the scilla server has provided the socket
while [ ! -S /tmp/scilla-socket/server.sock ]; do sleep 1; done

echo "forwarding other traffic." 1>&2

# Forward traffic from stdin (provided by socat) to Unix socket.
# Note this means we ONLY use stderr when echoing within this script to avoid it going to the socket
socat - UNIX-CONNECT:/tmp/scilla-socket/server.sock

echo "connection closed - killing" 1>&2
echo "" 1>&2

# Kill the scilla-server process when done
kill $SERVER_PID
