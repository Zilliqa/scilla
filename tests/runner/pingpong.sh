#!/bin/bash

cd ../../

echo "SetPongAddr"
bin/scilla-runner -init tests/runner/ping/init.json -i tests/runner/ping/contract.scilla -o /tmp/ping_output.json -imessage tests/runner/ping/message_0.json -istate tests/runner/ping/state_0.json -iblockchain tests/runner/ping/blockchain_0.json
echo "Output of Ping after setting pong address"
cat /tmp/ping_output.json
echo ""

echo "SetPingAddr"
bin/scilla-runner -init tests/runner/pong/init.json -i tests/runner/pong/contract.scilla -o /tmp/pong_output.json -imessage tests/runner/pong/message_0.json -istate tests/runner/pong/state_0.json -iblockchain tests/runner/pong/blockchain_0.json
echo "Output of Pong after setting ping address"
cat /tmp/pong_output.json
echo ""

echo "Ping ..."
bin/scilla-runner -init tests/runner/ping/init.json -i tests/runner/ping/contract.scilla -o /tmp/ping_output.json -imessage tests/runner/ping/message_1.json -istate tests/runner/ping/state_1.json -iblockchain tests/runner/ping/blockchain_1.json
echo "Output of Ping after pinging"
cat /tmp/ping_output.json
echo ""

echo "Pong ..."
bin/scilla-runner -init tests/runner/pong/init.json -i tests/runner/pong/contract.scilla -o /tmp/pong_output.json -imessage tests/runner/pong/message_1.json -istate tests/runner/pong/state_1.json -iblockchain tests/runner/pong/blockchain_1.json 
echo "Output of Pong after ponging"
cat /tmp/pong_output.json
echo ""

echo "Ping ..."
bin/scilla-runner -init tests/runner/ping/init.json -i tests/runner/ping/contract.scilla -o /tmp/ping_output.json -imessage tests/runner/ping/message_2.json -istate tests/runner/ping/state_2.json -iblockchain tests/runner/ping/blockchain_2.json
echo "Output of Ping after pinging"
cat /tmp/ping_output.json
echo ""

echo "Pong ..."
bin/scilla-runner -init tests/runner/pong/init.json -i tests/runner/pong/contract.scilla -o /tmp/pong_output.json -imessage tests/runner/pong/message_2.json -istate tests/runner/pong/state_2.json -iblockchain tests/runner/pong/blockchain_2.json 
echo "Output of Pong after ponging"
cat /tmp/pong_output.json
echo ""

echo "Ping ..."
bin/scilla-runner -init tests/runner/ping/init.json -i tests/runner/ping/contract.scilla -o /tmp/ping_output.json -imessage tests/runner/ping/message_3.json -istate tests/runner/ping/state_3.json -iblockchain tests/runner/ping/blockchain_3.json 
echo "Output of Ping after pinging"
cat /tmp/ping_output.json
echo ""
