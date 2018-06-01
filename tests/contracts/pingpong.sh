#!/bin/bash

cd ../../

echo "Ping ..."
bin/scilla-runner -init tests/contracts/ping/init.json -i tests/contracts/ping/contract.scilla -o /tmp/ping_output.json -imessage tests/contracts/ping/message_1.json -istate tests/contracts/ping/state_1.json -iblockchain tests/contracts/ping/blockchain_1.json
echo "Output state of Ping after pinging"
cat /tmp/ping_output.json
echo ""

echo "Pong ..."
bin/scilla-runner -init tests/contracts/pong/init.json -i tests/contracts/pong/contract.scilla -o /tmp/pong_output.json -imessage tests/contracts/pong/message_1.json -istate tests/contracts/pong/state_1.json -iblockchain tests/contracts/pong/blockchain_1.json 
echo "Output state of Pong after ponging"
cat /tmp/pong_output.json
echo ""

echo "Ping ..."
bin/scilla-runner -init tests/contracts/ping/init.json -i tests/contracts/ping/contract.scilla -o /tmp/ping_output.json -imessage tests/contracts/ping/message_2.json -istate tests/contracts/ping/state_2.json -iblockchain tests/contracts/ping/blockchain_2.json
echo "Output state of Ping after pinging"
cat /tmp/ping_output.json
echo ""

echo "Pong ..."
bin/scilla-runner -init tests/contracts/pong/init.json -i tests/contracts/pong/contract.scilla -o /tmp/pong_output.json -imessage tests/contracts/pong/message_2.json -istate tests/contracts/pong/state_2.json -iblockchain tests/contracts/pong/blockchain_2.json 
echo "Output state of Pong after ponging"
cat /tmp/pong_output.json
echo ""

echo "Ping ..."
bin/scilla-runner -init tests/contracts/ping/init.json -i tests/contracts/ping/contract.scilla -o /tmp/ping_output.json -imessage tests/contracts/ping/message_3.json -istate tests/contracts/ping/state_3.json -iblockchain tests/contracts/ping/blockchain_3.json 
echo "Output state of Ping after pinging"
cat /tmp/ping_output.json
echo ""
