# This script assumes that there is an IPC state server running
# at /tmp/zilliqa.sock and runs the interpreter to deploy
# and run all transitions of tests/contracts/map_corners_test.scilla.

num_transitions=18
ipcaddress=/tmp/zilliqa.sock
scilla_runner=../bin/scilla-runner
test_source=../tests/contracts/map_corners_test.scilla
libdir=../src/stdlib
init_json_file=/tmp/ipc_map_corner_tests.init.json
message_json_file=/tmp/ipc_map_corner_tests.message.json
blockchain_json_file=/tmp/ipc_map_corner_tests.blockchain.json
output_json_file=/tmp/ipc_map_corner_tests.output.json
gaslimit=100000

init_json='
[
    {
        "vname" : "_scilla_version",
        "type" : "Uint32",
        "value" : "0"
    },
    {
        "vname" : "_this_address",
        "type" : "ByStr20",
        "value" : "0xabfeccdc9012345678901234567890f777567890"
    },
    {
        "vname" : "_creation_block",
        "type" : "BNum",
        "value" : "1"
    }
]
'

blockchain_json='[ { "vname": "BLOCKNUMBER", "type": "BNum", "value": "100" } ]'

get_message_json () {
    local i=$1
    message_json='{
       "_tag": "t'$i'",
       "_amount": "0",
       "_sender" : "0x12345678901234567890123456789012345678ab",
       "params": []
   }'
}

echo $init_json > $init_json_file
echo $blockchain_json > $blockchain_json_file

# First deploy the contract.
$scilla_runner -init $init_json_file -i $test_source -iblockchain $blockchain_json_file -o $output_json_file -ipcaddress $ipcaddress -libdir $libdir -gaslimit $gaslimit

if [[ $? -ne 0 ]]
then
    echo "Deployment failed"
    exit 1
fi

i=1
while [[ $i -le $num_transitions ]]
do
    get_message_json $i
    echo $message_json > $message_json_file
    $scilla_runner -init $init_json_file -i $test_source -iblockchain $blockchain_json_file -o $output_json_file -ipcaddress $ipcaddress -libdir $libdir -gaslimit $gaslimit -imessage $message_json_file -balance 0
    test_result=$?
    if  [[ $test_result -ne 0 ]]
    then
        echo "Transition test $i failed"
        exit 1
    fi
    i=$(($i+1))
done

echo "All transition tests passed"

