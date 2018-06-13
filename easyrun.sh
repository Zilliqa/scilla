#!/bin/bash

function print_usage
if [ $# != 3 ]
then
    echo "Usage: $0 contract-name test_number"
fi

contract=$1
i=$2


cdir="tests/contracts/$contract"

if [[ ! -f ${cdir}/state_${i}.json ||
      ! -d ${cdir} ]]
then
    echo "Test $contract $i does not exist"
    print_usage
    exit 1
fi

./bin/scilla-runner -init ${cdir}/init.json -istate ${cdir}/state_${i}.json -imessage ${cdir}/message_${i}.json -o ${cdir}/output_${i}.json -iblockchain ${cdir}/blockchain_${i}.json -i ${cdir}/contract.scilla

echo "output.json emitted by interpreter:"
cat ${cdir}/output_${i}.json
echo ""
