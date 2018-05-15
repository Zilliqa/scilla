#!/bin/bash

function print_usage
if [ $# != 3 ]
then
    echo "Usage: $0 [crowdfunding|zil-game] [1-5]"
fi

contract=$1
i=$2

if [[ $contract != "crowdfunding" && $contract != "zil-game" ||
      $i -lt 1 || $i -gt 5 ]]
then
   print_usage
   exit 1
fi

cdir="examples/contracts/$contract"

./bin/scilla-runner -init ${cdir}/init.json -istate ${cdir}/state_${i}.json -imessage ${cdir}/message_${i}.json -o ${cdir}/output_${i}.json -iblockchain ${cdir}/blockchain_${i}.json -i ${cdir}/contract

echo "output.json emitted by interpreter:"
cat ${cdir}/output_${i}.json
echo ""
