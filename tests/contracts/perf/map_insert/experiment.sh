runs=1

function runscilla
{
    n=$1
    i=1
    while [[ $i -le $runs ]]
    do
        ../../../../bin/scilla-runner -init init.json -istate state_${n}.json -imessage message.json -o output.json -iblockchain blockchain.json -i contract.scilla -libdir ../../../..//src/stdlib -gaslimit 1000000
        
        i=$(($i + 1))
    done
}

function runexp
{
    n=$1
    t=`(time runscilla ${1}) 2>&1 | tee -a timer.log | grep "real" | cut -d $'\t' -f 2`
    echo "Time for $runs runs of list size $1: $t"
}

runexp 5000
runexp 10000
