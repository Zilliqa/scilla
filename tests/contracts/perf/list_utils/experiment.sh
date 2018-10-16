runs=5

function runscilla
{
    n=$1
    trans=$2
    i=1
    while [[ $i -le $runs ]]
    do
        ../../../../bin/scilla-runner -init init.json -istate state_${n}.json -imessage message_${trans}.json -o output.json -iblockchain blockchain.json -i contract.scilla -libdir ../../../..//src/stdlib -gaslimit 9999999999
        
        i=$(($i + 1))
    done
}


function runexp
{
    n=$1
    trans=$2
    t=`(time runscilla ${n} ${trans}) 2>&1 | tee -a timer-log.txt | grep "real" | cut -d $'\t' -f 2`
    echo "Time for $runs runs of list_${trans}: $t"
}

rm timer-log.txt

runexp 5000 reverse_length
runexp 5000 map
runexp 5000 filter
runexp 5000 unzip_zip
runexp 5000 to_map_eq
runexp 5000 mem
runexp 5000 sort
runexp 5000 lforall
runexp 5000 exists
runexp 5000 find
runexp 5000 nth

runexp 10000 reverse_length
runexp 10000 map
runexp 10000 filter
runexp 10000 unzip_zip
runexp 10000 to_map_eq
runexp 10000 mem
runexp 10000 sort
runexp 10000 lforall
runexp 10000 exists
runexp 10000 find
runexp 10000 nth
