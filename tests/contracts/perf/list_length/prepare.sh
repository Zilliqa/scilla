
update_state()
{
    states=`sed '1,10d' output.json | head -n -3`
    echo -e "[\n$states \n]\n" > state.json
}

i=1
while [[ $i -lt 100 ]]
do
    ../../../../bin/scilla-runner -init init.json -istate state.json -imessage message_1.json -o output.json -iblockchain blockchain_1.json -i contract.scilla -libdir ../../../..//src/stdlib -gaslimit 100000
    
    update_state

    i=$(($i + 1))
done
