#!/bin/bash
if [ $# -eq "0" ]
    then
        echo "No arguments supplied"
        exit 1
else
    cp blockchain_$1.json blockchain_$2.json
    cp state_$1.json state_$2.json
    cp message_$1.json message_$2.json
fi
