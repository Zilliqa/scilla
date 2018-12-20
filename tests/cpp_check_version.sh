#!/bin/bash

zil_baseurl="https://raw.githubusercontent.com/Zilliqa/Zilliqa/master/src"

files=(
    "libUtils/DataConversion.cpp"
    "libUtils/DataConversion.h"
    "libCrypto/Schnorr.cpp"
    "libCrypto/Schnorr.h"
    "libCrypto/Sha2.h"
    "common/Serializable.h"
    )

for file in ${files[@]}
do
    bname=`basename $file`
    wget -qO /tmp/${bname} ${zil_baseurl}/${file}
    if ! cmp -s /tmp/${bname} src/cpp/Zilliqa/${bname}
    then
        echo "$bname is out of date with Zilliqa repo. Please update"
    fi
done
