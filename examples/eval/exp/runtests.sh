#!/bin/bash

# It is expected that the caller of this sript sets the following variable
# $EVAL_RUNNER: to point to the path of eval-runner executable.
# $SCILLA_TMP: to a directory where output temporary files can be created.
# $UPDATE_GOLD: should the gold be updated upon failure? 0/1

if [ ! -f $EVAL_RUNNER ]
then
   echo "${PWD}/runtests.sh scilla-runner executable not found: $SCILLA_RUNNER"
   exit 1
fi

if [ ! -d $SCILLA_TMP ]
then
   echo "${PWD}/runtests.sh temp directory not found"
   exit 1
fi

orig_dir=${PWD}
cd "$(dirname "$0")"

succ=1

i=0
for testf in *.scilla
do
    ${EVAL_RUNNER} $testf > "${SCILLA_TMP}/${testf}.output"

    # Compare output against gold
    if ! cmp -s "gold/${testf}.gold" "${SCILLA_TMP}/${testf}.output"
    then
        echo "Test failed: $testf"
        succ=0

        if [[ $UPDATE_GOLD -eq 1 ]]
        then
            echo "Updating gold for $testf".
            cp "${SCILLA_TMP}/${testf}.output" "gold/${testf}.gold"
        fi
    fi

done

if [[ $succ -eq 1 ]]
then
    echo "Successfully run $0"
fi

cd $orig_dir
