#!/usr/bin/bash

# It is expected that the caller of this sript sets the following variable
# $SCILLA_RUNNER: to point to the path of scilla-runner executable.
# $SCILLA_TMP: to a directory where output temporary files can be created.
# $UPDATE_GOLD: should the gold be updated upon failure? 0/1

if [ ! -f $SCILLA_RUNNER ]
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

i=1
while [[ $i -le 5 ]]
do
    # scilla-runner needs to execute from the root directory
    curdir=${PWD}
    cd ../../

    cdir="examples/contracts/crowdfunding"
    ${SCILLA_RUNNER} -i "${cdir}/contract" -istate "${cdir}/state_${i}.json" -iblockchain "${cdir}/blockchain_${i}.json" -imessage "${cdir}/message_${i}.json" -init "${cdir}/init.json" -o "${SCILLA_TMP}/output_${i}.json" > "${SCILLA_TMP}/crowdfunding_${i}.output"

    # Get back to testdir, that is where the gold file is.
    cd $curdir

    # Compare output against gold
    if ! cmp -s "gold/crowdfunding_${i}.gold" "${SCILLA_TMP}/crowdfunding_${i}.output"
    then
        echo "Test failed: crowdfunding_${i}"

        if [[ $UPDATE_GOLD -eq 1 ]]
        then
            echo "Updating gold for crowdfunding_${i}".
            cp "${SCILLA_TMP}/crowdfunding_${i}.output" "gold/crowdfunding_${i}.gold"
        fi
    fi

    if ! cmp -s "crowdfunding/output_${i}.json" "${SCILLA_TMP}/output_${i}.json"
    then
        echo "Test json output comparison failed: crowdfunding_${i}"

        if [[ $UPDATE_GOLD -eq 1 ]]
        then
            echo "Updating gold output json for crowdfunding_${i}".
            cp "${SCILLA_TMP}/output_${i}.json" "crowdfunding/output_${i}.json"
        fi
    fi


    # Next state test
    i=$(($i+1))
done

i=1
while [[ $i -le 5 ]]
do
    # scilla-runner needs to execute from the root directory
    curdir=${PWD}
    cd ../../

    cdir="examples/contracts/zil-game"
    ${SCILLA_RUNNER} -i "${cdir}/contract" -istate "${cdir}/state_${i}.json" -iblockchain "${cdir}/blockchain_${i}.json" -imessage "${cdir}/message_${i}.json" -init "${cdir}/init.json" -o "${SCILLA_TMP}/output_${i}.json" > "${SCILLA_TMP}/zil-game_${i}.output"

    # Get back to testdir, that is where the gold file is.
    cd $curdir

    # Compare output against gold
    if ! cmp -s "gold/zil-game_${i}.gold" "${SCILLA_TMP}/zil-game_${i}.output"
    then
        echo "Test failed: zil-game_${i}"

        if [[ $UPDATE_GOLD -eq 1 ]]
        then
            echo "Updating gold for zil-game_${i}".
            cp "${SCILLA_TMP}/zil-game_${i}.output" "gold/zil-game_${i}.gold"
        fi
    fi

    if ! cmp -s "zil-game/output_${i}.json" "${SCILLA_TMP}/output_${i}.json"
    then
        echo "Test json output comparison failed: zil-game_${i}"

        if [[ $UPDATE_GOLD -eq 1 ]]
        then
            echo "Updating gold output json for zil-game_${i}".
            cp "${SCILLA_TMP}/output_${i}.json" "zil-game/output_${i}.json"
        fi
    fi


    # Next state test
    i=$(($i+1))
done

echo "Successfully run $0"

cd $orig_dir
