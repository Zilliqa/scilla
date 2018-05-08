#!/usr/bin/bash

# It is expected that the caller of this sript sets the following variable
# $SCILLA_RUNNER: to point to the path of scilla-runner executable.
# $SCILLA_TMP: to a directory where output temporary files can be created.

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

i=0
while [[ $i -le 5 ]]
do
    # scilla-runner needs to execute from the root directory
    curdir=${PWD}
    cd ../../

    file="crowdfunding_${i}"
    ${SCILLA_RUNNER} crowdfunding $i > "${SCILLA_TMP}/${file}.output"

    # Get back to testdir, that is where the gold file is.
    cd $curdir

    # Compare output against gold
    if ! cmp -s "gold/${file}.gold" "${SCILLA_TMP}/${file}.output"
    then
        echo "Test failed: $file"
    fi

    # Next state test
    i=$(($i+1))
      
done

i=0
while [[ $i -le 5 ]]
do
    # scilla-runner needs to execute from the root directory
    curdir=${PWD}
    cd ../../

    file="zil-game_${i}"
    ${SCILLA_RUNNER} zil-game $i > "${SCILLA_TMP}/${file}.output"

    # Get back to testdir, that is where the gold file is.
    cd $curdir

    # Compare output against gold
    if ! cmp -s "gold/${file}.gold" "${SCILLA_TMP}/${file}.output"
    then
        echo "Test failed: $file"
    fi

    # Next state test
    i=$(($i+1))
      
done

echo "Successfully run $0"

cd $orig_dir
