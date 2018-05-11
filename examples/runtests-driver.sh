# This script looks for "runtests.sh" in every subdirectory
# and executes it after setting the following variables:
#  $SCILLA_RUNNER: pointer to the scilla-runner executable
#  $EVAL_RUNNER: pointer to the eval-runner executable
#  $SCILLA_TMP: pointer to a tmp directory for outputs
#  $UPDATE_GOLD: should the gold be updated upon failure? 0/1
#                This should only be set when all tests pass,
#                that require actual update.

# Switch to directory in which this file is present.
orig_dir=${PWD}
cd "$(dirname "$0")"

export SCILLA_RUNNER=${PWD}/../bin/scilla-runner
export EVAL_RUNNER=${PWD}/../bin/eval-runner
export SCILLA_TMP=`mktemp -d`

subtests=`find ./ -name "runtests.sh"`

for subtest in $subtests
do
    echo "Running $subtest with tmp directory $SCILLA_TMP"
    bash $subtest
done

rm ${SCILLA_TMP}/*
rmdir ${SCILLA_TMP}

# Switch back to wherever we were before.
cd $orig_dir
