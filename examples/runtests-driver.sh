# This script looks for "runtests.sh" in every subdirectory
# and executes it after setting the following variables:
#  $SCILLA_RUNNER: pointer to the scilla-runner executable
#  $EVAL_RUNNER: pointer to the eval-runner executable
#  $SCILLA_TMP: pointer to a tmp directory for outputs

export SCILLA_RUNNER=${PWD}/../bin/scilla-runner
export EVAL_RUNNER=${PWD}/../bin/eval-runner
export SCILLA_TMP=`mktemp -d`

subtests=`find ./ -name "runtests.sh"`

for subtest in $subtests
do
    echo "Running $subtest with tmp directory $SCILLA_TMP"
    bash $subtest
done
