#!/usr/bin/env bash

# The -e flag makes sure the script exits as soon as one command returns a non-zero exit code
set -e

# Check that the required arguments are provided
check_variable () {
  if [ -z "${1+x}" ]; then
      echo "Variable $1 should be set"
      exit 1
  fi
}

check_variable "WORKSPACE"

working_dir="${WORKSPACE%@*}/travis"

if [ -n "$BENCH_DEBUG" ]
then
   echo "DEBUG: ocaml -version = $(ocaml -version)"
   echo "DEBUG: working_dir = $working_dir"
   echo "DEBUG: TRAVIS_COMMIT = $TRAVIS_COMMIT"
fi

rm -rf "$working_dir"
mkdir -p "$working_dir"

scilla_dir="$working_dir/scilla"
git clone -q https://github.com/Zilliqa/scilla.git "$scilla_dir"
cd "$scilla_dir"

git checkout -q "$TRAVIS_COMMIT"

dune exe ./bench/bin/scilla_bench_runner.exe -- -ci

git checkout -q master

dune exe ./bench/bin/scilla_bench_runner.exe -- -ci
