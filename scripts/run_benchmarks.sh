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

if [ -n "$BENCH_DEBUG" ]
then
   echo "DEBUG: ocaml -version = $(ocaml -version)"
   echo "DEBUG: TRAVIS_COMMIT = $TRAVIS_COMMIT"
fi

git checkout -q "$TRAVIS_COMMIT"

# Clean up previous benchmark results
rm -rf ./bench/results/*
mkdir -p ./bench/results

make
dune exec ./bench/bin/scilla_bench_runner.exe

git checkout -q "$TRAVIS_COMMIT"

make
dune exe ./bench/bin/scilla_bench_runner.exe
