#!/usr/bin/env bash

# The -e flag makes sure the script exits as soon
# as one command returns a non-zero exit code
set -e

# Base branch or commit (defaults to master)
BASE=${1:-master}
# Branch or commit to compare with.
# Try the second argument and default to the $TRAVIS_COMMIT on CI.
# If none is set it defaults to comparing with the current state of the repository.
COMPARE=${2:-$TRAVIS_COMMIT}

if [ -n "$BENCH_DEBUG" ]; then
  echo "DEBUG: ocaml -version = $(ocaml -version)"
  echo "DEBUG: TRAVIS_COMMIT = $TRAVIS_COMMIT"
  echo "DEBUG: BASE = $BASE"
  echo "DEBUG: COMPARE = $COMPARE"
fi

if [ -n "$COMPARE" ]; then
  # Checkout the branch/commit to compare with
  git checkout -q "$COMPARE"
fi

# Clean up previous benchmark results
rm -rf ./bench/results/*
mkdir -p ./bench/results

make
dune exec ./bench/bin/scilla_bench_runner.exe

git checkout -q "$BASE"

make
dune exec ./bench/bin/scilla_bench_runner.exe
