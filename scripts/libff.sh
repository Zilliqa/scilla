#!/usr/bin/env bash

# This script is expected to be executed from Scilla root
# $./scripts/libff.sh
# The script builds and installs libff in the
# _build directory of Scilla root.

libffdir="deps/libff"

# Check if CWD has `scilla.opam`, assuring us that it's the root.
if [[ ! -f scilla.opam ]]
then
   echo "Please run this script from Scilla root"
   exit 1
fi

# If there's already a built version available, exit early.
if [[ -f ${libffdir}/install/lib/libff.a ]]
then
    echo "Found libff.a, not building again"
    exit 0
fi

cd $libffdir || exit
mkdir -p build install
cd src || exit
echo "Installing libff into ${libffdir}/install"
cd ../build || exit
if ! cmake ../src -DCMAKE_INSTALL_PREFIX=../install -DCMAKE_POSITION_INDEPENDENT_CODE=1 -DWITH_PROCPS=OFF
then
    echo "libff: CMake configuration failed"
    exit 1
fi

if ! make -j4 install
then
    echo "libff: build failed"
    exit 1
fi

echo "libff: installation complete"
