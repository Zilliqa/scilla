#!/usr/bin/env bash
# This script is expected to be executed from Scilla root
# $./scripts/libff.sh
# The script fetches, builds and installs libff in the
# _build directory of Scilla root.

libffurl="https://github.com/scipr-lab/libff.git"
libffdir="_deps/libff"

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

mkdir -p $libffdir
cd $libffdir
mkdir -p build install
echo "Cloning libff into ${libffdir}/src"
git clone $libffurl src
cd src
echo "Fetching submodules for libff"
git submodule init && git submodule update
git checkout f2067162520f91438b44e71a2cab2362f1c3cab4
echo "Installing libff into ${libffdir}/install"
cd ../build
cmake ../src -DCMAKE_INSTALL_PREFIX=../install -DCMAKE_POSITION_INDEPENDENT_CODE=1 -DWITH_PROCPS=OFF
if [[ $? -ne 0 ]]
then
    echo "libff: CMake configuration failed"
    exit 1
fi

make -j4 install
if [[ $? -ne 0 ]]
then
    echo "libff: build failed"
    exit 1
fi

echo "libff: installation complete"
