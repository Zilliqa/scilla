#!/usr/bin/env bash

##  This file is part of scilla.
##
##  Copyright (c) 2018 - present Zilliqa Research Pvt. Ltd.
##  
##  scilla is free software: you can redistribute it and/or modify it under the
##  terms of the GNU General Public License as published by the Free Software
##  Foundation, either version 3 of the License, or (at your option) any later
##  version.
## 
##  scilla is distributed in the hope that it will be useful, but WITHOUT ANY
##  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
##  A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
## 
##  You should have received a copy of the GNU General Public License along with
##  scilla.  If not, see <http://www.gnu.org/licenses/>.

# This script is expected to be executed from Scilla root
# $./scripts/schnorr.sh
# The script builds and installs schnorr in the
# _build directory of Scilla root.

if [ -z ${VCPKG_ROOT} ]; then
  echo -e "\033[1;33mVCPKG_ROOT is not set\033[0m"
  exit 1
fi

schnorrdir="deps/schnorr"

# Check if CWD has `scilla.opam`, assuring us that it's the root.
if [[ ! -f scilla.opam ]]
then
   echo "Please run this script from Scilla root"
   exit 1
fi

# If there's already a built version available, exit early.
if [[ -f ${schnorrdir}/install/lib/libSchnorr.a ]]
then
    echo "Found libSchnorr.a, not building again"
    exit 0
fi

cp vcpkg.json ${schnorrdir}/src/
cp vcpkg-configuration.json ${schnorrdir}/src/
cp -R vcpkg-registry ${schnorrdir}/src/

cd $schnorrdir || exit
mkdir -p build install
cd src || exit
echo "Installing schnorr into ${schnorrdir}/install"
cd ../build || exit
if ! cmake ../src -DCMAKE_INSTALL_PREFIX=../install -DCMAKE_POSITION_INDEPENDENT_CODE=1 -DSCHNORR_BUILD_ARCHIVE=1 -DCMAKE_TOOLCHAIN_FILE=$VCPKG_ROOT/vcpkg/scripts/buildsystems/vcpkg.cmake -DVCPKG_TARGET_TRIPLET=x64-linux-dynamic
then
    echo "schnorr: CMake configuration failed"
    exit 1
fi

if ! make -j $(nproc) install
then
    echo "schnorr: build failed"
    exit 1
fi

echo "schnorr: installation complete"
