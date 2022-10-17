#!/usr/bin/env bash

##  This file is part of scilla.
##
##  Copyright (c) 2022 - present Zilliqa Research Pvt. Ltd.
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
# $./scripts/vcpkg.sh
# The script installs vcpkg in the vcpkg_installed directory of Scilla root.

if [ -z "${VCPKG_ROOT}" ]; then
  echo -e "\033[1;33mVCPKG_ROOT is not set\033[0m"
  exit 1
fi

mkdir -pv ${VCPKG_ROOT}

git clone -b 2022.09.27 https://github.com/Microsoft/vcpkg.git

cd ${VCPKG_ROOT} && ../vcpkg/bootstrap-vcpkg.sh

# Check if CWD has `scilla.opam`, assuring us that it's the root.
if [[ ! -f scilla.opam ]]
then
   echo "Please run this script from Scilla root"
   exit 1
fi

# If already installed, exit early.
if [[ -d vcpkg_installed ]]
then
    echo "Found vcpkg_installed, not installing again"
    exit 0
fi

echo "Installing vcpkg"
if ! "$VCPKG_ROOT"/vcpkg install --triplet x64-linux-dynamic
then 
    echo "vcpkg installation failed"
    exit 1
fi

echo "vcpkg: installation complete"
