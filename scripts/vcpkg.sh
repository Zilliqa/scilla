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

ARG COMMIT_OR_TAG=
ARG REPO=https://github.com/Zilliqa/Zilliqa.git
ARG SOURCE_DIR=/zilliqa
ARG BUILD_TRIPLET=x64-linux-dynamic
ARG EXTRA_CMAKE_ARGS=


RUN git clone ${REPO} ${SOURCE_DIR}
RUN git -C ${SOURCE_DIR} checkout master
WORKDIR ${SOURCE_DIR}
RUN ${VCPKG_ROOT}/vcpkg install --triplet=${BUILD_TRIPLET}
WORKDIR /
RUN rm -rf ${SOURCE_DIR}


    $ 
    $ cd /path/to/zilliqa
    $ export VCPKG_ROOT=/path/to/vcpkg