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

# Determine vcpkg os and arch triplet
OS="unknown"
ARCH="$(uname -m)"

unameOut="$(uname -s)"
case "${unameOut}" in
    Linux*)     OS=linux;;
    Darwin*)    OS=osx;;
    *)          echo "Unknown machine ${unameOut}"
esac

unameOut="$(uname -m)"
case "${unameOut}" in
    arm*)       ARCH=arm64;;
    aarch*)     ARCH=arm64;;
    x86_64*)    ARCH=x64;;
    *)          echo "Unknown machine ${unameOut}"
esac

VCPKG_TRIPLET=${ARCH}-${OS}-dynamic
echo ${VCPKG_TRIPLET}

