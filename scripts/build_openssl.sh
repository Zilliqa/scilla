#!/usr/bin/env sh

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

if [ -d ~/openssl/install ]
then
    echo "OpenSSL already installed, exiting"
    exit 0
fi

git clone https://github.com/openssl/openssl.git ~/openssl/src
cd ~/openssl/src || exit
git checkout tags/OpenSSL_1_1_1
./config --prefix="${HOME}"/openssl/install --openssldir="${HOME}"/openssl/ssl
make -j4
make install
