#!/bin/sh

if [ -d ~/openssl/install ]
then
    echo "OpenSSL already installed, exiting"
    exit 0
fi

git clone https://github.com/openssl/openssl.git ~/openssl/src
cd ~/openssl/src
git checkout tags/OpenSSL_1_0_2n
./config --prefix=${HOME}/openssl/install --openssldir=${HOME}/openssl/ssl
make -j4
make install
