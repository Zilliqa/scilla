# escape=\
ARG BASE_IMAGE=ubuntu:16.04

FROM ${BASE_IMAGE}

COPY . /scilla

WORKDIR /scilla

RUN apt-get update \
    && apt-get install -y software-properties-common \
    && add-apt-repository ppa:tah83/secp256k1 -y \
    && apt-get update && apt-get install -y --no-install-recommends \
    curl \
    build-essential \
    m4 \
    ocaml \
    opam \
    pkg-config \
    zlib1g-dev \
    libgmp-dev \
    libffi-dev \
    libssl-dev \
    libsecp256k1-dev \
    libboost-system-dev \
    && rm -rf /var/lib/apt/lists/*

RUN ./scripts/build_openssl.sh && \
    make opamdep && echo \
    ". ~/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true " >> ~/.bashrc && \
    eval `opam config env` && \
    # build_openssl.sh builds and install OpenSSL 1.1.1 in ${HOME}/openssl/install
    CPLUS_INCLUDE_PATH=${HOME}/openssl/install/include LIBRARY_PATH=${HOME}/openssl/install/lib LD_LIBRARY_PATH=${HOME}/openssl/install/lib make
