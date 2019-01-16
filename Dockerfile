# escape=\
ARG BASE_IMAGE=ubuntu:16.04
ARG MAJOR_VERSION=0

FROM ${BASE_IMAGE}

COPY . /scilla/${MAJOR_VERSION}

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

RUN cd /scilla && make opamdep \
    && echo '. ~/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true ' >> ~/.bashrc \
    && eval `opam config env` && \
    make
