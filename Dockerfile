# escape=\
ARG BASE_IMAGE=ubuntu:18.04

FROM ${BASE_IMAGE}

ARG MAJOR_VERSION=0

COPY . /scilla/${MAJOR_VERSION}

WORKDIR /scilla/${MAJOR_VERSION}

RUN apt-get update \
    && apt-get install -y software-properties-common \
    && add-apt-repository ppa:avsm/ppa -y \
    && apt-get update && apt-get install -y --no-install-recommends \
    curl \
    cmake \
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
    libpcre3-dev \
    && rm -rf /var/lib/apt/lists/*

ENV OCAML_VERSION 4.07.1

RUN make opamdep-ci \
    && echo '. ~/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true ' >> ~/.bashrc \
    && eval $(opam env) && \
    make
