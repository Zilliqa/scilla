# escape=\
ARG BASE_IMAGE=ubuntu:16.04

FROM ${BASE_IMAGE}

COPY . /scilla

WORKDIR /scilla

RUN apt-get update && apt-get install -y --no-install-recommends \
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
    libboost-system-dev \
    && rm -rf /var/lib/apt/lists/*

RUN make opamdep && echo \
    ". ~/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true " >> ~/.bashrc && \
    eval `opam config env` && make
