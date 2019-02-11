# escape=\
ARG BASE_IMAGE=ubuntu:16.04

FROM ${BASE_IMAGE}

ARG MAJOR_VERSION=0

COPY . /scilla/${MAJOR_VERSION}

WORKDIR /scilla/${MAJOR_VERSION}

RUN apt-get update && \
  apt-get -y install ca-certificates curl software-properties-common && \
  add-apt-repository ppa:deadsnakes/ppa

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
    git

RUN apt-get -y install python3.6 python3.6-dev \
    && rm -rf /var/lib/apt/lists/*

RUN cd /scilla/${MAJOR_VERSION} && make opamdep \
    && echo '. ~/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true ' >> ~/.bashrc \
    && eval `opam config env` && \
    make

WORKDIR /code

RUN apt-get update && apt-get -y install wget

RUN wget http://www.valgrind.org/downloads/valgrind-3.14.0.tar.bz2 && \
    tar -xvjf valgrind-3.14.0.tar.bz2

WORKDIR /code/valgrind-3.14.0

RUN ./configure && make && make install && make clean

CMD python3.6 /code/while.py
