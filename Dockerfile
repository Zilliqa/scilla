# escape=\
ARG BASE_IMAGE=ubuntu:18.04

FROM --platform=linux/amd64 ${BASE_IMAGE}

ARG MAJOR_VERSION=0

COPY . /scilla/${MAJOR_VERSION}

WORKDIR /scilla/${MAJOR_VERSION}

# Install socat
RUN apt-get update && apt-get install -y socat

RUN apt-get update \
    && apt-get install -y software-properties-common \
    && add-apt-repository ppa:avsm/ppa -y \
    && apt-get update && apt-get install -y --no-install-recommends \
    git \
    curl \
    wget \
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
    libboost-test-dev \
    libboost-dev \
    libpcre3-dev \
    vim \
    netcat-openbsd \
    && rm -rf /var/lib/apt/lists/*

ENV OCAML_VERSION 4.11.2

# CMake gets installed here
ENV PATH="/root/.local/bin:${PATH}"

RUN bash scripts/install_cmake_ubuntu.sh \
    && make opamdep-ci \
    && echo '. ~/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true ' >> ~/.bashrc \
    && eval $(opam env) && \
    make

# Expose the port used by socat and netcat
EXPOSE 12345
EXPOSE 12346

