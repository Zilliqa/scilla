# escape=\
ARG BASE_IMAGE=zilliqa/zilliqa:v8.3.0-deps

FROM ${BASE_IMAGE}

ARG MAJOR_VERSION=0
ARG SCILLA_PATH="/scilla/${MAJOR_VERSION}"

COPY . ${SCILLA_PATH}

WORKDIR ${SCILLA_PATH}

RUN apt-get update \
    && apt-get install -y software-properties-common \
    && add-apt-repository ppa:avsm/ppa -y \
    && apt-get update && apt-get install -y --no-install-recommends \
    git \
    curl \
    wget \
    build-essential \
    m4 \
    ocaml \
    opam \
    pkg-config \
    zlib1g-dev \
    libgmp-dev \
    libssl-dev \
    libsecp256k1-dev \
    libpcre3-dev \
    && rm -rf /var/lib/apt/lists/*

ENV OCAML_VERSION 4.11.2

# CMake gets installed here
ENV PATH="/root/.local/bin:${PATH}"

# Make sure vcpkg installs brings in the dependencies
RUN /vcpkg/vcpkg install --triplet=x64-linux-dynamic

ENV PKG_CONFIG_PATH="${SCILLA_PATH}/vcpkg_installed/x64-linux-dynamic/lib/pkgconfig"

RUN apt update -y && make opamdep-ci \
    && echo '. ~/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true ' >> ~/.bashrc \
    && eval $(opam env) \
    && make

RUN mkdir -p _build/default/vcpkg_installed/x64-linux/dynamic/lib/ \
  && cp vcpkg_installed/x64-linux-dynamic/lib/libffi* _build/default/vcpkg_installed/x64-linux/dynamic/lib/ \
  && find . -type d -name vcpkg_installed | xargs rm -rf
