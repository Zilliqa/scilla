# escape=\
ARG BASE_IMAGE=zilliqa/zilliqa:v8.3.0-deps

FROM ${BASE_IMAGE}

# Manually input tag or commit, can be overwritten by docker build-args
ARG MAJOR_VERSION=0
ARG SOURCE_DIR="/scilla/${MAJOR_VERSION}"
ARG COMMIT_OR_TAG=master
ARG REPO=https://github.com/Zilliqa/scilla.git

RUN git clone ${REPO} ${SOURCE_DIR}
RUN git -C ${SOURCE_DIR} checkout ${COMMIT_OR_TAG} \
    && git -C ${SOURCE_DIR} submodule update --init --recursive

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
    patchelf \
    && rm -rf /var/lib/apt/lists/*

ENV OCAML_VERSION 4.11.2

WORKDIR ${SOURCE_DIR}

# Make sure vcpkg installs brings in the dependencies
ENV VCPKG_ROOT=/vcpkg
RUN ${VCPKG_ROOT}/vcpkg install --triplet=x64-linux-dynamic

ENV PKG_CONFIG_PATH="${SOURCE_DIR}/vcpkg_installed/x64-linux-dynamic/lib/pkgconfig"

RUN apt update -y && make opamdep-ci \
    && echo '. ~/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true ' >> ~/.bashrc \
    && eval $(opam env) \
    && make

RUN mkdir -p _build/default/vcpkg_installed/x64-linux-dynamic/lib/ \
  && cp vcpkg_installed/x64-linux-dynamic/lib/libffi* _build/default/vcpkg_installed/x64-linux-dynamic/lib/ \
  && find _build/default -type f -name '*.exe' -print0 | xargs -0 patchelf --set-rpath "$(pwd)/_build/default/vcpkg_installed/x64-linux-dynamic/lib" \
  && rm -rf vcpkg_installed
