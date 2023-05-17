# escape=\

# Common dependencies of the builder and runner stages.
FROM ubuntu:22.04 AS base
# Format guideline: one package per line and keep them alphabetically sorted
RUN apt-get update -y \
    && apt-get install -y software-properties-common \
    && apt-get update && apt-get install -y --no-install-recommends \
    autoconf \
    bison \
    build-essential \
    ca-certificates \
    ccache \
    cron \
    curl \
    dnsutils \
    gawk \
    git \
    lcov \
    libcurl4-openssl-dev \
    libgmp-dev \
    libpcre3-dev \
    libssl-dev \
    libtool \
    libxml2-utils \
    ninja-build \
    nload \
    ocaml \
    ocl-icd-opencl-dev \
    opam \
    openssh-client \
    patchelf \
    pkg-config \
    rsync \
    rsyslog \
    tar \
    trickle \
    unzip \
    vim \
    wget \
    zip \
    zlib1g-dev \
    && apt-get remove -y cmake python2 python3 && apt-get autoremove -y

FROM base AS builder

# CMake 3.24
ARG CMAKE_VERSION=3.24.2
RUN wget https://github.com/Kitware/CMake/releases/download/v${CMAKE_VERSION}/cmake-${CMAKE_VERSION}-Linux-x86_64.sh \
    && mkdir -p "${HOME}"/.local \
    && bash ./cmake-${CMAKE_VERSION}-Linux-x86_64.sh --skip-license --prefix="${HOME}"/.local/ \
    && "${HOME}"/.local/bin/cmake --version \
    && rm cmake-${CMAKE_VERSION}-Linux-x86_64.sh
ENV PATH="/root/.local/bin:${PATH}"

# Setup ccache
RUN ln -s "$(which ccache)" /usr/local/bin/gcc \
  && ln -s "$(which ccache)" /usr/local/bin/g++ \
  && ln -s "$(which ccache)" /usr/local/bin/cc \
  && ln -s "$(which ccache)" /usr/local/bin/c++

# This tag must be equivalent to the hash specified by "builtin-baseline" in vcpkg.json
ARG VCPKG_COMMIT_OR_TAG=2022.09.27
ARG VCPKG_ROOT=/vcpkg

RUN ccache -p && ccache -z

# If COMMIT_OR_TAG is a branch name or a tag, clone a shallow copy which is
# faster; if this fails, just clone the full repo and checkout the commit.
RUN git clone https://github.com/microsoft/vcpkg ${VCPKG_ROOT} \
  && git -C ${VCPKG_ROOT} checkout ${VCPKG_COMMIT_OR_TAG} \
  && ${VCPKG_ROOT}/bootstrap-vcpkg.sh

RUN apt-get remove -y python3-dev && apt-get autoremove -y
# Manually input tag or commit, can be overwritten by docker build-args
ARG MAJOR_VERSION=0
ARG SOURCE_DIR="/scilla/${MAJOR_VERSION}"

WORKDIR ${SOURCE_DIR}

ENV OCAML_VERSION 4.12.0

COPY vcpkg-registry/ vcpkg-registry
COPY vcpkg-configuration.json .
COPY vcpkg.json .
COPY vcpkg-ocaml/ vcpkg-ocaml
COPY src/ src
COPY scripts/ scripts
COPY dune* ./
COPY Makefile .
COPY scilla.opam .
COPY shell.nix .
COPY .ocamlformat .
COPY tests/ tests

# Make sure vcpkg installs brings in the dependencies
RUN --mount=type=cache,target=/root/.cache/vcpkg/ ${VCPKG_ROOT}/vcpkg install --triplet=x64-linux-dynamic

RUN update-alternatives --install /usr/bin/python python "${SOURCE_DIR}/vcpkg_installed/x64-linux-dynamic/tools/python3/python3.10" 10 \
  && update-alternatives --install /usr/bin/python3 python3 "${SOURCE_DIR}/vcpkg_installed/x64-linux-dynamic/tools/python3/python3.10" 10 # set python3 as default instead python3

ENV PKG_CONFIG_PATH="${SOURCE_DIR}/vcpkg_installed/x64-linux-dynamic/lib/pkgconfig"
ENV SCILLA_REPO_ROOT="${SOURCE_DIR}"

RUN make opamdep-ci \
    && echo '. ~/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true ' >> ~/.bashrc \
    && eval $(opam env)
    && make

ARG BUILD_DIR="${SOURCE_DIR}/_build/default"
ARG VCPKG_INSTALL_LIB_DIR="${BUILD_DIR}/vcpkg_installed/x64-linux-dynamic/lib"

RUN mkdir -p ${VCPKG_INSTALL_LIB_DIR} \
  && ldd ${BUILD_DIR}/src/runners/*.exe | grep vcpkg_installed | gawk '{print $3}' | xargs -I{} cp {} ${VCPKG_INSTALL_LIB_DIR} \
  && rm -rf vcpkg_installed \
  && ln -s ${BUILD_DIR}/vcpkg_installed vcpkg_installed

FROM ubuntu:22.04

RUN apt-get update -y \
    && apt-get install -y build-essential \
    libgmp-dev

ARG SOURCE_DIR="/scilla/${MAJOR_VERSION}"

COPY --from=builder ${SOURCE_DIR}       ${SOURCE_DIR}

