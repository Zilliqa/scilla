# escape=\

ARG ZILLIQA_VERSION=v8.4.0
FROM zilliqa/zilliqa-deps:${ZILLIQA_VERSION} AS builder

# Manually input tag or commit, can be overwritten by docker build-args
ARG MAJOR_VERSION=0
ARG SOURCE_DIR="/scilla/${MAJOR_VERSION}"
ARG COMMIT_OR_TAG=master
ARG REPO=https://github.com/Zilliqa/scilla.git

RUN git clone ${REPO} ${SOURCE_DIR}
RUN git -C ${SOURCE_DIR} checkout ${COMMIT_OR_TAG}

RUN apt-get install -y software-properties-common \
    && add-apt-repository ppa:avsm/ppa -y \
    && apt-get install -y --no-install-recommends \
    libgmp-dev \
    libpcre3-dev \
    libsecp256k1-dev \
    ocaml \
    opam \
    patchelf \
    pkg-config \
    zlib1g-dev

ENV OCAML_VERSION 4.11.2

WORKDIR ${SOURCE_DIR}

# Make sure vcpkg installs brings in the dependencies
ENV VCPKG_ROOT=/vcpkg
ENV PKG_CONFIG_PATH="${SOURCE_DIR}/vcpkg_installed/x64-linux-dynamic/lib/pkgconfig"

RUN make opamdep-ci \
    && echo '. ~/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true ' >> ~/.bashrc \
    && eval $(opam env) \
    && make

ARG BUILD_DIR="${SOURCE_DIR}/_build/default"
ARG VCPKG_INSTALL_LIB_DIR="${BUILD_DIR}/vcpkg_installed/x64-linux-dynamic/lib"

RUN mkdir -p ${VCPKG_INSTALL_LIB_DIR} \
  && ldd ${BUILD_DIR}/src/runners/*.exe | grep vcpkg_installed | gawk '{print $3}' | xargs -I{} cp {} ${VCPKG_INSTALL_LIB_DIR} \
  && rm -rf vcpkg_installed \
  && ln -s ${BUILD_DIR}/vcpkg_installed vcpkg_installed

FROM zilliqa/zilliqa-base:${ZILLIQA_VERSION}

ARG SOURCE_DIR="/scilla/${MAJOR_VERSION}"

COPY --from=builder ${SOURCE_DIR}       ${SOURCE_DIR}

