#!/usr/bin/env bash

wget https://github.com/Kitware/CMake/releases/download/v3.24.2/cmake-3.24.2-Linux-x86_64.sh
mkdir -p "${HOME}"/.local
bash ./cmake-3.24.2-Linux-x86_64.sh --skip-license --prefix="${HOME}"/.local/
