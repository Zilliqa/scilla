#!/bin/bash

# sudo is only required for below two installs.
sudo apt-get update
sudo apt-get install -y curl build-essential m4 ocaml opam pkg-config zlib1g-dev libgmp-dev

opam init -y
opam switch -y 4.05.0
opam install -y ocaml-migrate-parsetree core cryptokit ppx_sexp_conv yojson batteries angstrom hex ppx_deriving ppx_deriving_yojson menhir oUnit jbuilder
echo ". ~/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true " >> ~/.bashrc

echo "Environment for building Scilla setup successfully. Please exit this shell and start a new one before building scilla."
