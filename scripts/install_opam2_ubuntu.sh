#!/bin/bash

# The -x flag makes sure the script exits as soon as one command returns a non-zero exit code
# The -v flag makes the shell print all lines in the script before executing them, which helps identify which steps failed
set -ev

# ./scripts/build_openssl.sh

# install opam 2.0 -- the current Ubuntu versions available on Travis CI are v1.x
# -O option does not exactly work as "save as", but it does the job in this case
wget https://github.com/ocaml/opam/releases/download/2.0.4/opam-2.0.4-x86_64-linux -O opam
sudo mv ./opam /usr/local/bin/opam
sudo chmod a+x /usr/local/bin/opam
