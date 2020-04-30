#!/usr/bin/env bash

# The -x flag makes sure the script exits as soon as one command returns a non-zero exit code
# The -v flag makes the shell print all lines in the script before executing them, which helps identify which steps failed
set -ev

# The shellcheck package is already available in Travis CI, but this pins its version to avoid
# having unrelated build failures when new warnings are added to shellcheck in the future and Travis updates the package
scversion="v0.7.1"
wget -qO- "https://github.com/koalaman/shellcheck/releases/download/${scversion?}/shellcheck-${scversion?}.linux.x86_64.tar.xz" | tar -xJv
sudo mv "shellcheck-${scversion}/shellcheck" /usr/local/bin/
rm -rf "shellcheck-${scversion}/shellcheck"
shellcheck --version
