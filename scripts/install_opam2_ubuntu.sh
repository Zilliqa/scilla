#!/usr/bin/env bash

##  This file is part of scilla.
##
##  Copyright (c) 2018 - present Zilliqa Research Pvt. Ltd.
##  
##  scilla is free software: you can redistribute it and/or modify it under the
##  terms of the GNU General Public License as published by the Free Software
##  Foundation, either version 3 of the License, or (at your option) any later
##  version.
## 
##  scilla is distributed in the hope that it will be useful, but WITHOUT ANY
##  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
##  A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
## 
##  You should have received a copy of the GNU General Public License along with
##  scilla.  If not, see <http://www.gnu.org/licenses/>.

# The -e flag makes sure the script exits as soon as one command returns a non-zero exit code
# The -v flag makes the shell print all lines in the script before executing them, which helps identify which steps failed
set -ev

# install opam 2.0 -- the current Ubuntu versions available on Travis CI are v1.x
# -O option does not exactly work as "save as", but it does the job in this case
wget https://github.com/ocaml/opam/releases/download/2.0.4/opam-2.0.4-x86_64-linux -O opam
sudo mv ./opam /usr/local/bin/opam
sudo chmod a+x /usr/local/bin/opam
