# Building and Developing Scilla

Please, read the instructions below if you intend to hack on Scilla implementation.
Setup OCaml using the instructions [here](https://github.com/realworldocaml/book/wiki/Installation-Instructions).
Please make sure you install opam v2.0 or greater (this can be done by running `opam --version`)

Scilla requires OpenSSL 1.0.2 and if your platform does not have packages for this, you may need to build OpenSSL
yourself and set `PKG_CONFIG_PATH` environment variable accordingly
(if you install OpenSSL in a non-default path):
```shell
export PKG_CONFIG_PATH="_OpenSSL_prefix_/lib/pkgconfig:$PKG_CONFIG_PATH"
```


## OS-specific setup for building Scilla

### openSUSE

- Install `libsecp256k1-devel` from
  https://software.opensuse.org/package/libsecp256k1-devel by clicking
  on the `Show experimental packages` button and then performing the
  1-click install from the `network:cryptocurrencies` project.

- Install `bubblewrap` from
  https://software.opensuse.org/package/bubblewrap by clicking on the
  `Show experimental packages` button and then performing the 1-click
  install from the `Virtualization:containers` project.

- Install `ocaml` from https://software.opensuse.org/package/ocaml by
  performing the 1-click install of the officially released package.

- Run:
```shell
sudo zypper install -y curl m4 opam2 pkg-config zlib-devel gmp-devel libffi-devel libopenssl-devel boost-devel
```

### Ubuntu

On machines with Ubuntu strictly older than 18.04, run these additional commands first:

```shell
# Add Ubuntu PPA for libsecp256k1-dev
sudo add-apt-repository ppa:tah83/secp256k1 -y
```

Required Ubuntu packages can be installed as below:

```shell
sudo add-apt-repository -y ppa:avsm/ppa
sudo apt-get update
sudo apt-get install -y curl build-essential m4 ocaml opam pkg-config zlib1g-dev libgmp-dev libffi-dev libssl-dev libboost-system-dev libsecp256k1-dev libpcre3-dev
```

### macOS

The dependencies (listed in [Brewfile](Brewfile)) can be installed via [Homebrew](https://brew.sh/) as follows.
Run 
```shell
brew bundle
```
from the project root.

Normally, by this moment everything should be set up as the [Dune](https://dune.build) build system
takes care of environment variables for `pkg-config` utility.
However Homebrew's `openssl` package is keg-only, which means it doesn't get symlinked
into `/usr/local` directory, so in case of a non-default version of the package,
you will need to set up `PKG_CONFIG_PATH` environment variable as Homebrew suggests.
It should look like
```shell
export PKG_CONFIG_PATH="/usr/local/opt/openssl@_Version_/lib/pkgconfig:$PKG_CONFIG_PATH"
```

To run tests using Dune (`dune exec tests/testsuite.exe`), you may need to increase
the maximum number of open file descriptors as `Makefile`'s `test` target does:
```shell
ulimit -n 1024
```


## Installing opam packages

### If you just installed opam package manager

#### Initialize opam
```shell
opam init --disable-sandboxing --compiler=4.06.1 --yes
```
Note: the initializer will change your shell configuration to setup the environment opam needs to work.
You can remove `--yes` from the above command to manually control that process.

#### Setup your current shell to work with opam
```shell
eval $(opam env)
```

#### Install Scilla dependencies using opam
```shell
cd PROJECT_DIR    # go inside your Scilla project directory
opam install ./scilla.opam --deps-only --with-test
```
The above commands can, alternatively, be run using the make target `opamdep`:
```shell
make opamdep
```

### If you have opam package manager already installed
You can try installing the Scilla dependencies using the instructions above, but skipping the initialization step.
If `opam` reports a dependency conflict, one way out might be creating yet another opam switch and 
managing your switches when doing Scilla- and non-Scilla- related hacking.
Another way is to use opam's feature called "local switch".
This is like a standard opam switch but instead of `$HOME/.opam`, it will reside in the project root directory in `_opam` subdirectory.
This lets us to avoid dependency conflict and changing our switches back and forth when working on different projects.
To create a local opam switch and install all the Scilla dependencies, `cd` into project root and execute:
```shell
opam switch create . --deps-only --with-test --yes
```
Now, whenever you are inside the project directory, opam will prefer the local switch to any globally installed switches,
unless being told explicitly which one to use.

Note: using `git clean`, extra care should be paid so that it won't delete `_opam` directory.
We suggest using `make clean` command or keeping `_opam` directory like so:
```shell
git clean -dfX --exclude=\!_opam/**
```


## Using Ocaml with Emacs

As Scilla is written in [OCaml](https://ocaml.org/), the following extensions would be
useful for working on this codebase:

* [tuareg](https://github.com/ocaml/tuareg) for syntax highlighting
* [merlin](https://github.com/ocaml/merlin/wiki/emacs-from-scratch) for auto-completion
* [ocp-indent](https://github.com/OCamlPro/ocp-indent) for smart indentation

All those libraries can be installed via [opem-user-setup](https://github.com/OCamlPro/opam-user-setup):

```shell
opam install user-setup
```

To enable flycheck mode (integration of `scilla-checker` with Emacs for editing Scilla files), install
flycheck for Emacs. See installation instructions [here](http://www.flycheck.org/en/latest/user/installation.html).
