# Building and Developing Scilla

Please, read the instructions below if you intend to hack on Scilla implementation.

## Platform-specific setup for building Scilla

Setup OCaml using the instructions
[here](https://github.com/realworldocaml/book/wiki/Installation-Instructions). Make
sure you have switched (using `opam switch`) to a version not older
than the one specified below.

### Ubuntu

On machines older than Ubuntu 18.04, run these additional commands first: The last three lines must also be added to your `~/.bashrc`.

```
# Add Ubuntu PPA for libsecp256k1-dev
sudo add-apt-repository ppa:tah83/secp256k1 -y
# Fetch, build and install OpenSSL 1.1.1 into ${HOME}/openssl.
./scripts/build_openssl.sh
# Exports for using OpenSSL 1.1.1 built above, instead of system OpenSSL.
export CPLUS_INCLUDE_PATH="${HOME}/openssl/install/include:${CPLUS_INCLUDE_PATH}"
export LIBRARY_PATH="${HOME}/openssl/install/lib:${LIBRARY_PATH}"
export LD_LIBRARY_PATH="${HOME}/openssl/install/lib:${LD_LIBRARY_PATH}"
```

Required ubuntu packages can be installed as below:

```
sudo add-apt-repository -y ppa:avsm/ppa
sudo apt-get update
sudo apt-get install -y curl build-essential m4 ocaml opam pkg-config zlib1g-dev libgmp-dev libffi-dev libssl-dev libboost-system-dev libsecp256k1-dev
```

Building Scilla requires OCaml 4.06.1. You can switch to this version and install required
opam packages using the commands listed below:

```
opam init -y
opam switch -y 4.06.1
opam install -y ocaml-migrate-parsetree core cryptokit ppx_sexp_conv yojson batteries angstrom hex ppx_deriving ppx_deriving_yojson menhir oUnit dune stdint fileutils ctypes ctypes-foreign bisect_ppx secp256k1
```

The above three commands can, alternatively, be run using the make target `opamdep`

```
make opamdep
```

Finally, opam environment needs to be set in your shell. This can be done as:

```
echo ". ~/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true " >> ~/.bashrc
```

Note that the command above does not setup the environment for the current shell. Either
quit and start a new shell (so that ~/.bashrc is invoked) OR, run the following command 
to setup the environment for your current shell.

```
eval `opam config env`
```

Scilla requires OpenSSL 1.1+ and if your platform does not have packages for this, you may need to build OpenSSL
yourself and set $CPLUS_INCLUDE_PATH, $LIBRARY_PATH and $LD_LIBRARY_PATH accordingly (if you install OpenSSL in
a non-default path).

### Mac OS X

The dependencies can be installed via [Homebrew](https://brew.sh/):

Note: pending PR at https://github.com/DomT4/homebrew-crypto/pull/95/commits/9c62017362aa973afad75616046d14006f31be6a
```
brew tap iantanwx/crypto
```

```
brew install ocaml opam pkg-config libffi openssl@1.1 boost secp256k1
opam init
opam switch -y 4.06.1
opam install angstrom batteries core cryptokit fileutils hex num oUnit ppx_deriving ppx_deriving_yojson ppx_let ppx_sexp_conv stdint yojson menhir dune ctypes ctypes-foreign bisect_ppx secp256k1

```
Then run the following command to setup environment on current shell. 
```
eval `opam config env`
```

## Using Ocaml with Emacs

As Scilla is written in [OCaml](https://ocaml.org/), the following extensions would be
useful for working on this codebase:

* [tuareg](https://github.com/ocaml/tuareg) for syntax highlighting
* [merlin](https://github.com/ocaml/merlin/wiki/emacs-from-scratch) for auto-completion
* [ocp-indent](https://github.com/OCamlPro/ocp-indent) for smart indentation

All those libraries can be installed via [opem-user-setup](https://github.com/OCamlPro/opam-user-setup):

```
opam install user-setup
```

To enable flycheck mode (integration of `scilla-checker` with Emacs for editing Scilla files), install
flycheck for Emacs. See installation instructions [here](http://www.flycheck.org/en/latest/user/installation.html).
