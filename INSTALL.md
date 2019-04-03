# Building and Developing Scilla

Please, read the instructions below if you intend to hack on Scilla implementation.

## Platform-specific setup for building Scilla

Setup OCaml using the instructions
[here](https://github.com/realworldocaml/book/wiki/Installation-Instructions). Make
sure you have switched (using `opam switch`) to a version not older
than the one specified below.

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

    sudo zypper install -y curl m4 opam2 pkg-config zlib-devel gmp-devel libffi-devel libopenssl-devel boost-devel

### Ubuntu

On machines with Ubuntu strictly older than 18.04, run these additional commands first:

```shell
# Add Ubuntu PPA for libsecp256k1-dev
sudo add-apt-repository ppa:tah83/secp256k1 -y
```

Required ubuntu packages can be installed as below:

```shell
sudo add-apt-repository -y ppa:avsm/ppa
sudo apt-get update
sudo apt-get install -y curl build-essential m4 ocaml opam pkg-config zlib1g-dev libgmp-dev libffi-dev libssl-dev libboost-system-dev libsecp256k1-dev libpcre3-dev
```

### OCaml toolchain

Building Scilla requires OCaml 4.06.1. You can switch to this version and install required
opam packages using the commands listed below:

```shell
opam init --disable-sandboxing -y --compiler=4.06.1
opam install ocaml-migrate-parsetree core cryptokit ppx_sexp_conv yojson batteries angstrom hex ppx_deriving menhir oUnit dune stdint fileutils ctypes ctypes-foreign bisect_ppx secp256k1 patdiff
```

The above three commands can, alternatively, be run using the make target `opamdep`

```shell
make opamdep
```

Finally, opam environment needs to be set in your shell. This can be done as:

```shell
echo ". ~/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true " >> ~/.bashrc
```

Note that the command above does not setup the environment for the current shell. Either
quit and start a new shell (so that ~/.bashrc is invoked) OR, run the following command 
to setup the environment for your current shell.

```shell
eval `opam config env`
```

Scilla requires OpenSSL 1.0.2 and if your platform does not have packages for this, you may need to build OpenSSL
yourself and set `PKG_CONFIG_PATH` environment variable accordingly
(if you install OpenSSL in a non-default path):
```shell
export PKG_CONFIG_PATH="_OpenSSL_prefix_/lib/pkgconfig:$PKG_CONFIG_PATH"
```

### macOS

The dependencies can be installed via [Homebrew](https://brew.sh/).

To install the compatible version of `secp256k1` package, one needs to add the following tap:
```shell
brew tap iantanwx/crypto
```
(side note: pending PR at https://github.com/DomT4/homebrew-crypto/pull/95/commits/9c62017362aa973afad75616046d14006f31be6a)

and proceed with OS-level package installation:
```shell
brew install gcc ocaml opam pkg-config libffi openssl boost pcre iantanwx/crypto/secp256k1
```

Now we can set up opam dependencies:
```shell
opam init --disable-sandboxing -y --compiler=4.06.1
opam install ocaml-migrate-parsetree core cryptokit ppx_sexp_conv yojson batteries angstrom hex ppx_deriving menhir oUnit dune stdint fileutils ctypes ctypes-foreign bisect_ppx secp256k1 patdiff
```

Then run the following command to setup environment on current shell. 
```shell
eval `opam config env`
```

Normally, by this moment everything should be set up as the [Dune](https://dune.build) build system
takes care of environment variables for `pkg-config` utility.
However Homebrew's `openssl` package is keg-only, which means it doesn't get symlinked
into `/usr/local` directory, so in case of a non-default version of the package,
you will need to set up `PKG_CONFIG_PATH` environment variable as Homebrew suggests.
It should look like
```shell
export PKG_CONFIG_PATH="/usr/local/opt/openssl@_Version_/lib/pkgconfig:$PKG_CONFIG_PATH""
```

To run tests with `make test` you might need to increase the maximum number of open file descriptors:
```shell
ulimit -n 1024
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
