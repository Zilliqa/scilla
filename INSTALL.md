# Building and Developing Scilla

Please, read the instructions below if you intend to hack on Scilla implementation.

## Platform-specific setup for building Scilla

Setup OCaml using the instructions
[here](https://github.com/realworldocaml/book/wiki/Installation-Instructions). Make
sure you have switched (using `opam switch`) to a version not older
than the one specified below.

### Ubuntu

Required ubuntu packages can be installed as below:

```
sudo apt-get update
sudo apt-get install -y curl build-essential m4 ocaml opam pkg-config zlib1g-dev libgmp-dev libffi-dev libssl-dev libboost-system-dev
```

Building Scilla requires OCaml 4.06.1. You can switch to this version and install required
opam packages using the commands listed below:

```
opam init -y
opam switch -y 4.06.1
opam install -y ocaml-migrate-parsetree core cryptokit ppx_sexp_conv yojson batteries angstrom hex ppx_deriving ppx_deriving_yojson menhir oUnit dune stdint fileutils ctypes ctypes-foreign
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

### Mac OS X

The dependencies can be installed via [Homebrew](https://brew.sh/):

```
brew install ocaml opam pkg-config libffi openssl boost
opam init
opam switch -y 4.06.1
opam install angstrom batteries core cryptokit fileutils hex num oUnit ppx_deriving ppx_deriving_yojson ppx_let ppx_sexp_conv stdint yojson menhir dune ctypes ctypes-foreign

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
