## Platform setup for building Scilla

### Ubuntu

Required ubuntu packages can be installed as below:

```
sudo apt-get update
sudo apt-get install -y curl build-essential m4 ocaml opam pkg-config zlib1g-dev libgmp-dev
```

Building Scilla requires OCaml 4.06.1. You can switch to this version and install required
opam packages using the commands listed below:

```
opam init -y
opam switch -y 4.05.0
opam install -y ocaml-migrate-parsetree core cryptokit ppx_sexp_conv yojson batteries angstrom hex ppx_deriving ppx_deriving_yojson menhir oUnit jbuilder
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

### MacOS

TODO
