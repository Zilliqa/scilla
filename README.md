# Scilla: A Smart Contract Intermediate Level Language

## Project structure

* [`docs`](./docs) -- specification and other documents 
* [`src/lang`](./src/lang) -- language definition
* [`src/runners`](./src/runners) -- interpreters

## Building and Running

### Build requirements

Install the folowing dependencies for OCaml:

* `opam`, package manager for OCaml, version >= 1.2'
* `jbuilder` build tool, can be installed via `opam install jbuilder`
* `ocamlc`, version >= 4.05

The package dependencies can be installed via `opam` as follows:

```
opam install ocaml-migrate-parsetree
opam install core cryptokit ppx_sexp_conv yojson
opam install angstrom hex ppx_deriving ppx_deriving_yojson
opam install menhir
```

### Compiling and Running

To build the project, run `make clean; make` from the root folder.

Once the probject is built you can try the following things:

#### Evaluating a standalone closed expression:

From the project root, execute

```
./bin/eval-runner examples/eval/exp/let.scilla 
```

Instead of `let.scilla` you might want to try any idfferent file in `examples/eval/exp`.

#### Running a step-based simulation

From the project root, execute

```
./bin/scilla-runner crowdfunding n
```
  or
```
./bin/scilla-runner zil-game n
```
where `n` is a number `0-5` for the number of "steps" to execute the
protocol (the messages and blockchain states are provided for only so
many steps in the simulation).

### Where to find binaries

* The runnables are put into the folder

```
$PROJECT_DIR/_build/install/default/bin
```

## Using Ocaml with Emacs

The following extensions would be useful:

* [tuareg](https://github.com/ocaml/tuareg) for syntax highlighting
* [merlin](https://github.com/ocaml/merlin/wiki/emacs-from-scratch) for auto-completion
* [ocp-indent](https://github.com/OCamlPro/ocp-indent) for smart indentation

All thos libraries can be installed via [opem-user-setup](https://github.com/OCamlPro/opam-user-setup):

```
opam install user-setup
```

## Roadmap

Check the working [Notes](./ROADMAP.md)

